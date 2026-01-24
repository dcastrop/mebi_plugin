(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

module Context = struct
  type t =
    { env : Environ.env
    ; sigma : Evd.evar_map
    }

  module type TYPE = sig
    val get : unit -> t ref
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
  end

  module type S = sig
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
  end

  module Make (X : S) : TYPE = struct
    let get () : t ref = ref { env = !(X.env ()); sigma = !(X.sigma ()) }
    let env () : Environ.env ref = ref !(get ()).env
    let sigma () : Evd.evar_map ref = ref !(get ()).sigma
  end

  module Default : TYPE = Make (struct
      let env () : Environ.env ref = ref (Global.env ())
      let sigma () : Evd.evar_map ref = ref (Evd.from_env !(env ()))
    end)
end

(***********************************************************************)

module MakeEConstrMap (X : Context.TYPE) : Hashtbl.S with type key = EConstr.t =
Hashtbl.Make (struct
    type t = EConstr.t

    let equal (a : t) (b : t) : bool = EConstr.eq_constr !(X.sigma ()) a b

    let hash (x : t) : int =
      Constr.hash
        (EConstr.to_constr ~abort_on_undefined_evars:false !(X.sigma ()) x)
    ;;
  end)

(***********************************************************************)

module Encoding = struct
  module type TYPE = sig
    type t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val counter : t ref
    val reset : unit -> unit
    val incr : unit -> t
  end

  module type S = sig
    type t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
  end

  module Make (X : S) : TYPE = struct
    type t = X.t

    let init = X.init
    let next = X.next
    let equal = X.equal
    let compare = X.compare
    let hash = X.hash
    let to_string = X.to_string

    (* *)
    let counter : t ref = ref init
    let reset () : unit = counter := init

    let incr () : t =
      let x : t = !counter in
      counter := X.next !counter;
      x
    ;;
  end

  module Int : TYPE = Make (struct
      include Int

      type t = int

      let init : t = 0
      let next : t -> t = fun x -> x + 1
      let to_string : t -> string = Printf.sprintf "%i"
    end)
end

(***********************************************************************)

module Constructor = struct
  module Tree = struct
    module type TYPE = sig
      module Node : sig
        type t

        val to_string : t -> string
      end

      type 'a tree = Node of 'a * 'a tree list
      type t

      val add : t -> t -> t
      val add_list : t -> t list -> t list
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val minimize : t -> Node.t list

      exception Mebi_constr_Tree_EmptyList of unit

      val min : t list -> Node.t list
      val to_string : t -> string
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Make (Enc : Encoding.TYPE) : TYPE = struct
      module Node = struct
        type t = Enc.t * int

        let to_string ((lts, index) : t) : string =
          Utils.Strfy.record
            [ "lts (enc)", Enc.to_string lts; "index", Utils.Strfy.int index ]
        ;;
      end

      type 'a tree = Node of 'a * 'a tree list
      type t = Node.t tree

      (** [add x y] inserts [x] to be a new leaf of [y], mutually recursive with [add_list x ys] (where [ys] is a list of [t]).
      *)
      let rec add (x : t) : t -> t = function
        | Node ((enc, index), tl) -> Node ((enc, index), add_list x tl)

      and add_list (x : t) : t list -> t list = function
        | [] -> [ x ]
        | h :: tl -> add x h :: add_list x tl
      ;;

      let rec equal (t1 : t) (t2 : t) : bool =
        match t1, t2 with
        | Node (a1, b1), Node (a2, b2) ->
          fst a1 == fst a2 && snd a1 == snd a2 && List.equal equal b1 b2
      ;;

      let compare (t1 : t) (t2 : t) : int =
        match t1, t2 with
        | Node (i1, l1), Node (i2, l2) ->
          Utils.compare_chain
            [ Enc.compare (fst i1) (fst i2)
            ; Int.compare (snd i1) (snd i2)
            ; List.compare compare l1 l2
            ]
      ;;

      (** converts a given tree into a flattened list with the minimal number of constructors to apply
      *)
      let rec minimize : t -> Node.t list = function
        | Node ((enc, index), []) -> [ enc, index ]
        | Node ((enc, index), h :: tl) ->
          (enc, index)
          :: (List.fold_left (fun acc x -> minimize x :: acc) [] tl
              |> List.fold_left
                   (fun the_min x ->
                     match
                       Int.compare (List.length x) (List.length the_min)
                     with
                     | -1 -> x
                     | _ -> the_min)
                   (minimize h))
      ;;

      exception Mebi_constr_Tree_EmptyList of unit

      let min : t list -> Node.t list = function
        | [] -> raise (Mebi_constr_Tree_EmptyList ())
        | h :: tl ->
          List.map minimize tl
          |> List.fold_left
               (fun the_min y ->
                 match Int.compare (List.length y) (List.length the_min) with
                 | -1 -> y
                 | _ -> the_min)
               (minimize h)
      ;;

      let rec to_string (Node (node, nodes) : t) : string =
        Utils.Strfy.tuple
          (Of Node.to_string)
          (Args (Utils.Strfy.list (Of to_string)))
          (node, nodes)
      ;;

      let list_to_string
            ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        : t list -> string
        =
        Utils.Strfy.list
          ~args:{ args with name = Some "Constructor Trees" }
          (Of to_string)
      ;;
    end
  end

  module Triple = struct
    module type TYPE = sig
      module Tree : Tree.TYPE

      type t = Evd.econstr * Evd.econstr * Tree.t

      val to_string
        :  Environ.env
        -> Evd.evar_map
        -> ?args:Utils.Strfy.style_args
        -> t
        -> string
    end

    module Make (Enc : Encoding.TYPE) = struct
      module Tree = Tree.Make (Enc)

      (** A triple denoting a constructor of an rocq LTS definition.
          - [EConstr.t] action
          - [EConstr.t] destination
          - [Mebi_constr.Tree.t] coq-constructor index *)
      type t = EConstr.t * EConstr.t * Tree.t

      let to_string
            env
            sigma
            ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
            ((action, destination, tree) : t)
        : string
        =
        let open Rocq_utils.Strfy in
        let action : string = econstr env sigma action in
        let destination : string = econstr env sigma destination in
        let tree : string = Tree.to_string tree in
        Utils.Strfy.record
          [ "action", action; "destination", destination; "tree", tree ]
      ;;
    end
  end

  module type TYPE = sig
    module Tree : Tree.TYPE
    module Triple : Triple.TYPE
  end

  module Make (Enc : Encoding.TYPE) : TYPE = struct
    module Tree = Tree.Make (Enc)
    module Triple = Triple.Make (Enc)
  end
end

(***********************************************************************)

module BiEncoding = struct
  module type TYPE = sig
    module Enc : Encoding.TYPE
    module FwdMap : Hashtbl.S with type key = EConstr.t
    module BckMap : Hashtbl.S with type key = Enc.t

    type fwdmap = Enc.t FwdMap.t
    type bckmap = EConstr.t BckMap.t

    type maps =
      { fwd : fwdmap
      ; bck : bckmap
      }

    val the_maps : unit -> maps ref
    val reset : unit -> unit

    (* *)
    val encode : EConstr.t -> Enc.t
    val encoded : EConstr.t -> bool
    val decode : Enc.t -> EConstr.t
    val decode_opt : Enc.t -> EConstr.t option
    val encode_map : 'a FwdMap.t -> 'a BckMap.t
    val decode_map : 'a BckMap.t -> 'a FwdMap.t
    val to_list : unit -> (Enc.t * EConstr.t) list

    (* *)

    val make_hashtbl : (module Hashtbl.S with type key = Enc.t)
    val make_set : (module Set.S with type elt = Enc.t)
  end

  module Make (Ctx : Context.TYPE) (Enc : Encoding.TYPE) : TYPE = struct
    module Enc : Encoding.TYPE = Enc
    module FwdMap : Hashtbl.S with type key = EConstr.t = MakeEConstrMap (Ctx)
    module BckMap : Hashtbl.S with type key = Enc.t = Hashtbl.Make (Enc)

    type fwdmap = Enc.t FwdMap.t
    type bckmap = EConstr.t BckMap.t

    type maps =
      { fwd : fwdmap
      ; bck : bckmap
      }

    let the_maps () : maps ref =
      ref { fwd = FwdMap.create 0; bck = BckMap.create 0 }
    ;;

    let reset () : unit =
      Enc.reset ();
      the_maps () := { fwd = FwdMap.create 0; bck = BckMap.create 0 }
    ;;

    let fwdmap () : Enc.t FwdMap.t = !(the_maps ()).fwd
    let bckmap () : EConstr.t BckMap.t = !(the_maps ()).bck

    module F = FwdMap
    module B = BckMap

    (* *)
    let get_encoding : EConstr.t -> Enc.t = F.find (fwdmap ())

    let encode (x : EConstr.t) : Enc.t =
      try get_encoding x with
      | Not_found ->
        (* NOTE: map to the next encoding and return *)
        let next_enc : Enc.t = Enc.incr () in
        F.add (fwdmap ()) x next_enc;
        B.add (bckmap ()) next_enc x;
        next_enc
    ;;

    let encoded (x : EConstr.t) : bool = F.mem (fwdmap ()) x

    (* *)
    let get_econstr : Enc.t -> EConstr.t = B.find (bckmap ())

    exception InvalidEncoding of Enc.t

    let decode (x : Enc.t) : EConstr.t =
      try get_econstr x with Not_found -> raise (InvalidEncoding x)
    ;;

    let decode_opt (x : Enc.t) : EConstr.t option =
      try Some (decode x) with InvalidEncoding _ -> None
    ;;

    (* *)
    let decode_map (bmap : 'a B.t) : 'a F.t =
      let fmap : 'a F.t = F.create (B.length bmap) in
      B.iter (fun (k : Enc.t) (v : 'a) -> F.add fmap (decode k) v) bmap;
      fmap
    ;;

    let encode_map (fmap : 'a F.t) : 'a B.t =
      let bmap : 'a B.t = B.create (F.length fmap) in
      F.iter (fun (k : EConstr.t) (v : 'a) -> B.add bmap (encode k) v) fmap;
      bmap
    ;;

    (* *)
    let to_list () : (Enc.t * EConstr.t) list =
      B.to_seq (bckmap ())
      |> List.of_seq
      |> List.sort (fun (a, _) (b, _) -> Enc.compare a b)
    ;;

    (* *)
    let make_hashtbl : (module Hashtbl.S with type key = Enc.t) =
      (module Hashtbl.Make (struct
           include Enc
         end))
    ;;

    let make_set : (module Set.S with type elt = Enc.t) =
      (module Set.Make (struct
           include Enc
         end))
    ;;
  end
end

(***********************************************************************)

module Errors = struct
  module type TYPE = sig
    type t =
      | Invalid_Sort_LTS of Sorts.family
      | Invalid_Sort_Type of Sorts.family

    exception MEBI_exn of t

    val invalid_sort_lts : Sorts.family -> exn
    val invalid_sort_type : Sorts.family -> exn
  end

  module Make (Ctx : Context.TYPE) : TYPE = struct
    type t =
      | Invalid_Sort_LTS of Sorts.family
      | Invalid_Sort_Type of Sorts.family

    exception MEBI_exn of t

    let invalid_sort_lts x = MEBI_exn (Invalid_Sort_LTS x)
    let invalid_sort_type x = MEBI_exn (Invalid_Sort_Type x)

    let mebi_handler : t -> string = function
      | Invalid_Sort_LTS x -> "Invalid_Sort_LTS"
      | Invalid_Sort_Type x -> "Invalid_Sort_Type"
    ;;

    let _ =
      CErrors.register_handler (fun e ->
        match e with MEBI_exn e -> Some (Pp.str (mebi_handler e)) | _ -> None)
    ;;
  end
end

(***********************************************************************)

module Monad = struct
  module type TYPE = sig
    module Enc : Encoding.TYPE
    module Constructor : Constructor.TYPE
    module FwdMap : Hashtbl.S with type key = EConstr.t
    module BckMap : Hashtbl.S with type key = Enc.t
    module Error : Errors.TYPE

    type fwdmap = Enc.t FwdMap.t
    type bckmap = EConstr.t BckMap.t

    type maps =
      { fwd : fwdmap
      ; bck : bckmap
      }

    val encode : EConstr.t -> Enc.t
    val encoded : EConstr.t -> bool
    val decode : Enc.t -> EConstr.t
    val decode_opt : Enc.t -> EConstr.t option
    val to_list : unit -> (Enc.t * EConstr.t) list

    type 'a mm = wrapper ref -> 'a in_wrapper

    and wrapper =
      { ctx : Context.t ref
      ; maps : maps ref
      }

    and 'a in_wrapper =
      { state : wrapper ref
      ; value : 'a
      }

    val run : ?reset_encoding:bool -> 'a mm -> 'a
    val return : 'a -> 'a mm
    val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
    val map : ('a -> 'b) -> 'a mm -> 'b mm
    val product : 'a mm -> 'b mm -> ('a * 'b) mm
    val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

    val state
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> wrapper ref
      -> 'a in_wrapper

    val sandbox : ?sigma:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_wrapper
    val get_env : wrapper ref -> Environ.env in_wrapper
    val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
    val get_fwdmap : wrapper ref -> fwdmap in_wrapper
    val get_bckmap : wrapper ref -> bckmap in_wrapper

    module type SYNTAX = sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( let$* )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
        -> (unit -> 'b mm)
        -> 'b mm

      val ( let$+ )
        :  (Environ.env -> Evd.evar_map -> 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    module Syntax : SYNTAX

    val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string

    (* for graph *)
    val make_state_tree_pair_set
      : (module Set.S with type elt = Enc.t * Constructor.Tree.t)
  end

  module Make (Ctx : Context.TYPE) (Enc : Encoding.TYPE) : TYPE = struct
    module BiEnc : BiEncoding.TYPE = BiEncoding.Make (Ctx) (Enc)
    include BiEnc
    module Error : Errors.TYPE = Errors.Make (Ctx)
    module Constructor : Constructor.TYPE = Constructor.Make (Enc)

    type 'a mm = wrapper ref -> 'a in_wrapper

    and wrapper =
      { ctx : Context.t ref
      ; maps : BiEnc.maps ref
      }

    and 'a in_wrapper =
      { state : wrapper ref
      ; value : 'a
      }

    (* *)
    let run ?(reset_encoding : bool = false) (x : 'a mm) : 'a =
      BiEnc.reset ();
      let a : 'a in_wrapper =
        x (ref { ctx = Ctx.get (); maps = BiEnc.the_maps () })
      in
      a.value
    ;;

    let return (x : 'a) : 'a mm =
      fun (st : wrapper ref) -> { state = st; value = x }
    [@@inline always]
    ;;

    let bind (x : 'a mm) (f : 'a -> 'b mm) : 'b mm =
      fun (st : wrapper ref) ->
      let a : 'a in_wrapper = x st in
      f a.value a.state
    [@@inline always]
    ;;

    let map (f : 'a -> 'b) (x : 'a mm) : 'b mm =
      fun (st : wrapper ref) ->
      let x_st : 'a in_wrapper = x st in
      { x_st with value = f x_st.value }
    [@@inline always]
    ;;

    let product (x : 'a mm) (y : 'b mm) : ('a * 'b) mm =
      bind x (fun a -> bind y (fun b -> return (a, b)))
    [@@inline always]
    ;;

    (** Monadic for loop *)
    let rec iterate
              (index : int)
              (upper_bound : int)
              (acc : 'a)
              (f : int -> 'a -> 'a mm)
      : 'a mm
      =
      if index > upper_bound
      then return acc
      else
        bind (f index acc) (fun acc' -> iterate (index + 1) upper_bound acc' f)
    ;;

    (** [state f] provides the [env] and [sigma] for [f] and returns the result.
    *)
    let state
          (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
          (st : wrapper ref)
      : 'a in_wrapper
      =
      let sigma, a = f !(!st.ctx).env !(!st.ctx).sigma in
      st := { !st with ctx = ref { !(!st.ctx) with sigma } };
      { state = st; value = a }
    ;;

    (** [sandbox ?sigma m] evaluates [m] without updating the state of the monad.
        @param ?sigma
          allows a specific sigma to be used (instead of the one from the state).
    *)
    let sandbox ?(sigma : Evd.evar_map option) (m : 'a mm) (st : wrapper ref)
      : 'a in_wrapper
      =
      let st_copy : wrapper = !st in
      let st =
        Option.cata
          (fun (sigma : Evd.evar_map) ->
            ref { !st with ctx = ref { !(!st.ctx) with sigma } })
          st
          sigma
      in
      let result : 'a in_wrapper = m st in
      { state = ref st_copy; value = result.value }
    ;;

    module type SYNTAX = sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( let$* )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
        -> (unit -> 'b mm)
        -> 'b mm

      val ( let$+ )
        :  (Environ.env -> Evd.evar_map -> 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    module Syntax : SYNTAX = struct
      let ( let+ ) x f = map f x
      let ( let* ) = bind
      let ( let$ ) f g = bind (state f) g
      let ( let$* ) f g = bind (state (fun e s -> f e s, ())) g
      let ( let$+ ) f g = bind (state (fun e s -> s, f e s)) g
      let ( and+ ) x y = product x y
    end

    (* *)
    let get_ctx (st : wrapper ref) : Context.t in_wrapper =
      { state = st; value = !(!st.ctx) }
    ;;

    let get_env (st : wrapper ref) : Environ.env in_wrapper =
      { state = st; value = !(!st.ctx).env }
    ;;

    let get_sigma (st : wrapper ref) : Evd.evar_map in_wrapper =
      { state = st; value = !(!st.ctx).sigma }
    ;;

    (* *)
    let get_maps (st : wrapper ref) : BiEnc.maps in_wrapper =
      { state = st; value = !(!st.maps) }
    ;;

    let get_fwdmap (st : wrapper ref) : fwdmap in_wrapper =
      { state = st; value = !(!st.maps).fwd }
    ;;

    let get_bckmap (st : wrapper ref) : bckmap in_wrapper =
      { state = st; value = !(!st.maps).bck }
    ;;

    (* *)
    let fstring (f : Environ.env -> Evd.evar_map -> 'a -> string) : 'a -> string
      =
      run
        (let open Syntax in
         let* env = get_env in
         let* sigma = get_sigma in
         return (f env sigma))
    ;;

    (********************************************)
    (****** GRAPH *******************************)
    (********************************************)

    let make_state_tree_pair_set
      : (module Set.S with type elt = Enc.t * Constructor.Tree.t)
      =
      (module Set.Make (struct
           type t = Enc.t * Constructor.Tree.t

           let compare t1 t2 =
             Utils.compare_chain
               [ Enc.compare (fst t1) (fst t2)
               ; Constructor.Tree.compare (snd t1) (snd t2)
               ]
           ;;
         end))
    ;;
  end
end

(***********************************************************************)

module Term = struct
  module type TYPE = sig
    module Enc : Encoding.TYPE

    type t =
      { enc : Enc.t
      ; pp : string option
      }
  end

  module Make (Enc : Encoding.TYPE) : TYPE = struct
    module Enc = Enc

    type t =
      { enc : Enc.t
      ; pp : string option
      }

    let equal (a : t) (b : t) = Enc.equal a.enc b.enc
    let compare (a : t) (b : t) = Enc.compare a.enc b.enc
    let hash (x : t) : int = Enc.hash x.enc

    let to_string : t -> string = function
      | { enc; pp = None } -> Utils.Strfy.record [ "enc", Enc.to_string enc ]
      | { enc; pp = Some pp } ->
        Utils.Strfy.record [ "enc", Enc.to_string enc; "pp", pp ]
    ;;
  end
end

(***********************************************************************)

module Model = struct
  module Label = struct
    module type TYPE = sig end

    module Make (Enc : Encoding.TYPE) : TYPE = struct end
  end

  module Transition = struct
    module Annotation = struct
      module type TYPE = sig end

      module Make (Enc : Encoding.TYPE) : TYPE = struct end
    end

    module type TYPE = sig end

    module Make (Enc : Encoding.TYPE) : TYPE = struct end
  end

  module Action = struct
    module type TYPE = sig end

    module Make (Enc : Encoding.TYPE) : TYPE = struct end
  end

  module type TYPE = sig end
  module type S = sig end

  module Make (X : S) : TYPE = struct end
end

(***********************************************************************)

module Wrapper = struct
  module type TYPE = sig
    module M : Monad.TYPE
  end

  module Make (Ctx : Context.TYPE) (Enc : Encoding.TYPE) : TYPE = struct
    module M = Monad.Make (Ctx) (Enc)
  end
end

(***********************************************************************)

(* module _ = struct

   module type TYPE = sig

   end

   module type S = sig

   end

   module Make (X:S) : TYPE = struct

   end

   end *)

(***********************************************************************)
