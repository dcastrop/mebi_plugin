(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

module XUtils = Utils

module EncTree = struct
  module type S = sig
    module Enc : Encoding.SEncoding

    module type STreeNode = sig
      type t = Enc.t * int

      val to_string : t -> string
    end

    module TreeNode : STreeNode with type t = Enc.t * int

    (* module type STree = sig *)
    type 'a tree = Node of 'a * 'a tree list
    type t = TreeNode.t tree

    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> TreeNode.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> TreeNode.t list
    val to_string : t -> string
    val list_to_string : ?args:XUtils.Strfy.style_args -> t list -> string
    (* end

       module Tree : STree *)
  end

  module Make (E : Encoding.SEncoding) : S with type Enc.t = E.t = struct
    module Enc : Encoding.SEncoding with type t = E.t = E

    module type STreeNode = sig
      type t = Enc.t * int

      val to_string : t -> string
    end

    module TreeNode : STreeNode with type t = Enc.t * int = struct
      type t = Enc.t * int

      let to_string ((enc, index) : t) : string =
        XUtils.Strfy.record
          [ "enc", Enc.to_string enc; "index", XUtils.Strfy.int index ]
      ;;
    end

    (* module type STree = sig
       type 'a tree = Node of 'a * 'a tree list
       type t = TreeNode.t tree

       val add : t -> t -> t
       val add_list : t -> t list -> t list
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val minimize : t -> TreeNode.t list

       exception CannotMinimizeEmptyList of unit

       val min : t list -> TreeNode.t list
       val to_string : t -> string
       val list_to_string : ?args:XUtils.Strfy.style_args -> t list -> string
       end

       module Tree : STree = struct *)
    type 'a tree = Node of 'a * 'a tree list
    type t = TreeNode.t tree

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
        XUtils.compare_chain
          [ Enc.compare (fst i1) (fst i2)
          ; Int.compare (snd i1) (snd i2)
          ; List.compare compare l1 l2
          ]
    ;;

    (** converts a given tree into a flattened list with the minimal number of constructors to apply
    *)
    let rec minimize : t -> TreeNode.t list = function
      | Node ((enc, index), []) -> [ enc, index ]
      | Node ((enc, index), h :: tl) ->
        (enc, index)
        :: (List.fold_left (fun acc x -> minimize x :: acc) [] tl
            |> List.fold_left
                 (fun the_min x ->
                   match Int.compare (List.length x) (List.length the_min) with
                   | -1 -> x
                   | _ -> the_min)
                 (minimize h))
    ;;

    exception CannotMinimizeEmptyList of unit

    let min : t list -> TreeNode.t list = function
      | [] -> raise (CannotMinimizeEmptyList ())
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
      XUtils.Strfy.tuple
        (Of TreeNode.to_string)
        (Args (XUtils.Strfy.list (Of to_string)))
        (node, nodes)
    ;;

    let list_to_string
          ?(args : XUtils.Strfy.style_args = XUtils.Strfy.style_args ())
      : t list -> string
      =
      XUtils.Strfy.list
        ~args:{ args with name = Some "Constructor Trees" }
        (Of to_string)
    ;;
    (* end *)
  end
end

(***********************************************************************)

module type SRocq_monad = sig
  module Context : Rocq_context.SRocq_context
  module BiEnc : Bi_encoding.S

  (* module Constructor : Rocq_constructor.S *)
  module FwdMap : Hashtbl.S with type key = EConstr.t
  module BckMap : Hashtbl.S with type key = BiEnc.Enc.t

  type fwdmap = BiEnc.fwdmap
  type bckmap = BiEnc.bckmap
  type maps = BiEnc.maps

  val encode : EConstr.t -> BiEnc.Enc.t
  val encoded : EConstr.t -> bool
  val decode : BiEnc.Enc.t -> EConstr.t
  val decode_opt : BiEnc.Enc.t -> EConstr.t option
  val bienc_to_list : unit -> (BiEnc.Enc.t * EConstr.t) list

  type 'a mm = wrapper ref -> 'a in_wrapper

  and wrapper =
    { ctx : Rocq_context.t ref
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
  val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
  val get_env : wrapper ref -> Environ.env in_wrapper
  val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
  val get_maps : wrapper ref -> BiEnc.maps in_wrapper
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

  module type STreeNode = sig
    type t = BiEnc.Enc.t * int

    val to_string : t -> string
  end

  module TreeNode : STreeNode with type t = BiEnc.Enc.t * int

  module type STree = sig
    type 'a tree = Node of 'a * 'a tree list
    type t = TreeNode.t tree

    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> TreeNode.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> TreeNode.t list
    val to_string : t -> string
    val list_to_string : ?args:XUtils.Strfy.style_args -> t list -> string
  end

  module Tree : STree

  module type SConstructor = sig
    module Tree : STree

    type t = EConstr.t * EConstr.t * Tree.t

    val to_string : Environ.env -> Evd.evar_map -> t -> string
  end

  module Constructor : SConstructor

  val make_state_tree_pair_set
    : (module Set.S with type elt = BiEnc.Enc.t * Constructor.Tree.t)
end

module Make (Ctx : Rocq_context.SRocq_context) (E : Encoding.SEncoding) :
  SRocq_monad with module BiEnc.Enc = E = struct
  module Context : Rocq_context.SRocq_context = Ctx
  module BiEnc : Bi_encoding.S with module Enc = E = Bi_encoding.Make (Ctx) (E)
  module FwdMap : Hashtbl.S with type key = EConstr.t = BiEnc.FwdMap
  module BckMap : Hashtbl.S with type key = BiEnc.Enc.t = BiEnc.BckMap

  type fwdmap = BiEnc.fwdmap
  type bckmap = BiEnc.bckmap
  type maps = BiEnc.maps

  let encode : EConstr.t -> BiEnc.Enc.t = BiEnc.encode
  let encoded : EConstr.t -> bool = BiEnc.encoded
  let decode : BiEnc.Enc.t -> EConstr.t = BiEnc.decode
  let decode_opt : BiEnc.Enc.t -> EConstr.t option = BiEnc.decode_opt
  let bienc_to_list : unit -> (BiEnc.Enc.t * EConstr.t) list = BiEnc.to_list

  type 'a mm = wrapper ref -> 'a in_wrapper

  and wrapper =
    { ctx : Rocq_context.t ref
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
    else bind (f index acc) (fun acc' -> iterate (index + 1) upper_bound acc' f)
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
  let get_ctx (st : wrapper ref) : Rocq_context.t in_wrapper =
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
  let fstring (f : Environ.env -> Evd.evar_map -> 'a -> string) : 'a -> string =
    run
      (let open Syntax in
       let* env = get_env in
       let* sigma = get_sigma in
       return (f env sigma))
  ;;

  (********************************************)
  (****** GRAPH *******************************)
  (********************************************)

  module type STreeNode = sig
    type t = BiEnc.Enc.t * int

    val to_string : t -> string
  end

  module TreeNode : STreeNode with type t = BiEnc.Enc.t * int = struct
    type t = BiEnc.Enc.t * int

    let to_string ((enc, index) : t) : string =
      XUtils.Strfy.record
        [ "enc", BiEnc.Enc.to_string enc; "index", XUtils.Strfy.int index ]
    ;;
  end

  module type STree = sig
    type 'a tree = Node of 'a * 'a tree list
    type t = TreeNode.t tree

    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> TreeNode.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> TreeNode.t list
    val to_string : t -> string
    val list_to_string : ?args:XUtils.Strfy.style_args -> t list -> string
  end

  module Tree : STree = struct
    type 'a tree = Node of 'a * 'a tree list
    type t = TreeNode.t tree

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
        XUtils.compare_chain
          [ BiEnc.Enc.compare (fst i1) (fst i2)
          ; Int.compare (snd i1) (snd i2)
          ; List.compare compare l1 l2
          ]
    ;;

    (** converts a given tree into a flattened list with the minimal number of constructors to apply
    *)
    let rec minimize : t -> TreeNode.t list = function
      | Node ((enc, index), []) -> [ enc, index ]
      | Node ((enc, index), h :: tl) ->
        (enc, index)
        :: (List.fold_left (fun acc x -> minimize x :: acc) [] tl
            |> List.fold_left
                 (fun the_min x ->
                   match Int.compare (List.length x) (List.length the_min) with
                   | -1 -> x
                   | _ -> the_min)
                 (minimize h))
    ;;

    exception CannotMinimizeEmptyList of unit

    let min : t list -> TreeNode.t list = function
      | [] -> raise (CannotMinimizeEmptyList ())
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
      XUtils.Strfy.tuple
        (Of TreeNode.to_string)
        (Args (XUtils.Strfy.list (Of to_string)))
        (node, nodes)
    ;;

    let list_to_string
          ?(args : XUtils.Strfy.style_args = XUtils.Strfy.style_args ())
      : t list -> string
      =
      XUtils.Strfy.list
        ~args:{ args with name = Some "Constructor Trees" }
        (Of to_string)
    ;;
  end

  (* module type SConstructor = sig *)
  (* module type STreeNode = sig
     type t = BiEnc.Enc.t * int

     val to_string : t -> string
     end

     module TreeNode : STreeNode with type t = BiEnc.Enc.t * int

     module type STree = sig
     type 'a tree = Node of 'a * 'a tree list
     type t = TreeNode.t tree

     val add : t -> t -> t
     val add_list : t -> t list -> t list
     val equal : t -> t -> bool
     val compare : t -> t -> int
     val minimize : t -> TreeNode.t list

     exception CannotMinimizeEmptyList of unit

     val min : t list -> TreeNode.t list
     val to_string : t -> string
     val list_to_string : ?args:XUtils.Strfy.style_args -> t list -> string
     end *)

  module type SConstructor = sig
    module Tree : STree

    type t = EConstr.t * EConstr.t * Tree.t

    val to_string : Environ.env -> Evd.evar_map -> t -> string
  end

  module Constructor : SConstructor = struct
    module Tree : STree = Tree

    type t = EConstr.t * EConstr.t * Tree.t

    let to_string
          (env : Environ.env)
          (sigma : Evd.evar_map)
          ((action, destination, tree) : t)
      : string
      =
      XUtils.Strfy.record
        [ "action", Rocq_utils.Strfy.econstr env sigma action
        ; "destination", Rocq_utils.Strfy.econstr env sigma destination
        ; "tree", Tree.to_string tree
        ]
    ;;
  end

  let make_state_tree_pair_set
    : (module Set.S with type elt = BiEnc.Enc.t * Constructor.Tree.t)
    =
    (module Set.Make (struct
         type t = BiEnc.Enc.t * Constructor.Tree.t

         let compare t1 t2 =
           XUtils.compare_chain
             [ BiEnc.Enc.compare (fst t1) (fst t2)
             ; Constructor.Tree.compare (snd t1) (snd t2)
             ]
         ;;
       end))
  ;;
end

(***********************************************************************)

module Utils = struct
  module type S = sig
    module M : SRocq_monad
    module Enc : Encoding.SEncoding
    module BiEnc : Bi_encoding.S with module Enc = Enc
    module F : Hashtbl.S with type key = EConstr.t
    module B : Hashtbl.S with type key = M.BiEnc.Enc.t
    module Syntax : M.SYNTAX
    module Tree : M.STree
    module Constructor : M.SConstructor

    type 'a mm = 'a M.mm
    type 'a in_wrapper = 'a M.in_wrapper
    type wrapper = M.wrapper

    val env : unit -> Environ.env mm
    val sigma : unit -> Evd.evar_map mm
    val run : ?reset_encoding:bool -> 'a mm -> 'a
    val return : 'a -> 'a mm
    val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

    val state
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> wrapper ref
      -> 'a in_wrapper

    val sandbox : ?sigma:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_wrapper
    val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm
    val econstr_eq : EConstr.t -> EConstr.t -> bool mm
    val econstr_normalize : EConstr.t -> EConstr.t mm
    val econstr_is_evar : EConstr.t -> bool mm

    val econstr_to_constr
      :  ?abort_on_undefined_evars:bool
      -> EConstr.t
      -> Constr.t mm

    val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
    val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm

    module type SStrfy = sig
      val mm : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string
      val econstr : EConstr.t -> string
      val econstr_rel_decl : EConstr.rel_declaration -> string
    end

    module Strfy : SStrfy

    module type SErrors = sig
      type t =
        (* NOTE: *)
        | Invalid_Sort_LTS of Sorts.family
        | Invalid_Sort_Type of Sorts.family
        (* NOTE: *)
        | InvalidCheckUpdatedCtx of
            (Environ.env
            * Evd.evar_map
            * EConstr.t list
            * EConstr.rel_declaration list)
          (* NOTE: *)
        | InvalidLTSArgsLength of int
        | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

      exception MEBI_exn of t

      (* NOTE: *)
      val invalid_sort_lts : Sorts.family -> exn
      val invalid_sort_type : Sorts.family -> exn

      (* NOTE: *)
      val invalid_check_updated_ctx
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t list
        -> EConstr.rel_declaration list
        -> exn

      (* NOTE: *)
      val invalid_lts_args_length : int -> exn
      val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
    end

    module Errors : SErrors

    module type SErr = sig
      val invalid_check_updated_ctx
        :  EConstr.t list
        -> EConstr.rel_declaration list
        -> 'a mm

      val invalid_lts_args_length : int -> 'a
      val invalid_lts_term_kind : Constr.t -> 'a mm
    end

    module Err : SErr

    val mk_ctx_substl
      :  EConstr.Vars.substl
      -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list
      -> EConstr.Vars.substl mm

    val extract_args
      :  ?substl:EConstr.Vars.substl
      -> Constr.t
      -> Rocq_utils.constructor_args mm
  end

  module Make (M : SRocq_monad) : S = struct
    module M : SRocq_monad = M
    module BiEnc = M.BiEnc
    module Enc = BiEnc.Enc
    module F : Hashtbl.S with type key = EConstr.t = M.BiEnc.FwdMap
    module B : Hashtbl.S with type key = Enc.t = M.BiEnc.BckMap
    module Tree = M.Tree
    module Constructor = M.Constructor
    module Syntax : M.SYNTAX = M.Syntax

    type 'a mm = 'a M.mm
    type 'a in_wrapper = 'a M.in_wrapper
    type wrapper = M.wrapper

    let env () : Environ.env mm = M.get_env
    let sigma () : Evd.evar_map mm = M.get_sigma
    let run = M.run
    let return = M.return
    let iterate = M.iterate
    let state = M.state
    let sandbox = M.sandbox

    let fresh_evar (x : Rocq_utils.evar_source) : EConstr.t mm =
      state (fun env sigma -> Rocq_utils.get_next env sigma x)
    ;;

    let econstr_eq a b : bool mm =
      state (fun env sigma -> sigma, EConstr.eq_constr sigma a b)
    ;;

    let econstr_normalize (x : EConstr.t) : EConstr.t mm =
      let open Syntax in
      let$+ t env sigma = Reductionops.nf_all env sigma x in
      return t
    ;;

    let econstr_kind (x : EConstr.t) : Rocq_utils.econstr_kind mm =
      state (fun env sigma -> sigma, EConstr.kind sigma x)
    ;;

    let econstr_is_evar (x : EConstr.t) : bool mm =
      state (fun env sigma -> sigma, EConstr.isEvar sigma x)
    ;;

    let econstr_to_constr
          ?(abort_on_undefined_evars : bool = false)
          (x : EConstr.t)
      : Constr.t mm
      =
      state (fun env sigma -> sigma, Rocq_convert.econstr_to_constr sigma x)
    ;;

    let econstr_to_constr_opt (x : EConstr.t) : Constr.t option mm =
      state (fun env sigma -> sigma, Rocq_convert.econstr_to_constr_opt sigma x)
    ;;

    module type SStrfy = sig
      val mm : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string
      val econstr : EConstr.t -> string
      val econstr_rel_decl : EConstr.rel_declaration -> string
    end

    module Strfy : SStrfy = struct
      let mm = M.fstring
      let econstr : EConstr.t -> string = mm Rocq_utils.Strfy.econstr

      let econstr_rel_decl : EConstr.rel_declaration -> string =
        mm Rocq_utils.Strfy.econstr_rel_decl
      ;;
    end

    module type SErrors = sig
      type t =
        (* NOTE: *)
        | Invalid_Sort_LTS of Sorts.family
        | Invalid_Sort_Type of Sorts.family
        (* NOTE: *)
        | InvalidCheckUpdatedCtx of
            (Environ.env
            * Evd.evar_map
            * EConstr.t list
            * EConstr.rel_declaration list)
          (* NOTE: *)
        | InvalidLTSArgsLength of int
        | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

      exception MEBI_exn of t

      (* NOTE: *)
      val invalid_sort_lts : Sorts.family -> exn
      val invalid_sort_type : Sorts.family -> exn

      (* NOTE: *)
      val invalid_check_updated_ctx
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t list
        -> EConstr.rel_declaration list
        -> exn

      (* NOTE: *)
      val invalid_lts_args_length : int -> exn
      val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
    end

    module Errors : SErrors = struct
      type t =
        (* NOTE: *)
        | Invalid_Sort_LTS of Sorts.family
        | Invalid_Sort_Type of Sorts.family
        (* NOTE: *)
        | InvalidCheckUpdatedCtx of
            (Environ.env
            * Evd.evar_map
            * EConstr.t list
            * EConstr.rel_declaration list)
          (* NOTE: *)
        | InvalidLTSArgsLength of int
        | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

      exception MEBI_exn of t

      let invalid_sort_lts x = MEBI_exn (Invalid_Sort_LTS x)
      let invalid_sort_type x = MEBI_exn (Invalid_Sort_Type x)

      let invalid_check_updated_ctx env sigma x y =
        MEBI_exn (InvalidCheckUpdatedCtx (env, sigma, x, y))
      ;;

      (** Assert args length == 3 in [Command.extract_args]. *)
      let invalid_lts_args_length i = MEBI_exn (InvalidLTSArgsLength i)

      (** Assert Constr.kind tm is App _ in [Command.extract_args]. *)
      let invalid_lts_term_kind env sigma x =
        MEBI_exn (InvalidLTSTermKind (env, sigma, x))
      ;;

      let mebi_handler : t -> string = function
        (* NOTE: *)
        | Invalid_Sort_LTS x -> "Invalid_Sort_LTS"
        | Invalid_Sort_Type x -> "Invalid_Sort_Type"
        (* NOTE: *)
        | InvalidCheckUpdatedCtx (env, sigma, x, y) ->
          Printf.sprintf
            "Invalid Args to check_updated_ctx. Should both be empty, or both \
             have some.\n\
             substls: %s.\n\
             ctx_tys: %s."
            (XUtils.Strfy.list (Of Strfy.econstr) x)
            (XUtils.Strfy.list (Of Strfy.econstr_rel_decl) y)
          (* NOTE: *)
        | InvalidLTSArgsLength i ->
          Printf.sprintf "assertion: Array.length args == 3 failed. Got %i" i
        | InvalidLTSTermKind (env, sigma, tm) ->
          Printf.sprintf
            "assertion: Constr.kind tm matches App _ failed. Got %s which \
             matches with: %s"
            (Rocq_utils.Strfy.constr env sigma tm)
            (Rocq_utils.Strfy.constr_kind env sigma tm)
      ;;

      let _ =
        CErrors.register_handler (fun e ->
          match e with
          | MEBI_exn e -> Some (Pp.str (mebi_handler e))
          | _ -> None)
      ;;
    end

    module type SErr = sig
      val invalid_check_updated_ctx
        :  EConstr.t list
        -> EConstr.rel_declaration list
        -> 'a mm

      val invalid_lts_args_length : int -> 'a
      val invalid_lts_term_kind : Constr.t -> 'a mm
    end

    module Err : SErr = struct
      let invalid_check_updated_ctx
            (substl : EConstr.t list)
            (ctxl : EConstr.rel_declaration list)
        : 'a mm
        =
        M.state (fun env sigma ->
          raise (Errors.invalid_check_updated_ctx env sigma substl ctxl))
      ;;

      let invalid_lts_args_length (x : int) : 'a =
        raise (Errors.invalid_lts_args_length x)
      ;;

      let invalid_lts_term_kind (x : Constr.t) : 'a =
        M.state (fun env sigma ->
          raise (Errors.invalid_lts_term_kind env sigma x))
      ;;
    end

    let mk_ctx_substl
          (acc : EConstr.Vars.substl)
          (xs : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list)
      : EConstr.Vars.substl mm
      =
      state (fun env sigma -> Rocq_utils.mk_ctx_substl env sigma acc xs)
    ;;

    (** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
        @param ?substl
          is a list of substitutions applied to the terms prior to being returned.
        @param term
          must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
        @return a triple of [lhs_term, action, rhs_term]. *)
    let extract_args ?(substl : EConstr.Vars.substl = []) (term : Constr.t)
      : Rocq_utils.constructor_args mm
      =
      Log.trace __FUNCTION__;
      try return (Rocq_utils.extract_args ~substl term) with
      | Rocq_utils.Rocq_utils_InvalidLtsArgLength x ->
        (* TODO: err *) Err.invalid_lts_args_length x
      | Rocq_utils.Rocq_utils_InvalidLtsTermKind x ->
        Err.invalid_lts_term_kind term
    ;;
  end
end

(* (* (* *)*)*)
module Model = struct
  module type S = sig
    module Enc : Encoding.SEncoding
    module Tree : EncTree.S with type Enc.t = Enc.t
    module Trees : Set.S with type elt = Tree.t

    (* *)
    module type SState = sig
      type t =
        { term : Enc.t
        ; pp : string option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
    end

    module State : SState

    module type SStates = sig
      module S : Set.S with type elt = State.t

      type t = S.t

      val to_string : t -> string
    end

    module States : SStates

    (* *)
    module type SLabel = sig
      type t =
        { term : Enc.t
        ; is_silent : bool option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
      val is_silent : t -> bool
    end

    module Label : SLabel

    module type SLabels = sig
      module S : Set.S with type elt = Label.t

      type t = S.t

      val to_string : t -> string
    end

    module Labels : SLabels

    (* *)
    module type SNote = sig
      type t =
        { from : State.t
        ; label : Label.t
        ; using : Trees.t
        ; goto : State.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val to_string : t -> string
      val is_silent : t -> bool
    end

    module Note : SNote

    (* *)
    module type SAnnotation = sig
      type t =
        { this : Note.t
        ; next : t option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val to_string : t -> string
      val is_empty : t -> bool
      val length : t -> int
      val shorter : t -> t -> t
      val exists : Note.t -> t -> bool
      val exists_label : Label.t -> t -> bool
      val append : Note.t -> t -> t
      val last : t -> Note.t

      exception CannotDropLastOfSingleton of t

      val drop_last : t -> t
    end

    module Annotation : SAnnotation

    (* *)
    module type STransition = sig
      type t =
        { from : State.t
        ; goto : State.t
        ; label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val to_string : t -> string
    end

    module Transition : STransition

    module type STransitions = sig
      module S : Set.S with type elt = Transition.t

      type t = S.t

      val to_string : t -> string
    end

    module Transitions : STransitions

    (* *)
    module type SAction = sig
      type t =
        { label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      val wk_equal : t -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val to_string : t -> string
    end

    module Action : SAction

    module type SActions = sig
      module S : Set.S with type elt = Action.t

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Actions : SActions

    module type SActionMap = sig
      module M : Hashtbl.S with type key = Action.t

      type t = States.t M.t

      val update : t -> Action.t -> States.t -> unit
      val get_destinations : t -> States.t
      val to_string : t -> string
    end

    module ActionMap : SActionMap

    (* *)
    module type SEdge = sig
      type t =
        { from : State.t
        ; goto : State.t
        ; action : Action.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
    end

    module Edge : SEdge

    module type SEdges = sig
      module S : Set.S with type elt = Edge.t

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Edges : SEdges

    module type SEdgeMap = sig
      module M : Hashtbl.S with type key = State.t

      type t = ActionMap.t M.t

      val update : t -> State.t -> Action.t -> States.t -> unit
      val get_destinations : t -> State.t -> States.t
      val get_actions : t -> State.t -> Actions.t
      val get_edges : t -> State.t -> Edges.t
      val to_string : t -> string
    end

    module EdgeMap : SEdgeMap

    (* *)
    module type SParitition = sig
      module S : Set.S with type elt = States.t

      type t = S.t

      val get_reachable : t -> State.t -> EdgeMap.t -> t
      val to_string : t -> string
    end

    module Paritition : SParitition

    (* *)
    module type SInfo = sig
      type t =
        { meta : meta option
        ; weak_labels : Labels.t
        }

      and meta =
        { is_complete : bool
        ; is_merged : bool
        ; bounds : bounds
        ; lts : lts list
        }

      and bounds =
        | States of int
        | Transitions of int

      and lts =
        { enc : Enc.t
        ; constructors : Rocq_bindings.constructor list
        }

      val to_string : t -> string
    end

    module Info : SInfo

    (* *)
    module type SLTS = sig
      type t =
        { init : State.t option
        ; terminals : States.t
        ; alphabet : Labels.t
        ; states : States.t
        ; transitions : Transitions.t
        ; info : Info.t
        }

      val to_string : t -> string
    end

    module LTS : SLTS
  end

  module Make (E : Encoding.SEncoding) : S with module Enc = E = struct
    module Enc : Encoding.SEncoding with type t = E.t = E
    module Tree : EncTree.S with type Enc.t = E.t = EncTree.Make (E)
    module Trees : Set.S with type elt = Tree.t = Set.Make (Tree)

    (* *)
    module type SState = sig
      type t =
        { term : Enc.t
        ; pp : string option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
    end

    module State : SState = struct
      type t =
        { term : Enc.t
        ; pp : string option
        }

      let equal (a : t) (b : t) : bool = Enc.equal a.term b.term
      let compare (a : t) (b : t) : int = Enc.compare a.term b.term
      let hash (x : t) : int = Enc.hash x.term

      (* *)
      let to_string (x : t) : string =
        XUtils.Strfy.record
          [ "term", Enc.to_string x.term
          ; "pp", XUtils.Strfy.option (Args XUtils.Strfy.string) x.pp
          ]
      ;;
    end

    module type SStates = sig
      module S : Set.S with type elt = State.t

      type t = S.t

      val to_string : t -> string
    end

    module States : SStates = struct
      module S : Set.S with type elt = State.t = Set.Make (State)

      type t = S.t

      let to_string (xs : t) : string =
        S.to_list xs
        |> XUtils.Strfy.list
             ~args:{ (XUtils.Strfy.style_args ()) with name = Some "States" }
             (Of State.to_string)
      ;;
    end

    (* *)
    module type SLabel = sig
      type t =
        { term : Enc.t
        ; is_silent : bool option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
      val is_silent : t -> bool
    end

    module Label : SLabel = struct
      type t =
        { term : Enc.t
        ; is_silent : bool option
        }

      let equal (a : t) (b : t) : bool = Enc.equal a.term b.term
      let compare (a : t) (b : t) : int = Enc.compare a.term b.term
      let hash (x : t) : int = Enc.hash x.term

      (* *)
      let to_string (x : t) : string =
        XUtils.Strfy.record
          [ "term", Enc.to_string x.term
          ; ( "is_silent"
            , XUtils.Strfy.option (Args XUtils.Strfy.bool) x.is_silent )
          ]
      ;;

      (* *)
      let is_silent (x : t) : bool = Option.default false x.is_silent
    end

    module type SLabels = sig
      module S : Set.S with type elt = Label.t

      type t = S.t

      val to_string : t -> string
    end

    module Labels : SLabels = struct
      module S : Set.S with type elt = Label.t = Set.Make (Label)

      type t = S.t

      let to_string (xs : t) : string =
        S.to_list xs
        |> XUtils.Strfy.list
             ~args:{ (XUtils.Strfy.style_args ()) with name = Some "Labels" }
             (Of Label.to_string)
      ;;
    end

    (* *)
    (* module Tree : Constructor.Tree.TYPE = Constructor.Tree.Make (T) *)
    (* module Trees : Set.S with type elt = Tree.t = Set.Make (Tree) *)

    (* *)
    module type SNote = sig
      type t =
        { from : State.t
        ; label : Label.t
        ; using : Trees.t
        ; goto : State.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val to_string : t -> string
      val is_silent : t -> bool
    end

    module Note : SNote = struct
      type t =
        { from : State.t
        ; label : Label.t
        ; using : Trees.t
        ; goto : State.t
        }

      let equal (a : t) (b : t) : bool =
        State.equal a.from b.from
        && State.equal a.goto b.goto
        && Label.equal a.label b.label
        && Trees.equal a.using b.using
      ;;

      let compare (a : t) (b : t) : int =
        XUtils.compare_chain
          [ State.compare a.from b.from
          ; State.compare a.goto b.goto
          ; Label.compare a.label b.label
          ; Trees.compare a.using b.using
          ]
      ;;

      (* *)
      let to_string (x : t) : string =
        (* XUtils.Strfy.record
            [ "from", S.to_string x.from
            ; "goto", S.to_string x.goto
            ; "via", L.to_string x.via
            ; "using", XUtils.Strfy.list (Of C.to_string) x.using
           
            ] *)
        Printf.sprintf
          "<State (%s) Goto (%s) Via (%s)>"
          (State.to_string x.from)
          (State.to_string x.goto)
          (Label.to_string x.label)
      ;;

      (* *)
      let is_silent (x : t) : bool = Label.is_silent x.label
    end

    (* *)
    module type SAnnotation = sig
      type t =
        { this : Note.t
        ; next : t option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val to_string : t -> string
      val is_empty : t -> bool
      val length : t -> int
      val shorter : t -> t -> t
      val exists : Note.t -> t -> bool
      val exists_label : Label.t -> t -> bool
      val append : Note.t -> t -> t
      val last : t -> Note.t

      exception CannotDropLastOfSingleton of t

      val drop_last : t -> t
    end

    module Annotation : SAnnotation = struct
      type t =
        { this : Note.t
        ; next : t option
        }

      let rec equal (a : t) (b : t) : bool =
        Note.equal a.this b.this && Option.equal equal a.next b.next
      ;;

      let rec compare (a : t) (b : t) : int =
        XUtils.compare_chain
          [ Note.compare a.this b.this; Option.compare compare a.next b.next ]
      ;;

      let is_empty : t -> bool = function
        | { this; next = None } -> true
        | _ -> false
      ;;

      let rec length : t -> int = function
        | { next = None; _ } -> 1
        | { next = Some next; _ } -> 1 + length next
      ;;

      let shorter (a : t) (b : t) : t =
        match Int.compare (length a) (length b) with -1 -> a | _ -> b
      ;;

      let rec exists (x : Note.t) : t -> bool = function
        | { this; next = None } -> Note.equal x this
        | { this; next = Some next } ->
          if Note.equal x this then true else exists x next
      ;;

      let rec exists_label (x : Label.t) : t -> bool = function
        | { this; next = None } -> Label.equal x this.label
        | { this; next = Some next } ->
          if Label.equal x this.label then true else exists_label x next
      ;;

      let rec append (x : Note.t) : t -> t = function
        | { this; next = None } ->
          { this; next = Some { this = x; next = None } }
        | { this; next = Some next } -> { this; next = Some (append x next) }
      ;;

      let rec last : t -> Note.t = function
        | { this; next = None } -> this
        | { next = Some next; _ } -> last next
      ;;

      exception CannotDropLastOfSingleton of t

      let rec drop_last : t -> t = function
        | { this; next = None } ->
          raise (CannotDropLastOfSingleton { this; next = None })
        | { this; next = Some { next = None; _ }; _ } -> { this; next = None }
        | { this; next = Some next } -> { this; next = Some (drop_last next) }
      ;;

      let rec to_string : t -> string = function
        | { this; next = None } -> Note.to_string this
        | { this; next = Some next } ->
          Printf.sprintf "%s; %s" (Note.to_string this) (to_string next)
      ;;
    end

    module type SAnnotations = sig
      module S : Set.S with type elt = Annotation.t

      type t = S.t

      val to_string : t -> string
    end

    module Annotations : SAnnotations = struct
      module S : Set.S with type elt = Annotation.t = Set.Make (Annotation)

      type t = S.t

      let to_string (xs : t) : string =
        S.to_list xs
        |> XUtils.Strfy.list
             ~args:
               { (XUtils.Strfy.style_args ()) with name = Some "Annotations" }
             (Of Annotation.to_string)
      ;;
    end

    (* *)
    module type STransition = sig
      type t =
        { from : State.t
        ; goto : State.t
        ; label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val to_string : t -> string
    end

    module Transition : STransition = struct
      type t =
        { from : State.t
        ; goto : State.t
        ; label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      let equal (a : t) (b : t) : bool =
        State.equal a.from b.from
        && State.equal a.goto b.goto
        && Label.equal a.label b.label
        && Option.equal Annotation.equal a.annotation b.annotation
        && Trees.equal a.constructor_trees b.constructor_trees
      ;;

      let compare (a : t) (b : t) : int =
        XUtils.compare_chain
          [ State.compare a.from b.from
          ; State.compare a.goto b.goto
          ; Label.compare a.label b.label
          ; Option.compare Annotation.compare a.annotation b.annotation
          ; Trees.compare a.constructor_trees b.constructor_trees
          ]
      ;;

      let is_silent (x : t) : bool = Label.is_silent x.label

      let annotation_is_empty : t -> bool = function
        | { annotation = None; _ } -> true
        | { annotation = Some annotation; _ } -> Annotation.is_empty annotation
      ;;

      let to_string (x : t) : string =
        XUtils.Strfy.record
          [ "from", State.to_string x.from
          ; "goto", State.to_string x.from
          ; "label", Label.to_string x.label
          ; ( "annotation"
            , XUtils.Strfy.option (Of Annotation.to_string) x.annotation )
          ; ( "constructor_trees"
            , XUtils.Strfy.list
                (Of Tree.to_string)
                (Trees.to_list x.constructor_trees) )
          ]
      ;;
    end

    module type STransitions = sig
      module S : Set.S with type elt = Transition.t

      type t = S.t

      val to_string : t -> string
    end

    module Transitions : STransitions = struct
      module S : Set.S with type elt = Transition.t = Set.Make (Transition)

      type t = S.t

      let to_string (xs : t) : string =
        S.to_list xs
        |> XUtils.Strfy.list
             ~args:
               { (XUtils.Strfy.style_args ()) with name = Some "Transitions" }
             (Of Transition.to_string)
      ;;
    end

    (* *)
    module type SAction = sig
      type t =
        { label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      val wk_equal : t -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val to_string : t -> string
    end

    module Action : SAction = struct
      type t =
        { label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      let wk_equal (a : t) (b : t) : bool = Label.equal a.label b.label

      let equal (a : t) (b : t) : bool =
        Label.equal a.label b.label
        && Option.equal Annotation.equal a.annotation b.annotation
        && Trees.equal a.constructor_trees b.constructor_trees
      ;;

      let compare (a : t) (b : t) : int =
        XUtils.compare_chain
          [ Label.compare a.label b.label
          ; Option.compare Annotation.compare a.annotation b.annotation
          ; Trees.compare a.constructor_trees b.constructor_trees
          ]
      ;;

      let hash (x : t) : int = Label.hash x.label
      let is_silent (x : t) : bool = Label.is_silent x.label

      let annotation_is_empty : t -> bool = function
        | { annotation = None; _ } -> true
        | { annotation = Some annotation; _ } -> Annotation.is_empty annotation
      ;;

      let to_string (x : t) : string =
        XUtils.Strfy.record
          [ "label", Label.to_string x.label
          ; ( "annotation"
            , XUtils.Strfy.option (Of Annotation.to_string) x.annotation )
          ; ( "constructor_trees"
            , XUtils.Strfy.list
                (Of Tree.to_string)
                (Trees.to_list x.constructor_trees) )
          ]
      ;;
    end

    module type SActions = sig
      module S : Set.S with type elt = Action.t

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Actions : SActions = struct
      module S : Set.S with type elt = Action.t = Set.Make (Action)

      type t = S.t

      let labelled (xs : t) (y : Label.t) : t =
        S.filter (fun ({ label; _ } : Action.t) -> Label.equal label y) xs
      ;;

      let to_string (xs : t) : string =
        S.to_list xs
        |> XUtils.Strfy.list
             ~args:{ (XUtils.Strfy.style_args ()) with name = Some "Actions" }
             (Of Action.to_string)
      ;;
    end

    module type SActionMap = sig
      module M : Hashtbl.S with type key = Action.t

      type t = States.t M.t

      val update : t -> Action.t -> States.t -> unit
      val get_destinations : t -> States.t
      val to_string : t -> string
    end

    module ActionMap : SActionMap = struct
      module M : Hashtbl.S with type key = Action.t = Hashtbl.Make (Action)

      type t = States.t M.t

      let update (x : t) (action : Action.t) (states : States.t) : unit =
        if States.S.is_empty states
        then ()
        else (
          match M.find_opt x action with
          | None -> M.add x action states
          | Some old_states ->
            M.replace x action (States.S.union old_states states))
      ;;

      (** [get_destinations x f e] merges the values of [x] using [f], where [e] is some initial (i.e., "empty") collection of ['a].
      *)
      let get_destinations (x : t) : States.t =
        M.to_seq_values x |> Seq.fold_left States.S.union States.S.empty
      ;;

      let to_string (xs : t) : string =
        M.to_seq xs
        |> List.of_seq
        |> XUtils.Strfy.list
             ~args:{ (XUtils.Strfy.style_args ()) with name = Some "Actions" }
             (Of
                (fun (k, v) ->
                  XUtils.Strfy.record
                    [ "action", Action.to_string k; "->", States.to_string v ]))
      ;;
    end

    (* *)
    module type SEdge = sig
      type t =
        { from : State.t
        ; goto : State.t
        ; action : Action.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
    end

    module Edge : SEdge = struct
      type t =
        { from : State.t
        ; goto : State.t
        ; action : Action.t
        }

      let equal (a : t) (b : t) : bool =
        State.equal a.from b.from
        && State.equal a.goto b.goto
        && Action.equal a.action b.action
      ;;

      let compare (a : t) (b : t) : int =
        XUtils.compare_chain
          [ State.compare a.from b.from
          ; State.compare a.goto b.goto
          ; Action.compare a.action b.action
          ]
      ;;

      let is_silent (x : t) : bool = Action.is_silent x.action

      let to_string (x : t) : string =
        XUtils.Strfy.record
          [ "from", State.to_string x.from
          ; "goto", State.to_string x.from
          ; "action", Action.to_string x.action
          ]
      ;;
    end

    module type SEdges = sig
      module S : Set.S with type elt = Edge.t

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Edges : SEdges = struct
      module S : Set.S with type elt = Edge.t = Set.Make (Edge)

      type t = S.t

      let labelled (xs : t) (y : Label.t) : t =
        S.filter (fun ({ action; _ } : Edge.t) -> Label.equal action.label y) xs
      ;;

      let to_string (xs : t) : string =
        S.to_list xs
        |> XUtils.Strfy.list
             ~args:{ (XUtils.Strfy.style_args ()) with name = Some "Edges" }
             (Of Edge.to_string)
      ;;
    end

    module type SEdgeMap = sig
      module M : Hashtbl.S with type key = State.t

      type t = ActionMap.t M.t

      val update : t -> State.t -> Action.t -> States.t -> unit
      val get_destinations : t -> State.t -> States.t
      val get_actions : t -> State.t -> Actions.t
      val get_edges : t -> State.t -> Edges.t
      val to_string : t -> string
    end

    module EdgeMap : SEdgeMap = struct
      module M : Hashtbl.S with type key = State.t = Hashtbl.Make (State)

      type t = ActionMap.t M.t

      let update
            (x : t)
            (from : State.t)
            (action : Action.t)
            (destinations : States.t)
        : unit
        =
        match M.find_opt x from with
        | None ->
          [ action, destinations ]
          |> List.to_seq
          |> ActionMap.M.of_seq
          |> M.add x from
        | Some actions -> ActionMap.update actions action destinations
      ;;

      let get_destinations (x : t) (from : State.t) : States.t =
        ActionMap.get_destinations (M.find x from)
      ;;

      let get_actions (x : t) (from : State.t) : Actions.t =
        M.find x from |> ActionMap.M.to_seq_keys |> Actions.S.of_seq
      ;;

      let get_edges (x : t) (from : State.t) : Edges.t =
        ActionMap.M.fold
          (fun (action : Action.t) (v : States.t) (acc : Edges.t) : Edges.t ->
            States.S.fold
              (fun (goto : State.t) (acc : Edges.t) : Edges.t ->
                Edges.S.add { from; action; goto } acc)
              v
              acc)
          (M.find x from)
          Edges.S.empty
      ;;

      let to_string (xs : t) : string =
        M.to_seq xs
        |> List.of_seq
        |> XUtils.Strfy.list
             ~args:{ (XUtils.Strfy.style_args ()) with name = Some "Edges" }
             (Of
                (fun (k, v) ->
                  XUtils.Strfy.record
                    [ "from", State.to_string k; "->", ActionMap.to_string v ]))
      ;;
    end

    (* *)
    module type SParitition = sig
      module S : Set.S with type elt = States.t

      type t = S.t

      val get_reachable : t -> State.t -> EdgeMap.t -> t
      val to_string : t -> string
    end

    module Paritition : SParitition = struct
      module S : Set.S with type elt = States.t = Set.Make (States.S)

      type t = S.t

      let get_reachable (x : t) (from : State.t) (edges : EdgeMap.t) : t =
        let destinations : States.t = EdgeMap.get_destinations edges from in
        S.filter
          (fun (y : States.t) ->
            Bool.not (States.S.is_empty (States.S.inter y destinations)))
          x
      ;;

      let to_string (xs : t) : string =
        S.to_list xs
        |> XUtils.Strfy.list
             ~args:
               { (XUtils.Strfy.style_args ()) with name = Some "Paritition" }
             (Of States.to_string)
      ;;
    end

    (* *)
    module type SInfo = sig
      type t =
        { meta : meta option
        ; weak_labels : Labels.t
        }

      and meta =
        { is_complete : bool
        ; is_merged : bool
        ; bounds : bounds
        ; lts : lts list
        }

      and bounds =
        | States of int
        | Transitions of int

      and lts =
        { enc : Enc.t
        ; constructors : Rocq_bindings.constructor list
        }

      val to_string : t -> string
    end

    module Info : SInfo = struct
      type t =
        { meta : meta option
        ; weak_labels : Labels.t
        }

      and meta =
        { is_complete : bool
        ; is_merged : bool
        ; bounds : bounds
        ; lts : lts list
        }

      and bounds =
        | States of int
        | Transitions of int

      and lts =
        { enc : Enc.t
        ; constructors : Rocq_bindings.constructor list
        }

      let to_string (x : t) : string =
        let f
              ?(args : XUtils.Strfy.style_args = XUtils.Strfy.style_args ())
              (y : meta)
          : string
          =
          XUtils.Strfy.record
            ~args
            [ "is complete", XUtils.Strfy.bool y.is_complete
            ; "is merged", XUtils.Strfy.bool y.is_merged
            ; ( "bounds"
              , match y.bounds with
                | States i -> Printf.sprintf "States (%i)" i
                | Transitions i -> Printf.sprintf "Transitions (%i)" i )
            ; ( "lts"
              , XUtils.Strfy.list
                  (Of
                     (fun ({ enc; constructors } : lts) ->
                       XUtils.Strfy.record
                         [ "enc", Enc.to_string enc
                         ; ( "constructors"
                           , XUtils.Strfy.list
                               (Of Rocq_bindings.constructor_to_string)
                               constructors )
                         ]))
                  y.lts )
            ]
        in
        XUtils.Strfy.record
          [ "meta", XUtils.Strfy.option (Args f) x.meta
          ; "weak labels", Labels.to_string x.weak_labels
          ]
      ;;
    end

    (* *)
    module type SLTS = sig
      type t =
        { init : State.t option
        ; terminals : States.t
        ; alphabet : Labels.t
        ; states : States.t
        ; transitions : Transitions.t
        ; info : Info.t
        }

      val to_string : t -> string
    end

    module LTS : SLTS = struct
      type t =
        { init : State.t option
        ; terminals : States.t
        ; alphabet : Labels.t
        ; states : States.t
        ; transitions : Transitions.t
        ; info : Info.t
        }

      let to_string (x : t) : string =
        XUtils.Strfy.record
          [ "init", XUtils.Strfy.option (Of State.to_string) x.init
          ; "terminals", States.to_string x.terminals
          ; "alphabet", Labels.to_string x.alphabet
          ; "states", States.to_string x.states
          ; "transitions", Transitions.to_string x.transitions
          ; "info", Info.to_string x.info
          ]
      ;;
    end

    (* *)
    module type SFSM = sig
      type t =
        { init : State.t option
        ; terminals : States.t
        ; alphabet : Labels.t
        ; states : States.t
        ; edges : EdgeMap.t
        ; info : Info.t
        }
    end

    module FSM : SFSM = struct
      type t =
        { init : State.t option
        ; terminals : States.t
        ; alphabet : Labels.t
        ; states : States.t
        ; edges : EdgeMap.t
        ; info : Info.t
        }

      let to_string (x : t) : string =
        XUtils.Strfy.record
          [ "init", XUtils.Strfy.option (Of State.to_string) x.init
          ; "terminals", States.to_string x.terminals
          ; "alphabet", Labels.to_string x.alphabet
          ; "states", States.to_string x.states
          ; "edges", EdgeMap.to_string x.edges
          ; "info", Info.to_string x.info
          ]
      ;;
    end

    (* *)
    module type SConvert = sig
      val transitions_to_edgemap : Transitions.t -> EdgeMap.t
      val lts_to_fsm : LTS.t -> FSM.t
    end

    module Convert : SConvert = struct
      (* let transitions_to_edges (xs:Transitions.t) : Edges.t = () *)

      let transitions_to_edgemap (xs : Transitions.t) : EdgeMap.t =
        let edges : EdgeMap.t = EdgeMap.M.create 0 in
        Transitions.S.iter
          (fun ({ from; goto; label; annotation; constructor_trees } :
                 Transition.t) ->
            EdgeMap.update
              edges
              from
              { label; annotation; constructor_trees }
              (States.S.singleton goto))
          xs;
        edges
      ;;

      let lts_to_fsm (x : LTS.t) : FSM.t =
        { init = x.init
        ; terminals = x.terminals
        ; alphabet = x.alphabet
        ; states = x.states
        ; edges = transitions_to_edgemap x.transitions
        ; info = x.info
        }
      ;;
    end
  end
end
