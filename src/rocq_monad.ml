(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
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

  module type SConstructor = sig
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
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

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

  module type SConstructor = sig
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
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Tree : STree

    type t = EConstr.t * EConstr.t * Tree.t

    val to_string : Environ.env -> Evd.evar_map -> t -> string
  end

  module Constructor : SConstructor = struct
    module type STreeNode = sig
      type t = BiEnc.Enc.t * int

      val to_string : t -> string
    end

    module TreeNode : STreeNode with type t = BiEnc.Enc.t * int = struct
      type t = BiEnc.Enc.t * int

      let to_string ((enc, index) : t) : string =
        Utils.Strfy.record
          [ "enc", BiEnc.Enc.to_string enc; "index", Utils.Strfy.int index ]
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
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
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
          Utils.compare_chain
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
                     match
                       Int.compare (List.length x) (List.length the_min)
                     with
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
        Utils.Strfy.tuple
          (Of TreeNode.to_string)
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

    type t = EConstr.t * EConstr.t * Tree.t

    let to_string
          (env : Environ.env)
          (sigma : Evd.evar_map)
          ((action, destination, tree) : t)
      : string
      =
      Utils.Strfy.record
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
           Utils.compare_chain
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

    module Constructor : sig
      module type STreeNode = sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = Enc.t * int

        val to_string : t -> string
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
        val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
      end

      module Tree : STree

      type t = EConstr.t * EConstr.t * Tree.t

      val to_string : Environ.env -> Evd.evar_map -> t -> string
    end

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
            (Utils.Strfy.list (Of Strfy.econstr) x)
            (Utils.Strfy.list (Of Strfy.econstr_rel_decl) y)
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
