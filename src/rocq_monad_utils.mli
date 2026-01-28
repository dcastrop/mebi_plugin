module Make : (C : Rocq_context.SRocq_context) (E : Encoding.SEncoding) -> sig
  module Ctx : sig
    val get : unit -> Rocq_context.t ref
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
    val update : Environ.env ref -> Evd.evar_map ref -> unit
  end

  module Enc : sig
    type t = Rocq_monad.Make(C)(E).Enc.t

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

  module F : sig
    type key = Evd.econstr
    type 'a t = 'a Bi_encoding.Make(Ctx)(Enc).F.t

    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  module B : sig
    type key = Enc.t
    type 'a t = 'a Bi_encoding.Make(Ctx)(Enc).B.t

    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  type maps = Bi_encoding.Make(Ctx)(Enc).maps =
    { fwd : Enc.t F.t
    ; bck : Evd.econstr B.t
    }

  val the_maps : unit -> maps ref
  val reset : unit -> unit
  val fwdmap : unit -> Enc.t F.t
  val bckmap : unit -> Evd.econstr B.t
  val get_encoding : Evd.econstr -> Enc.t
  val encode : Evd.econstr -> Enc.t
  val encoded : Evd.econstr -> bool
  val get_econstr : Enc.t -> Evd.econstr

  exception CannotDecode of Enc.t

  val decode : Enc.t -> Evd.econstr
  val decode_opt : Enc.t -> Evd.econstr option
  val decode_map : 'a B.t -> 'a F.t
  val encode_map : 'a F.t -> 'a B.t
  val to_list : unit -> (Enc.t * Evd.econstr) list
  val make_hashtbl : (module Hashtbl.S with type key = Enc.t)
  val make_set : (module Set.S with type elt = Enc.t)
  val bienc_to_list : unit -> (Enc.t * Evd.econstr) list

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

  module Syntax : sig
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

  val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
  val get_env : wrapper ref -> Environ.env in_wrapper
  val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
  val get_maps : wrapper ref -> maps in_wrapper
  val get_fwdmap : wrapper ref -> Enc.t F.t in_wrapper
  val get_bckmap : wrapper ref -> Evd.econstr B.t in_wrapper
  val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string

  module Tree : sig
    module type STreeNode = sig
      type t = Enc.t * int

      val to_string : t -> string
    end

    module TreeNode : sig
      type t = Enc.t * int

      val to_string : t -> string
    end

    type 'a tree = 'a Enc_tree.Make(Enc).tree = Node of 'a * 'a tree list
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

  module Constructor : sig
    type t = Evd.econstr * Evd.econstr * Tree.t

    val to_string : Environ.env -> Evd.evar_map -> t -> string
  end

  val make_state_tree_pair_set : (module Set.S with type elt = Enc.t * Tree.t)
  val fresh_evar : Rocq_utils.evar_source -> Evd.econstr mm
  val econstr_eq : Evd.econstr -> Evd.econstr -> bool mm
  val econstr_normalize : Evd.econstr -> Evd.econstr mm
  val econstr_kind : Evd.econstr -> Rocq_utils.econstr_kind mm
  val econstr_is_evar : Evd.econstr -> bool mm

  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> Evd.econstr
    -> Constr.t mm

  val econstr_to_constr_opt : Evd.econstr -> Constr.t option mm

  module Strfy : sig
    val econstr : Evd.econstr -> string
    val econstr_rel_decl : EConstr.rel_declaration -> string
  end

  module type SErrors = sig
    type t =
      | Invalid_Sort_LTS of Sorts.family
      | Invalid_Sort_Type of Sorts.family
      | InvalidCheckUpdatedCtx of
          (Environ.env
          * Evd.evar_map
          * Evd.econstr list
          * EConstr.rel_declaration list)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

    exception MEBI_exn of t

    val invalid_sort_lts : Sorts.family -> exn
    val invalid_sort_type : Sorts.family -> exn

    val invalid_check_updated_ctx
      :  Environ.env
      -> Evd.evar_map
      -> Evd.econstr list
      -> EConstr.rel_declaration list
      -> exn

    val invalid_lts_args_length : int -> exn
    val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  end

  module Errors : SErrors

  module type SErr = sig
    val invalid_check_updated_ctx
      :  Evd.econstr list
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

  module Unification : sig
    module type SPair = sig
      type t =
        { to_check : Evd.econstr
        ; acc : Evd.econstr
        }

      val to_string : Environ.env -> Evd.evar_map -> t -> string

      val make
        :  Environ.env
        -> Evd.evar_map
        -> Evd.econstr
        -> Evd.econstr
        -> Evd.evar_map * t

      val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
    end

    module Pair : SPair

    module type SProblem = sig
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : Tree.t
        }

      val to_string : Environ.env -> Evd.evar_map -> t -> string
      val unify_opt : t -> Tree.t option mm
    end

    module Problem : SProblem

    module type SProblems = sig
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      val empty : unit -> t mm
      val list_is_empty : t list -> bool
      val to_string : Environ.env -> t -> string
      val list_to_string : Environ.env -> t list -> string

      val sandbox_unify_all_opt
        :  Evd.econstr
        -> Evd.econstr
        -> t
        -> (Evd.econstr * Evd.econstr * Tree.t list) option mm
    end

    module Problems : SProblems

    module type SConstructors = sig
      type t = Constructor.t list

      val to_string : Environ.env -> Evd.evar_map -> t -> string

      val retrieve
        :  int
        -> t
        -> Evd.econstr
        -> Evd.econstr
        -> Enc.t * Problems.t list
        -> t mm
    end

    module Constructors : SConstructors

    val constr_to_problem
      :  Rocq_utils.constructor_args
      -> Constructor.t
      -> Problem.t

    val map_problems
      :  Rocq_utils.constructor_args
      -> Constructors.t
      -> Problems.t mm

    val cross_product : Problems.t list -> Problems.t -> Problems.t list
    val does_constructor_unify : Evd.econstr -> Evd.econstr -> bool mm

    val check_constructor_args_unify
      :  Evd.econstr
      -> Evd.econstr
      -> Rocq_utils.constructor_args
      -> bool mm

    val axiom_constructor
      :  Evd.econstr
      -> Evd.econstr
      -> Enc.t * int
      -> Constructors.t
      -> Constructors.t mm

    val check_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Enc.t Rocq_ind.t F.t
      -> Evd.econstr
      -> Evd.econstr
      -> Enc.t
      -> Constructors.t mm

    val explore_valid_constructor
      :  Enc.t Rocq_ind.t F.t
      -> Evd.econstr
      -> Enc.t
      -> Rocq_utils.constructor_args
      -> int * Constructors.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> Constructors.t mm

    val check_updated_ctx
      :  Enc.t
      -> Problems.t list
      -> Enc.t Rocq_ind.t F.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> (Enc.t * Problems.t list) option mm

    val check_for_next_constructors
      :  int
      -> Evd.econstr
      -> Evd.econstr
      -> Constructors.t
      -> (Enc.t * Problems.t list) option
      -> Constructors.t mm

    val collect_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Enc.t Rocq_ind.t F.t
      -> Evd.econstr
      -> Evd.econstr
      -> Enc.t
      -> Constructors.t mm
  end
end
