module Make : (Log : Logger.SLogger)
    (C : Rocq_context.SRocq_context)
    (E : Encoding.SEncoding)
    -> sig
  module Ctx : sig
    val get : unit -> Rocq_context.t ref
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
    val update : Environ.env ref -> Evd.evar_map ref -> unit
  end

  module Enc : sig
    type t = Rocq_monad.Make(Log)(C)(E).Enc.t

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
    type key = EConstr.t
    type 'a t = 'a Bi_encoding.Make(Log)(Ctx)(Enc).F.t

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
    type 'a t = 'a Bi_encoding.Make(Log)(Ctx)(Enc).B.t

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

  type maps = Bi_encoding.Make(Log)(Ctx)(Enc).maps =
    { fwd : Enc.t F.t
    ; bck : EConstr.t B.t
    }

  val the_maps : maps ref option ref
  val reset : unit -> unit

  exception MapsNotInitialised of unit

  val get_the_maps : unit -> maps ref
  val fwdmap : unit -> Enc.t F.t
  val bckmap : unit -> EConstr.t B.t

  exception EncodingNotFound of EConstr.t

  val get_encoding : EConstr.t -> Enc.t
  val encode : EConstr.t -> Enc.t
  val encoded : EConstr.t -> bool

  exception DecodingNotFound of Enc.t

  val get_econstr : Enc.t -> EConstr.t

  exception CannotDecode of Enc.t

  val decode : Enc.t -> EConstr.t
  val decode_opt : Enc.t -> EConstr.t option
  val decode_map : 'a B.t -> 'a F.t
  val encode_map : 'a F.t -> 'a B.t
  val to_list : unit -> (Enc.t * EConstr.t) list
  val bienc_to_list : unit -> (Enc.t * EConstr.t) list

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
  val get_bckmap : wrapper ref -> EConstr.t B.t in_wrapper
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
    type t = EConstr.t * EConstr.t * Tree.t

    val to_string : Environ.env -> Evd.evar_map -> t -> string
  end

  val make_state_tree_pair_set
    :  unit
    -> (module Set.S with type elt = Enc.t * Tree.t)

  val make_hashtbl : unit -> (module Hashtbl.S with type key = Enc.t)
  val make_set : unit -> (module Set.S with type elt = Enc.t)
  val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm
  val econstr_eq : EConstr.t -> EConstr.t -> bool mm
  val econstr_normalize : EConstr.t -> EConstr.t mm
  val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm
  val econstr_is_evar : EConstr.t -> bool mm

  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> EConstr.t
    -> Constr.t mm

  val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
  val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm
  val exists_eq : EConstr.t -> 'a list -> ('a -> EConstr.t) -> bool mm
  val type_of_econstr : EConstr.t -> EConstr.t mm
  val type_of_constrexpr : Constrexpr.constr_expr -> EConstr.t mm

  module Strfy : sig
    val constr : Constr.t -> string
    val constr_kind : Constr.t -> string
    val econstr : EConstr.t -> string
    val econstr_kind : EConstr.t -> string
    val econstr_rel_decl : EConstr.rel_declaration -> string
    val hyp_name : Rocq_utils.hyp -> string
    val hyp_type : Rocq_utils.hyp -> string
    val hyp : Rocq_utils.hyp -> string
    val hyp_value : Rocq_utils.hyp -> string
    val rocq_ind : ('a -> string) -> 'a Rocq_ind.t -> string
    val econstr_bindings : EConstr.t Tactypes.bindings -> string
  end

  module type SErrors = sig
    type t =
      | LTS_Empty
      | LTS_Incomplete
      | Not_Bisimilar
      | Invalid_Ind_Kind of Rocq_ind.kind
      | Invalid_Sort_LTS of Sorts.family
      | Invalid_Sort_Type of Sorts.family
      | Invalid_Ref_LTS of Names.GlobRef.t
      | Invalid_Ref_Type of Names.GlobRef.t
      | Invalid_Arity of (Environ.env * Evd.evar_map * Constr.t)
      | InvalidCheckUpdatedCtx of
          (Environ.env
          * Evd.evar_map
          * EConstr.t list
          * EConstr.rel_declaration list)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

    exception MEBI_exn of t

    val lts_empty : unit -> exn
    val lts_incomplete : unit -> exn
    val not_bisimilar : unit -> exn
    val invalid_ind_kind : Rocq_ind.kind -> exn
    val invalid_sort_lts : Sorts.family -> exn
    val invalid_sort_type : Sorts.family -> exn
    val invalid_ref_lts : Names.GlobRef.t -> exn
    val invalid_ref_type : Names.GlobRef.t -> exn
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn

    val invalid_check_updated_ctx
      :  Environ.env
      -> Evd.evar_map
      -> EConstr.t list
      -> EConstr.rel_declaration list
      -> exn

    val invalid_lts_args_length : int -> exn
    val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  end

  module Errors : SErrors

  module type SErr = sig
    val lts_empty : unit -> 'a
    val lts_incomplete : unit -> 'a
    val not_bisimilar : unit -> 'a
    val invalid_ind_kind : Rocq_ind.kind -> 'a
    val invalid_sort_lts : Sorts.family -> 'a
    val invalid_sort_type : Sorts.family -> 'a
    val invalid_ref_lts : Names.GlobRef.t -> 'a
    val invalid_ref_type : Names.GlobRef.t -> 'a
    val invalid_arity : Constr.t -> 'a mm

    val invalid_check_updated_ctx
      :  EConstr.t list
      -> EConstr.rel_declaration list
      -> 'a mm

    val invalid_lts_args_length : int -> 'a
    val invalid_lts_term_kind : Constr.t -> 'a mm
  end

  module Err : SErr

  module Ind : sig
    type t = Enc.t Rocq_ind.t

    val get_lts : t -> Rocq_ind.LTS.t
    val get_lts_term_type : t -> EConstr.t
    val get_lts_label_type : t -> EConstr.t
    val get_lts_constructor_types : t -> Rocq_ind.LTS.constructor array
    val to_string : Environ.env -> Evd.evar_map -> t -> string
    val lookup : Names.inductive -> Declarations.mind_specif mm

    val assert_mip_arity_is_type_or_set
      :  Declarations.inductive_arity
      -> unit mm

    val assert_mip_arity_is_prop : Declarations.inductive_arity -> unit mm

    val lts_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_type_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_prop_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_labels_and_terms
      :  Declarations.mind_specif
      -> (Constr.rel_declaration * Constr.rel_declaration) mm

    val lts : Names.GlobRef.t -> t mm
  end

  val mk_ctx_substl
    :  EConstr.Vars.substl
    -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
    -> EConstr.Vars.substl mm

  val extract_args
    :  ?substl:EConstr.Vars.substl
    -> Constr.t
    -> Rocq_utils.constructor_args mm

  module Unification : sig
    module type SPair = sig
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      val to_string : Environ.env -> Evd.evar_map -> t -> string

      val make
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t
        -> EConstr.t
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
        :  EConstr.t
        -> EConstr.t
        -> t
        -> (EConstr.t * EConstr.t * Tree.t list) option mm
    end

    module Problems : SProblems

    module type SConstructors = sig
      type t = Constructor.t list

      val to_string : Environ.env -> Evd.evar_map -> t -> string

      val retrieve
        :  int
        -> t
        -> EConstr.t
        -> EConstr.t
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
    val does_constructor_unify : EConstr.t -> EConstr.t -> bool mm

    val check_constructor_args_unify
      :  EConstr.t
      -> EConstr.t
      -> Rocq_utils.constructor_args
      -> bool mm

    val axiom_constructor
      :  EConstr.t
      -> EConstr.t
      -> Enc.t * int
      -> Constructors.t
      -> Constructors.t mm

    val check_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> Enc.t
      -> Constructors.t mm

    val explore_valid_constructor
      :  Ind.t F.t
      -> EConstr.t
      -> Enc.t
      -> Rocq_utils.constructor_args
      -> int * Constructors.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> Constructors.t mm

    val check_updated_ctx
      :  Enc.t
      -> Problems.t list
      -> Ind.t F.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> (Enc.t * Problems.t list) option mm

    val check_for_next_constructors
      :  int
      -> EConstr.t
      -> EConstr.t
      -> Constructors.t
      -> (Enc.t * Problems.t list) option
      -> Constructors.t mm

    val collect_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> Enc.t
      -> Constructors.t mm
  end
end
