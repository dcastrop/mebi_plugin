module Make : (Log : Logger.S)
    (Ctx : Rocq_context.SRocq_context)
    (Enc : Encoding.S)
    (Tree : sig
              module Node : sig
                  type t = Enc.t * int

                  val compare : t -> t -> int
                  val equal : t -> t -> bool
                  val json : ?as_elt:bool -> t -> Yojson.t
                  val to_string : ?pretty:bool -> t -> string
                  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
                end
                with type t = Enc.t * int

              type 'a tree = N of 'a * 'a tree list
              type t = Node.t tree

              val add : t -> t -> t
              val add_list : t -> t list -> t list
              val equal : t -> t -> bool
              val compare : t -> t -> int
              val minimize : t -> Node.t list

              exception CannotMinimizeEmptyList of unit

              val min : t list -> Node.t list
              val json : ?as_elt:bool -> t -> Yojson.t
              val to_string : ?pretty:bool -> t -> string
              val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
            end
            with type Node.t = Enc.t * int)
    -> sig
  include module type of Rocq_monad.Make (Log) (Ctx) (Enc)

  val fresh_evar : Rocq_utils.evar_source -> Evd.econstr mm
  val econstr_eq : Evd.econstr -> Evd.econstr -> bool
  val econstr_compare : Evd.econstr -> Evd.econstr -> int
  val get_encoding : Evd.econstr -> Enc.t
  val econstr_kind : Evd.econstr -> Rocq_utils.econstr_kind mm
  val econstr_is_evar : Evd.econstr -> bool mm

  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> Evd.econstr
    -> Constr.t mm

  val econstr_to_constr_opt : Evd.econstr -> Constr.t option mm
  val constrexpr_to_econstr : Constrexpr.constr_expr -> Evd.econstr mm
  val exists_eq : Evd.econstr -> 'a list -> ('a -> Evd.econstr) -> bool
  val type_of_econstr : Evd.econstr -> Evd.econstr mm
  val type_of_constrexpr : Constrexpr.constr_expr -> Evd.econstr mm

  module Strfy : sig
    val constr : Constr.t -> string
    val constr_kind : Constr.t -> string
    val econstr : Evd.econstr -> string
    val econstr_kind : Evd.econstr -> string
    val econstr_rel_decl : EConstr.rel_declaration -> string
    val hyp_name : Rocq_utils.hyp -> string
    val hyp_type : Rocq_utils.hyp -> string
    val hyp : Rocq_utils.hyp -> string
    val hyp_value : Rocq_utils.hyp -> string
    val rocq_ind : ('a -> string) -> 'a Rocq_ind.t -> string
    val econstr_bindings : Evd.econstr Tactypes.bindings -> string
  end

  module type SErrors = sig
    type t =
      | LTS_Empty
      | LTS_Incomplete of string
      | Not_Bisimilar
      | Invalid_Ind_Kind of Rocq_ind.kind
      | Invalid_Sort_LTS of Sorts.Quality.t
      | Invalid_Sort_Type of Sorts.Quality.t
      | Invalid_Ref_LTS of Names.GlobRef.t
      | Invalid_Ref_Type of Names.GlobRef.t
      | Invalid_Arity of (Environ.env * Evd.evar_map * Constr.t)
      | InvalidCheckUpdatedCtx of
          (Environ.env
          * Evd.evar_map
          * Evd.econstr list
          * EConstr.rel_declaration list)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

    exception MEBI_exn of t

    val lts_empty : unit -> exn
    val lts_incomplete : string -> exn
    val not_bisimilar : unit -> exn
    val invalid_ind_kind : Rocq_ind.kind -> exn
    val invalid_sort_lts : Sorts.Quality.t -> exn
    val invalid_sort_type : Sorts.Quality.t -> exn
    val invalid_ref_lts : Names.GlobRef.t -> exn
    val invalid_ref_type : Names.GlobRef.t -> exn
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn

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
    val lts_empty : unit -> 'a
    val lts_incomplete : string -> 'a
    val not_bisimilar : unit -> 'a
    val invalid_ind_kind : Rocq_ind.kind -> 'a
    val invalid_sort_lts : Sorts.Quality.t -> 'a
    val invalid_sort_type : Sorts.Quality.t -> 'a
    val invalid_ref_lts : Names.GlobRef.t -> 'a
    val invalid_ref_type : Names.GlobRef.t -> 'a
    val invalid_arity : Constr.t -> 'a mm

    val invalid_check_updated_ctx
      :  Evd.econstr list
      -> EConstr.rel_declaration list
      -> 'a mm

    val invalid_lts_args_length : int -> 'a
    val invalid_lts_term_kind : Constr.t -> 'a mm
  end

  module Err : SErr

  module Ind : sig
    type t = Enc.t Rocq_ind.t

    val get_lts : t -> Rocq_ind.LTS.t
    val get_lts_term_type : t -> Evd.econstr
    val get_lts_label_type : t -> Evd.econstr
    val get_lts_constructor_types : t -> Rocq_ind.LTS.constructor array
    val to_string : Environ.env -> Evd.evar_map -> t -> string
    val lookup : Names.inductive -> Declarations.mind_specif mm

    val assert_mip_arity_is_type_or_set
      :  Declarations.one_inductive_body
      -> unit mm

    val assert_mip_arity_is_prop : Declarations.one_inductive_body -> unit mm

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
    -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list
    -> EConstr.Vars.substl mm

  val extract_args
    :  ?substl:EConstr.Vars.substl
    -> Constr.t
    -> Rocq_utils.constructor_args mm

  module Constructor : sig
    include module type of Enc_constructor_tree.Make (Log) (Enc) (Tree)

    val encode : Evd.econstr -> Evd.econstr -> Tree.t -> t
  end

  val make_state_tree_pair_set
    :  unit
    -> (module Set.S with type elt = Enc.t * Tree.t)

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

    module Constructors : sig
      type t = Constructor.t list
      type k = (Enc.t * Enc.t * Tree.t) list

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit

      val retrieve
        :  int
        -> t
        -> Evd.econstr
        -> Evd.econstr
        -> Enc.t * Problems.t list
        -> t mm
    end

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
      -> Ind.t F.t
      -> Evd.econstr
      -> Evd.econstr
      -> Enc.t
      -> Constructors.t mm

    val explore_valid_constructor
      :  Ind.t F.t
      -> Evd.econstr
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
      -> Evd.econstr
      -> Evd.econstr
      -> Constructors.t
      -> (Enc.t * Problems.t list) option
      -> Constructors.t mm

    val collect_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Ind.t F.t
      -> Evd.econstr
      -> Evd.econstr
      -> Enc.t
      -> Constructors.t mm
  end

  val make_hashtbl : unit -> (module Hashtbl.S with type key = Enc.t)
  val make_set : unit -> (module Set.S with type elt = Enc.t)
  val make_econstr_set : unit -> (module Set.S with type elt = EConstr.t)
end
