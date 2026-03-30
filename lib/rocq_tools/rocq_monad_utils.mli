module type S = sig
  type tree

  include Rocq_monad.S

  val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm
  val econstr_eq : ?enc:bool -> EConstr.t -> EConstr.t -> bool mm
  val econstr_compare : EConstr.t -> EConstr.t -> int
  val get_encoding : EConstr.t -> enc
  val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm
  val econstr_is_evar : EConstr.t -> bool mm

  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> EConstr.t
    -> Constr.t mm

  val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
  val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm
  val to_atomic : EConstr.t -> EConstr.t Rocq_utils.kind_pair mm
  val to_lambda : EConstr.t -> Rocq_utils.lambda_triple mm
  val to_app : EConstr.t -> EConstr.t Rocq_utils.kind_pair mm
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

    (* val rocq_ind : ('a -> string) -> 'a Rocq_ind.t -> string *)
    val econstr_bindings : EConstr.t Tactypes.bindings -> string
  end

  val log_econstr
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> EConstr.t
    -> unit

  val log_econstrs
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> EConstr.t list
    -> unit

  val log_constr
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> Constr.t
    -> unit

  val log_constrs
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> Constr.t list
    -> unit

  module type SErrors = sig
    type t =
      | LTS_Empty
      | LTS_Incomplete of string
      | Not_Bisimilar
      | Invalid_Ind_Kind_Type of EConstr.t option
      | Invalid_Sort_LTS of Sorts.Quality.t
      | Invalid_Sort_Type of Sorts.Quality.t
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
    val lts_incomplete : string -> exn
    val not_bisimilar : unit -> exn
    val invalid_ind_kind_type : EConstr.t option -> exn
    val invalid_sort_lts : Sorts.Quality.t -> exn
    val invalid_sort_type : Sorts.Quality.t -> exn
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
    val lts_incomplete : string -> 'a
    val not_bisimilar : unit -> 'a
    val invalid_ind_kind_type : EConstr.t option -> 'a
    val invalid_sort_lts : Sorts.Quality.t -> 'a
    val invalid_sort_type : Sorts.Quality.t -> 'a
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
    module LTS : sig
      type t =
        { term_type : EConstr.t
        ; label_type : EConstr.t
        ; constructor_types : constructor array
        }

      and constructor =
        { name : Names.Id.t
        ; constructor : Rocq_utils.ind_constr
        }

      include Json.S with type k = t (** @closed *)
    end

    type t =
      { enc : enc
      ; ind : EConstr.t
      ; kind : kind
      }

    and kind =
      | Type of EConstr.t option
      | LTS of LTS.t

    include Json.S with type k = t (** @closed *)

    val get_lts : t -> LTS.t
    val get_lts_term_type : t -> EConstr.t
    val get_lts_label_type : t -> EConstr.t
    val get_lts_constructor_types : t -> LTS.constructor array
    val lookup : Names.inductive -> Declarations.mind_specif mm
    val get_lts_constructor_names : t -> Names.Id.t array
    val get_lts_constructors : t -> Rocq_utils.ind_constr array

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
    -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
    -> EConstr.Vars.substl mm

  val extract_args
    :  ?substl:EConstr.Vars.substl
    -> Constr.t
    -> Rocq_utils.constructor_args mm

  module Constructor : sig
    type t = enc * enc * tree

    include Json.S with type k = t (** @closed *)

    val encode : EConstr.t -> EConstr.t -> tree -> t
  end

  val make_state_tree_pair_set
    :  unit
    -> (module Set.S with type elt = enc * tree)

  module Unification : sig
    module Pair : sig
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      include Json.S with type k = t (** @closed *)

      val fresh : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * t

      val make
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t
        -> EConstr.t
        -> Evd.evar_map * t

      val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      val unifies : EConstr.t -> EConstr.t -> bool mm
    end

    module Problem : sig
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : tree
        }

      include Json.S with type k = t (** @closed *)

      val unify_pair_opt : Pair.t -> bool mm
      val unify_opt : t -> tree option mm
      val of_constructor : Rocq_utils.constructor_args -> Constructor.t -> t
    end

    module Problems : sig
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      include Json.S with type k = t (** @closed *)

      val empty : unit -> t mm
      val is_empty : t -> bool
      val unify_list_opt : Problem.t list -> tree list option mm

      val sandbox_unify_all_opt
        :  EConstr.t
        -> EConstr.t
        -> t
        -> (EConstr.t * EConstr.t * tree list) option mm
    end

    module ListOfProblems : sig
      type t = Problems.t list

      include Json.S with type k = t (** @closed *)

      val is_empty : t -> bool
      val cross_product : Problems.t -> t -> t
    end

    module Constructors : sig
      type t = Constructor.t list

      include Json.S with type k = t (** @closed *)

      val retrieve
        :  int
        -> t
        -> EConstr.t
        -> EConstr.t
        -> enc * ListOfProblems.t
        -> t mm

      val to_problems : Rocq_utils.constructor_args -> t -> Problems.t mm
      val axiom : EConstr.t -> EConstr.t -> enc * int -> t -> t mm
    end

    val check_constructor_args_unify
      :  EConstr.t
      -> EConstr.t
      -> Rocq_utils.constructor_args
      -> bool mm

    val check_valid_constructors
      :  Ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> enc
      -> Constructors.t mm

    val explore_valid_constructor
      :  Ind.t F.t
      -> EConstr.t
      -> enc
      -> Rocq_utils.constructor_args
      -> int * Constructors.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> Constructors.t mm

    val check_updated_ctx
      :  enc
      -> ListOfProblems.t
      -> Ind.t F.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> (enc * ListOfProblems.t) option mm

    val check_for_next_constructors
      :  int
      -> EConstr.t
      -> EConstr.t
      -> Constructors.t
      -> (enc * ListOfProblems.t) option
      -> Constructors.t mm

    val collect_valid_constructors
      :  Ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> enc
      -> Constructors.t mm
  end

  val make_enc_hashtbl : unit -> (module Hashtbl.S with type key = enc)
  val make_enc_set : unit -> (module Set.S with type elt = enc)
  val make_econstr_set : unit -> (module Set.S with type elt = EConstr.t)
end

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  S with module Ctx = Ctx and type enc = Enc.t and type tree = Enc.Tree.t
