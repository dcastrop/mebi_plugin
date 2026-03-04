type 'a kind_pair = 'a * 'a array

exception
  Rocq_utils_EConstrIsNot_Atomic of
    (Evd.evar_map * EConstr.t * EConstr.kind_of_type)

exception Rocq_utils_EConstrIsNotA_Type of (Evd.evar_map * EConstr.t * string)

val econstr_to_atomic : Evd.evar_map -> EConstr.t -> EConstr.t kind_pair

type constr_kind =
  ( Constr.t
    , Constr.t
    , Sorts.t
    , UVars.Instance.t
    , Sorts.relevance )
    Constr.kind_of_term

exception Rocq_utils_ConstrIsNot_App of (Constr.t * constr_kind)

val constr_to_app : Constr.t -> Constr.t kind_pair

type econstr_kind =
  ( EConstr.t
    , EConstr.t
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

exception
  Rocq_utils_EConstrIsNot_App of (Evd.evar_map * EConstr.t * econstr_kind)

val econstr_to_app : Evd.evar_map -> EConstr.t -> EConstr.t kind_pair

type lambda_triple =
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot * EConstr.t * EConstr.t

exception
  Rocq_utils_EConstrIsNot_Lambda of (Evd.evar_map * EConstr.t * econstr_kind)

val econstr_to_lambda : Evd.evar_map -> EConstr.t -> lambda_triple

type hyp = (EConstr.t, EConstr.t, Evd.erelevance) Context.Named.Declaration.pt

exception
  Rocq_utils_HypIsNot_Atomic of (Evd.evar_map * hyp * EConstr.kind_of_type)

val hyp_to_atomic : Evd.evar_map -> hyp -> EConstr.t kind_pair

type ind_constr = Constr.rel_context * Constr.t
type constr_decl = Constr.rel_declaration
type econstr_decl = EConstr.rel_declaration

val get_econstr_decls : Constr.rel_context -> econstr_decl list
val list_of_constr_kinds : Constr.t -> (string * bool) list
val list_of_econstr_kinds : Evd.evar_map -> EConstr.t -> (string * bool) list

val list_of_econstr_kinds_of_type
  :  Evd.evar_map
  -> EConstr.t
  -> (string * bool) list

val list_of_kinds
  :  Evd.evar_map
  -> (Evd.evar_map -> 'a -> (string * bool) list)
  -> 'a
  -> string list

val get_decl_type_of_constr : constr_decl -> EConstr.t
val get_decl_type_of_econstr : econstr_decl -> EConstr.t

val get_ind_ty
  :  Names.inductive
  -> Declarations.mutual_inductive_body
  -> EConstr.t

val type_of_econstr_rel : ?substl:EConstr.t list -> econstr_decl -> EConstr.t

val type_of_econstr
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> Evd.evar_map * EConstr.t

module Strfy : sig
  val pp : ?clean:bool -> ?args:Utils.Strfy.style_args -> Pp.t -> string
  val name_id : Names.variable -> string
  val global : Names.GlobRef.t -> string
  val evar : ?args:Utils.Strfy.style_args -> Evar.t -> string

  val evar'
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Evar.t
    -> string

  val constr : Environ.env -> Evd.evar_map -> Constr.t -> string

  val constr_opt
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Constr.t option
    -> string

  val constr_rel_decl
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> constr_decl
    -> string

  val constr_rel_context
    :  Environ.env
    -> Evd.evar_map
    -> Constr.rel_context
    -> string

  val ind_constr : Environ.env -> Evd.evar_map -> ind_constr -> string
  val ind_constrs : Environ.env -> Evd.evar_map -> ind_constr array -> string

  val constr_kind
    :  Environ.env
    -> Evd.evar_map (* -> ?args:Utils.Strfy.style_args *)
    -> Constr.t
    -> string

  val econstr : Environ.env -> Evd.evar_map -> EConstr.t -> string
  val feconstr : Environ.env -> Evd.evar_map -> EConstr.t Utils.Strfy.to_string
  val econstr_rel_decl : Environ.env -> Evd.evar_map -> econstr_decl -> string

  val econstr_type
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> string * EConstr.t * EConstr.t * EConstr.t array
    -> string

  val econstr_types
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> EConstr.t
    -> string

  val econstr_kind
    :  Environ.env
    -> Evd.evar_map
    -> (* ?args:Utils.Strfy.style_args -> *)
       EConstr.t
    -> string

  val concl
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> EConstr.t
    -> string

  val erel : 'a -> Evd.evar_map -> Evd.erelevance -> string
  val hyp_name : hyp -> string
  val hyp_value : Environ.env -> Evd.evar_map -> hyp -> string
  val hyp_type : Environ.env -> Evd.evar_map -> hyp -> string

  val hyp
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> hyp
    -> string

  val goal : ?args:Utils.Strfy.style_args -> Proofview.Goal.t -> string
end

type cache =
  { the_prev : Names.Id.Set.t
  ; the_next : Names.variable
  }

val the_cache : cache option ref
val the_default_next : unit -> Names.variable
val the_prev : unit -> Names.Id.Set.t
val the_next : unit -> Names.variable

exception CouldNotGetNextFreshEvarName of unit

val get_next_evar
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> Evd.evar_map * EConstr.t

type evar_source =
  | TypeOf of EConstr.t
  | OfType of EConstr.t

val get_next
  :  Environ.env
  -> Evd.evar_map
  -> evar_source
  -> Evd.evar_map * EConstr.t

val get_fresh_evar
  :  Environ.env
  -> Evd.evar_map
  -> evar_source
  -> Evd.evar_map * EConstr.t

val subst_of_decl
  :  EConstr.Vars.substl
  -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt
  -> EConstr.t

val mk_ctx_subst
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.Vars.substl
  -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt
  -> Evd.evar_map * EConstr.t

val mk_ctx_substl
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.Vars.substl
  -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
  -> Evd.evar_map * EConstr.Vars.substl

val map_decl_evar_pairs
  :  econstr_decl list
  -> EConstr.Vars.substl
  -> (EConstr.t * Names.Name.t) list

exception ConstructorArgsExpectsArraySize3 of unit

type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

val constructor_args : EConstr.t array -> constructor_args

exception Rocq_utils_InvalidLtsArgLength of int
exception Rocq_utils_InvalidLtsTermKind of Constr.t

val extract_args : ?substl:EConstr.Vars.substl -> Constr.t -> constructor_args

exception Rocq_utils_CouldNotExtractBinding of unit

val unpack_constr_args : Constr.t kind_pair -> Constr.t * Constr.t * Constr.t

val econstr_to_constrexpr
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> Constrexpr.constr_expr

val constrexpr_to_econstr
  :  Environ.env
  -> Evd.evar_map
  -> Constrexpr.constr_expr
  -> Evd.evar_map * EConstr.t

val econstr_to_constr
  :  ?abort_on_undefined_evars:bool
  -> Evd.evar_map
  -> EConstr.t
  -> Constr.t

val econstr_to_constr_opt : Evd.evar_map -> EConstr.t -> Constr.t option
val globref_to_econstr : Environ.env -> Names.GlobRef.t -> EConstr.t
val is_constant : Evd.evar_map -> EConstr.t -> (unit -> EConstr.t) -> bool
val is_theory : Evd.evar_map -> EConstr.t -> bool
val libnames_to_globrefs : Libnames.qualid list -> Names.GlobRef.t list
