type 'a kind_pair = 'a * 'a array

exception
  Rocq_utils_EConstrIsNot_Atomic of
    (Evd.evar_map * Evd.econstr * EConstr.kind_of_type)

exception Rocq_utils_EConstrIsNotA_Type of (Evd.evar_map * Evd.econstr * string)

val econstr_to_atomic : Evd.evar_map -> Evd.econstr -> Evd.econstr kind_pair

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
  ( Evd.econstr
    , Evd.econstr
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

exception
  Rocq_utils_EConstrIsNot_App of (Evd.evar_map * Evd.econstr * econstr_kind)

val econstr_to_app : Evd.evar_map -> Evd.econstr -> Evd.econstr kind_pair

type lambda_triple =
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot
  * Evd.econstr
  * Evd.econstr

exception
  Rocq_utils_EConstrIsNot_Lambda of (Evd.evar_map * Evd.econstr * econstr_kind)

val econstr_to_lambda : Evd.evar_map -> Evd.econstr -> lambda_triple

type hyp =
  (Evd.econstr, Evd.econstr, Evd.erelevance) Context.Named.Declaration.pt

exception
  Rocq_utils_HypIsNot_Atomic of (Evd.evar_map * hyp * EConstr.kind_of_type)

val hyp_to_atomic : Evd.evar_map -> hyp -> Evd.econstr kind_pair

type ind_constr = Constr.rel_context * Constr.t
type constr_decl = Constr.rel_declaration
type econstr_decl = EConstr.rel_declaration

val get_econstr_decls : Constr.rel_context -> econstr_decl list
val list_of_constr_kinds : Constr.t -> (string * bool) list
val list_of_econstr_kinds : Evd.evar_map -> Evd.econstr -> (string * bool) list

val list_of_econstr_kinds_of_type
  :  Evd.evar_map
  -> Evd.econstr
  -> (string * bool) list

val list_of_kinds
  :  Evd.evar_map
  -> (Evd.evar_map -> 'a -> (string * bool) list)
  -> 'a
  -> string list

val get_decl_type_of_constr : constr_decl -> Evd.econstr
val get_decl_type_of_econstr : econstr_decl -> Evd.econstr

val get_ind_ty
  :  Names.inductive
  -> Declarations.mutual_inductive_body
  -> Evd.econstr

val type_of_econstr_rel
  :  ?substl:Evd.econstr list
  -> econstr_decl
  -> Evd.econstr

val type_of_econstr
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.evar_map * Evd.econstr

module Strfy : sig
  val pp : ?clean:bool -> ?args:Utils.Strfy.style_args -> Pp.t -> string
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
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Constr.t
    -> string

  val econstr : Environ.env -> Evd.evar_map -> Evd.econstr -> string

  val feconstr
    :  Environ.env
    -> Evd.evar_map
    -> Evd.econstr Utils.Strfy.to_string

  val econstr_rel_decl : Environ.env -> Evd.evar_map -> econstr_decl -> string

  val econstr_type
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> string * Evd.econstr * Evd.econstr * Evd.econstr array
    -> string

  val econstr_types
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Evd.econstr
    -> string

  val econstr_kind
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Evd.econstr
    -> string

  val name_id : Names.variable -> string
  val global : Names.GlobRef.t -> string

  val concl
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Evd.econstr
    -> string

  val erel : 'a -> Evd.evar_map -> Evd.erelevance -> string
  val hyp_name : hyp -> string
  val hyp_value : Environ.env -> Evd.evar_map -> hyp -> string

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
  -> Evd.econstr
  -> Evd.evar_map * Evd.econstr

type evar_source =
  | TypeOf of Evd.econstr
  | OfType of Evd.econstr

val get_next
  :  Environ.env
  -> Evd.evar_map
  -> evar_source
  -> Evd.evar_map * Evd.econstr

val get_fresh_evar
  :  Environ.env
  -> Evd.evar_map
  -> evar_source
  -> Evd.evar_map * Evd.econstr

val subst_of_decl
  :  EConstr.Vars.substl
  -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt
  -> Evd.econstr

val mk_ctx_subst
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.Vars.substl
  -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt
  -> Evd.evar_map * Evd.econstr

val mk_ctx_substl
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.Vars.substl
  -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list
  -> Evd.evar_map * EConstr.Vars.substl

val map_decl_evar_pairs
  :  econstr_decl list
  -> EConstr.Vars.substl
  -> (Evd.econstr * Names.Name.t) list

exception ConstructorArgsExpectsArraySize3 of unit

type constructor_args =
  { lhs : Evd.econstr
  ; act : Evd.econstr
  ; rhs : Evd.econstr
  }

val constructor_args : Evd.econstr array -> constructor_args

exception Rocq_utils_InvalidLtsArgLength of int
exception Rocq_utils_InvalidLtsTermKind of Constr.t

val extract_args : ?substl:EConstr.Vars.substl -> Constr.t -> constructor_args

exception Rocq_utils_CouldNotExtractBinding of unit

val unpack_constr_args : Constr.t kind_pair -> Constr.t * Constr.t * Constr.t
