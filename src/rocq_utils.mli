type kind_pair = Evd.econstr * Evd.econstr array

exception
  Rocq_utils_EConstrIsNot_Atomic of
    (Evd.evar_map * Evd.econstr * EConstr.kind_of_type)

exception Rocq_utils_EConstrIsNotA_Type of (Evd.evar_map * Evd.econstr * string)

val econstr_to_atomic : Evd.evar_map -> Evd.econstr -> kind_pair

type constr_kind =
  ( Evd.econstr
    , Evd.econstr
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

exception
  Rocq_utils_EConstrIsNot_App of (Evd.evar_map * Evd.econstr * constr_kind)

val econstr_to_app : Evd.evar_map -> Evd.econstr -> kind_pair

type lambda_triple =
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot
  * Evd.econstr
  * Evd.econstr

exception
  Rocq_utils_EConstrIsNot_Lambda of (Evd.evar_map * Evd.econstr * constr_kind)

val econstr_to_lambda : Evd.evar_map -> Evd.econstr -> lambda_triple

type hyp =
  (Evd.econstr, Evd.econstr, Evd.erelevance) Context.Named.Declaration.pt

exception
  Rocq_utils_HypIsNot_Atomic of (Evd.evar_map * hyp * EConstr.kind_of_type)

val hyp_to_atomic : Evd.evar_map -> hyp -> kind_pair

type ind_constr = Constr.rel_context * Constr.t
type ind_constrs = ind_constr array
type econstr_decl = EConstr.rel_declaration
type econstr_decls = econstr_decl list

val list_of_constr_kinds : Constr.t -> (string * bool) list
val list_of_econstr_kinds : Evd.evar_map -> Evd.econstr -> (string * bool) list

val type_of_econstr_rel
  :  ?substl:Evd.econstr list
  -> EConstr.rel_declaration
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

  val constr
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Constr.t
    -> string

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
    -> Constr.rel_declaration
    -> string

  val constr_rel_context
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Constr.rel_context
    -> string

  val constr_kind
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Constr.t
    -> string

  val econstr
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Evd.econstr
    -> string

  val econstr_rel_decl
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> econstr_decl
    -> string

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

  val name_id : ?args:Utils.Strfy.style_args -> Names.variable -> string
  val global : ?args:Utils.Strfy.style_args -> Names.GlobRef.t -> string

  val concl
    :  Environ.env
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Evd.econstr
    -> string

  val erel
    :  'a
    -> Evd.evar_map
    -> ?args:Utils.Strfy.style_args
    -> Evd.erelevance
    -> string

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
