type hyp =
  ( Evd.econstr,
    Evd.econstr,
    Evd.erelevance )
  Context.Named.Declaration.pt

type constr_kind =
  ( Evd.econstr,
    Evd.econstr,
    Evd.esorts,
    EConstr.EInstance.t,
    Evd.erelevance )
  Constr.kind_of_term

type ind_constr = Constr.rel_context * Constr.t
type ind_constrs = ind_constr array
type econstr_decl = EConstr.rel_declaration
type econstr_decls = econstr_decl list

val type_of_econstr_rel :
  ?substl:Evd.econstr list ->
  EConstr.rel_declaration ->
  Evd.econstr

val type_of_econstr :
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  Evd.evar_map * Evd.econstr

module Strfy : sig
  val pp : ?clean:bool -> Pp.t -> string
  val evar : Evar.t -> string
  val evar' : Environ.env -> Evd.evar_map -> Evar.t -> string

  val constr :
    Environ.env -> Evd.evar_map -> Constr.t -> string

  val constr_opt :
    Environ.env -> Evd.evar_map -> Constr.t option -> string

  val constr_rel_decl :
    Environ.env ->
    Evd.evar_map ->
    Constr.rel_declaration ->
    string

  val constr_rel_context :
    Environ.env -> Evd.evar_map -> Constr.rel_context -> string

  val constr_kind :
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    Constr.t ->
    string

  val econstr :
    Environ.env -> Evd.evar_map -> Evd.econstr -> string

  val econstr_rel_decl :
    Environ.env -> Evd.evar_map -> econstr_decl -> string

  val econstr_types :
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    string

  val econstr_kind :
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    string

  val name_id : Names.variable -> string
  val global : Names.GlobRef.t -> string

  val concl :
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    string

  val erel : 'a -> Evd.evar_map -> Evd.erelevance -> string

  val hyp :
    ?force_newline:bool ->
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    hyp ->
    string

  val goal : ?indent:int -> Proofview.Goal.t -> string
end

type cache = {
  the_prev : Names.Id.Set.t;
  the_next : Names.variable;
}

val the_cache : cache option ref
val the_default_next : unit -> Names.variable
val the_prev : unit -> Names.Id.Set.t
val the_next : unit -> Names.variable

exception CouldNotGetNextFreshEvarName of unit

val get_next :
  Environ.env ->
  Evd.evar_map ->
  Evd.econstr ->
  Evd.evar_map * Evd.econstr
