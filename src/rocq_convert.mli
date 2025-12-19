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
