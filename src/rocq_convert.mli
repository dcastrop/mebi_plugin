
  val constrexpr_to_econstr :
    Environ.env ->
    Evd.evar_map ->
    Constrexpr.constr_expr ->
    Evd.evar_map * Evd.econstr

  val econstr_to_constr :
    ?abort_on_undefined_evars:bool ->
    Evd.evar_map ->
    Evd.econstr ->
    Constr.t

  val econstr_to_constr_opt :
    Evd.evar_map -> Evd.econstr -> Constr.t option

  val globref_to_econstr :
    Environ.env -> Names.GlobRef.t -> Evd.econstr