val invalid_sort : Sorts.family -> exn
val invalid_arity : Environ.env -> Evd.evar_map -> Constr.types -> exn
val invalid_ref : Names.GlobRef.t -> exn

val unknown_term_type
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t * EConstr.t * EConstr.t list
  -> exn

val primary_lts_not_found
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.econstr list
  -> exn
