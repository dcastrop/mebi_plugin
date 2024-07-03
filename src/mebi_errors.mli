val invalid_sort : Sorts.family -> exn
val invalid_arity : Environ.env -> Evd.evar_map -> Constr.types -> exn
val invalid_ref : Names.GlobRef.t -> exn
