
val collect_valid_constructors :
  (Constr.rel_context * Constr.t) array ->
  Mebi_ind.t Mebi_wrapper.F.t ->
  Evd.econstr ->
  Evd.econstr ->
  Mebi_setup.Enc.t ->
  Mebi_unification.Constructors.t Mebi_wrapper.mm
