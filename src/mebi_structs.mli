type coq_lts =
  { env : Environ.env
  ; sigma : Evd.evar_map
  ; lts_type : Evd.econstr
  ; term_type : Evd.econstr
  ; type_lbls : Evd.econstr
  ; constr_names : Names.variable array
  ; constr_transitions : (Constr.rel_context * Constr.t) array
  ; start_term : Evd.econstr
  ; states : Evd.econstr list
  ; edges : Evd.econstr list
  }

val coq_lts
  :  ?states:Evd.econstr list
  -> ?edges:Evd.econstr list
  -> Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.econstr
  -> Evd.econstr
  -> Names.variable array
  -> (Constr.rel_context * Constr.t) array
  -> Evd.econstr
  -> coq_lts

val coq_lts_update_sigma : coq_lts -> Evd.evar_map -> coq_lts
val coq_lts_update_states : coq_lts -> Evd.econstr list -> coq_lts
val coq_lts_update_edges : coq_lts -> Evd.econstr list -> coq_lts
