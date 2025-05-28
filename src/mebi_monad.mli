type term = EConstr.t

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  }

type 'a in_coq_context =
  { state : coq_context ref
  ; value : 'a
  }

type 'a cm = coq_context ref -> 'a in_coq_context

val init : coq_context ref
