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

let init : coq_context ref =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let coq_ref : coq_context ref = ref { coq_env = env; coq_ctx = sigma } in
  coq_ref
;;
