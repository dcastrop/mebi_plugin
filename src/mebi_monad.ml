(* type term = EConstr.t

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  }

type 'a in_coq_context =
  { state : coq_context ref
  ; value : 'a
  }

type 'a cm = coq_context ref -> 'a in_coq_context

let coq_env_wrapper : Environ.env option ref = ref None

let the_coq_env : Environ.env ref =
  match !coq_env_wrapper with
  | None ->
    let env = Global.env () in
    coq_env_wrapper := Some env;
    ref env
  | Some env -> ref env
;;

let the_coq_ctx : Evd.evar_map ref = ref (Evd.from_env !the_coq_env)

let init : coq_context ref =
  let env = !the_coq_env in
  let sigma = !the_coq_ctx in
  let coq_ref : coq_context ref = ref { coq_env = env; coq_ctx = sigma } in
  coq_ref
;; *)
