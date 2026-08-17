type t =
  { env : Environ.env
  ; sigma : Evd.evar_map
  }

(** Where an [env]/[sigma] pair is read from.

    This used to be a [module type S] threaded as a functor parameter through
    [Bi_encoding] -> [Rocq_monad] -> [Rocq_monad_utils] -> [Wrapper] /
    [Results] / [Proof_solver], to serve three call sites. Because it was a
    module, switching between the two contexts the plugin encounters -- the
    global environment for a [MeBi ...] command, and a proof goal for a
    [mebi_solve] step -- meant re-applying that whole stack, which also
    re-created [Bi_encoding]'s encoding table each time. As a value the two are
    just two [source]s.

    Note this is a {i pull}: each call reads the current state rather than
    caching it. The previous [S.update] could never have worked -- [Make.get]
    allocated a fresh [ref] per call, so [update] wrote into a value that was
    immediately discarded -- and nothing called it, so it is gone. *)
type source = unit -> t

let global : source =
  fun () ->
  let env : Environ.env = Global.env () in
  { env; sigma = Evd.from_env env }
;;

let of_goal (gl : Proofview.Goal.t ref) : source =
  fun () -> { env = Proofview.Goal.env !gl; sigma = Proofview.Goal.sigma !gl }
;;

let env (s : source) : Environ.env = (s ()).env
let sigma (s : source) : Evd.evar_map = (s ()).sigma
