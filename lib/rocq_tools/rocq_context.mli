type t =
  { env : Environ.env
  ; sigma : Evd.evar_map
  }

(** Where an [env]/[sigma] pair is read from. A value, not a module: switching
    between the global environment and a proof goal must not require
    re-applying the monad/encoding functor stack. Each call pulls the current
    state rather than caching it. *)
type source = unit -> t

(** The global environment, with a fresh [sigma] derived from it. Used by the
    [MeBi ...] commands. *)
val global : source

(** The environment and [sigma] of a proof goal. Used by [mebi_solve] steps;
    reads through the ref, so it tracks the goal as the proof advances. *)
val of_goal : Proofview.Goal.t ref -> source

val env : source -> Environ.env
val sigma : source -> Evd.evar_map
