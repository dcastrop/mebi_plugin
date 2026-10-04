type t =
  { env : Environ.env
  ; sigma : Evd.evar_map
  }

(** Where an [env]/[sigma] pair is read from: a function, called each time
    the pair is needed, so it pulls the current state rather than caching
    it. A value, not a module: switching between the global environment and
    a proof goal must not require re-applying the monad/encoding functor
    stack. *)
type source = unit -> t

(** [global] is the global environment, with a fresh [sigma] derived from
    it each time. Used by the [MeBi ...] commands. Raises nothing. *)
val global : source

(** [of_goal gl] is the environment and [sigma] of the proof goal in [gl],
    read through the ref, so it tracks the goal as the proof advances. Used
    by [mebi_solve] steps. Raises nothing. *)
val of_goal : Proofview.Goal.t ref -> source

(** [env s] is the environment [s] currently gives. Raises nothing. *)
val env : source -> Environ.env

(** [sigma s] is the evar map [s] currently gives. Raises nothing. *)
val sigma : source -> Evd.evar_map
