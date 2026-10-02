(** Bounded proof search for constructor premises that are not over an LTS
    (backlog item I2, stage 1). Shared by extraction, which needs the verdict,
    and the proof solver, which uses the proof. *)

type result =
  | Proved of EConstr.t (** a closed proof term of the premise *)
  | Refuted (** no proof exists: the search was complete *)
  | Unknown
  (** not decided: not closed, not an inductive proposition, an
      opaque or non-ground argument, or the depth bound was hit *)

val default_depth : int

(** [is_prop env sigma t]: [t] is a proposition (its sort is [Prop]). *)
val is_prop : Environ.env -> Evd.evar_map -> EConstr.t -> bool

(** The most nested constructor applications a search may try; set by
    [MeBi Config Premise Depth]. *)
val max_depth : int ref

(** [prove env sigma goal] searches for a proof of the closed proposition
    [goal]: an inductive proposition (after head reduction, so [lt] and a
    computing [In] unfold) or an equation. Never guesses: [Refuted] only when
    every match that failed was on ground constructor terms and no branch was
    cut short. *)
val prove : Environ.env -> Evd.evar_map -> EConstr.t -> result
