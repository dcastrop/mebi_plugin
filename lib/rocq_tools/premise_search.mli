(** Bounded proof search for constructor premises that are not over an LTS
    (backlog item I2, stage 1). Shared by extraction, which needs the verdict,
    and the proof solver, which uses the proof. *)

(** How a premise was proved: a closed proof term, or -- for a negation
    [~ P] -- the refutation of [P], replayed by {!negation_tac}. *)
type proof =
  | Term of EConstr.t
  | ByRefutation of EConstr.t

type result =
  | Proved of proof (** the premise holds *)
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

(** A user tactic tried on premises the search leaves undecided: proving
    [P] means it holds, proving [~ P] that it is false. Set by [MeBi Config Premise Tactic].
*)
val user_tactic : unit Proofview.tactic option ref

(** Close the goal from hypothesis [id], a closed premise [prove] refutes. *)
val refute_hyp_tac : ?depth:int -> Names.Id.t -> unit Proofview.tactic

(** Prove a goal [~ P] whose [P] [prove] refutes. *)
val negation_tac : unit Proofview.tactic

(** [enumerate env sigma goal]: for a premise that may mention open
    variables (a target still to be computed, say), every way to make it
    hold -- each an evar map instantiating them -- and whether those are all
    of them. Closed premises: one map if it holds, none if refuted. *)
val enumerate
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> Evd.evar_map list * bool
