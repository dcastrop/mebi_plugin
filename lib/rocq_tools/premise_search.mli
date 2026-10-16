(** Bounded proof search for constructor premises that are not over an LTS
    (backlog item I2, stage 1). Shared by extraction, which needs the verdict,
    and the proof solver, which uses the proof. *)

(** How a premise was proved: a closed proof term, or -- for a negation
    [~ P] -- the refutation of [P], replayed by {!negation_tac}, or -- for a
    bounded universal [forall k, k < n -> P k] -- each instance [P i], the
    proof built by {!premise_tac}. *)
type proof =
  | Term of EConstr.t
  | ByRefutation of EConstr.t
  | ByCases

(** What {!prove} concluded about a premise. Never a guess: [Refuted] only
    after a complete search. *)
type result =
  | Proved of proof (** the premise holds *)
  | Refuted (** no proof exists: the search was complete *)
  | Unknown
  (** not decided: not closed, not an inductive proposition, an
      opaque or non-ground argument, or the depth bound was hit *)

(** The default for {!max_depth}: 16. *)
val default_depth : int

(** [is_prop env sigma t] is whether [t] is a proposition (its sort is
    [Prop]); [false] if its sort cannot be computed. Raises nothing. *)
val is_prop : Environ.env -> Evd.evar_map -> EConstr.t -> bool

(** The most nested constructor applications a search may try; set by
    [MeBi Config Premise Depth]. *)
val max_depth : int ref

(** [prove env sigma goal] is what a bounded proof search concludes about
    the closed proposition [goal]: an inductive proposition (after head
    reduction, so [lt] and a computing [In] unfold), an equation, a
    negation (holding iff its body is refuted), or a bounded universal
    (decided instance by instance; {!is_bounded_universal}); failing those,
    the {!user_tactic} is tried. [Unknown] for an open [goal]. It never
    guesses: [Refuted] only when every failed match was on ground
    constructor terms and no branch was cut short by {!max_depth}.

    Raises nothing. *)
val prove : Environ.env -> Evd.evar_map -> EConstr.t -> result

(** A user tactic tried on premises the search leaves undecided: proving
    [P] means it holds, proving [~ P] that it is false. Set by [MeBi Config Premise Tactic].
*)
val user_tactic : unit Proofview.tactic option ref

(** [refute_hyp_tac ?depth id] is a tactic that closes the goal from the
    hypothesis [id], which cannot hold: a closed premise {!prove} refutes,
    an open one that is {!dead}, a negation whose body is proved, or a
    bounded universal with a refuted instance. An inductive hypothesis is
    unfolded and inverted, and every goal that leaves refuted the same way,
    to at most [depth] nested inversions (default {!max_depth}).

    Raises nothing; fails (as a tactic) when [id] cannot be refuted this
    way within [depth]. *)
val refute_hyp_tac : ?depth:int -> Names.Id.t -> unit Proofview.tactic

(** [negation_tac] is a tactic that proves a goal [~ P] whose [P] {!prove}
    refutes: [intro], then {!refute_hyp_tac} on the new hypothesis.

    Raises nothing; fails (as a tactic) when [P] cannot be refuted. *)
val negation_tac : unit Proofview.tactic

(** The default for {!max_range}: 256. *)
val default_range : int

(** The most values of [k] a bounded universal may range over to be
    decided ([MeBi Config Premise Range]). *)
val max_range : int ref

(** [is_bounded_universal env sigma t] is whether [t] is
    [forall k, k < n -> P k] or [forall k, k <= n -> P k] over [nat], with
    [n] a numeral once normalised and at most {!max_range} values of [k]:
    the premises {!prove} decides instance by instance. Raises nothing. *)
val is_bounded_universal : Environ.env -> Evd.evar_map -> EConstr.t -> bool

(** [above_range env sigma t] is whether [t] has that shape but ranges over
    more than {!max_range} values, and so is left undecided. Raises
    nothing. *)
val above_range : Environ.env -> Evd.evar_map -> EConstr.t -> bool

(** [premise_tac ()] is a tactic that proves the closed premise in focus
    the way {!prove} decided it holds: by its proof term, as a negation, or
    instance by instance for a bounded universal (which needs
    [MEBI.Premises] loaded).

    Raises nothing; fails (as a tactic) when {!prove} does not find the
    premise true, or a bounded universal's lemmas or an instance's proof
    are missing. *)
val premise_tac : unit -> unit Proofview.tactic

(** [enumerate env sigma goal] is every way to make the premise [goal]
    hold, each an evar map instantiating its open variables (a target still
    to be computed, say), and whether those are all of them. A closed
    premise: one map if it holds, none if it is refuted.

    Raises nothing. *)
val enumerate
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> Evd.evar_map list * bool

(** [abstract_vars env sigma t] is [t] with each local variable it mentions
    replaced by a fresh evar of the same type, and the evar map with them.
    Raises nothing. *)
val abstract_vars
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> Evd.evar_map * EConstr.t

(** [dead env sigma t] is whether the proposition [t] holds for no value of
    the local variables it mentions: a complete search over
    {!abstract_vars} found no instance, so a hypothesis of type [t] is false
    in any context. Not for negations. Memoised by [t] (variables numbered
    by occurrence) and the depth.

    Raises nothing. *)
val dead : Environ.env -> Evd.evar_map -> EConstr.t -> bool
