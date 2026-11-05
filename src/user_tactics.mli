(** The [bisim] and [sim] tactics: prove a [weak_bisimilar] or [weak_sim]
    goal between closed terms, with no setup.

    {[
      Set MeBi MaxStates 2000.
      Lemma pq : weak_bisimilar compLTS compLTS p q.
      Proof. bisim. Qed.
    ]}

    Each reads from its goal what [MeBi Sim Begin] is told by hand: the two
    LTSs, the two terms, the LTSs their constructors' premises use (the
    [Using] list), and the label type, whose [None] is the silent step.
    Then it runs the same proof search as [MeBi Sim Begin] followed by
    [MeBi Sim Solve], with the step bound [Set MeBi MaxSteps] gives
    ({!Api.the_max_steps}), and closes the goal with the proof found.
    Weak labels set with [MeBi Config Weak] are respected.

    Forms, after Rocq's [auto n] and [debug auto]:
    - [bisim], [sim]: at most [MaxSteps + 1] steps; no proof is an error;
    - [bisim n], [sim n]: at most [n] steps ([0]: none, only the setup);
      no proof leaves the goal unchanged, with a notice, as [auto n] does;
    - [debug bisim [n]], [debug sim [n]]: as above, and on stopping without
      a proof the search's open goals are printed. *)

type kind =
  | Bisim (** [bisim]: a [weak_bisimilar] goal *)
  | Sim (** [sim]: a [weak_sim] goal *)

(** [prove ?steps ?debug kind] is the tactic [bisim] or [sim], with [n]
    ([steps]) and the [debug] prefix as above.

    @raise CErrors.UserError
      when run, if the goal is not of its kind (naming the other tactic if
      it is of that one), not between closed terms, or over an LTS that is
      not an inductive [M -> option A -> M -> Prop] without parameters;
      and if no proof is found within [MaxSteps] when [steps] is not given
      (raised here); and
      whatever [MeBi Sim Begin] and [MeBi Sim Solve] raise (propagated). *)
val prove : ?steps:int -> ?debug:bool -> kind -> unit Proofview.tactic
