(** The simulation game's response choice, and the product it generates.

    This is the decision the proof solver makes at every step: given that the
    left-hand FSM has moved [from --label-> _], which state must the
    right-hand FSM move to? It used to live inside
    {!Mebi_plugin.Proof_solver_step}, tangled with the {b Rocq} goal it was
    read out of, which meant it could only ever be run one proof step at a
    time and could not be tested without a Rocq runtime.

    It is pure model code, so it lives here. {i See [ASSISTED-CHANGES.md],
    2026-09-29: an offline re-implementation of {!val:respond} that mirrored
    the original rather than sharing it got the right pairs but the wrong
    response in 8 of 16 cases, because the tie-breaks run through
    {!Components.S.Action.Pair.Set}'s own ordering. Everything that needs
    this decision must call this function, never reproduce it.} *)
module type S = sig
  (** See {!Model.S.State.t}. *)
  type state

  (** See {!Model.S.State.Set.t}. *)
  type states

  (** See {!Model.S.Label.t}. *)
  type label

  (** See {!Model.S.Transition.t}. *)
  type transition

  (** See {!Model.S.FSM.t}. *)
  type fsm

  (** Raised by {!val:respond} when no action out of the given state, under
      the given label, can reach any of the states it was asked to stay
      bisimilar to. *)
  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  (** [respond m from label bisimilar] is the transition [m] takes in
      response to a [label]-move by the other system, from [m]'s state
      [from], landing somewhere in [bisimilar].

      Among the actions out of [from] carrying [label], only those whose
      destinations meet [bisimilar] are kept, each restricted to that
      intersection; of those, the one with the shortest annotation wins
      {i (fewest steps left to perform)}, and its least destination is the
      answer.

      @raise NoBisimilarResponse when nothing qualifies. *)
  val respond : fsm -> state -> label -> states -> transition
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type tree = Base.Tree.t and type trees = Base.Trees.t)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type label = C.Label.t
   and type transition = C.Transition.t
   and type fsm = FSM.t
