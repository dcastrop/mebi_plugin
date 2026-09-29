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

  (** See {!Model.S.Partition.t}. *)
  type partition

  (** A state of the simulation game: a state of the left-hand FSM paired
      with the state of the right-hand FSM that is simulating it. This is
      what one [weak_sim _ _ x y] goal corresponds to. *)
  module Pair : sig
    type t = state * state

    val compare : t -> t -> int
    val equal : t -> t -> bool

    module Set : Set.S with type elt = t
  end

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

  (** [successors a b pi p] is every game state reachable from [p] in one
      move, where [a] is the {b unsaturated} left-hand FSM {i (its
      transitions are the obligations, one per move the left-hand system can
      make)}, [b] is the {b saturated} right-hand FSM {i (its actions are the
      weak transitions available in reply)} and [pi] is the bisimilar
      partition.

      Mirrors what the proof solver does with one [weak_sim] goal: a silent
      move to a state already bisimilar to the right-hand one is answered by
      standing still, and everything else by {!val:respond}. An obligation
      with no bisimilar response is dropped rather than raising -- in a
      genuine bisimulation there are none, and a caller that wants to know
      should compare the lengths. *)
  val successors : fsm -> fsm -> partition -> Pair.t -> Pair.t list

  (** [reachable a b pi root] is the set of game states reachable from
      [root], by breadth-first closure over {!val:successors}.

      This is the whole relation the proof needs, computed once, before any
      proof step runs. The proof solver instead discovers it depth-first
      while building the proof term, which is why it re-derives pairs it has
      already proved whenever the product is not a tree. See
      [ASSISTED-CHANGES.md], 2026-09-29, and backlog item B2. *)
  val reachable : fsm -> fsm -> partition -> Pair.t -> Pair.Set.t

  (** What a proof of this product costs, in [weak_sim] goals, under each of
      the two strategies. *)
  type cost =
    { pairs : int (** game states: one goal each under a mutual cofix *)
    ; moves : int (** moves between them: one closure each *)
    ; nested : int option
      (** goals a walk that can only close against its own {e ancestors} must
          visit -- i.e. what a fresh nested cofix per newly-seen pair costs.
          [None] when the count passed [cap], which means the nested strategy
          will not finish in any useful time. *)
    }

  (** [estimate ?cap_factor a b pi root] measures both strategies on the
      product reachable from [root], without running a single proof step.

      A mutual cofix over the whole relation visits each game state once and
      each move once: [pairs + moves]. A fresh nested cofix per newly-seen
      pair can only close a repeat that is an {e ancestor} on the current
      branch, so it re-derives any state reachable by a second route -- it
      walks the tree of simple paths, which is exponential on a product that
      is not a tree. [nested] counts exactly that walk, stopping once it
      passes [cap_factor] times the mutual cost (default 4) -- the exact
      figure past that point does not matter, only that it is larger.

      The ratio is what the caller wants: equal means the product is a tree
      and the two strategies do identical work; [None] means only the mutual
      cofix will finish. *)
  val estimate : ?cap_factor:int -> fsm -> fsm -> partition -> Pair.t -> cost

  (** [prefer_mutual c] is [true] when the nested walk costs more than a
      mutual cofix would, [c.nested = None] included. *)
  val prefer_mutual : cost -> bool
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
   and type partition = C.Partition.t
