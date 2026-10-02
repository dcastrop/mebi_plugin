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

  (** See {!Model.S.EdgeMap.t'}. *)
  type edgemap

  (** [respond ?silent m from label bisimilar] is the transition [m] takes in
      response to a [label]-move by the other system, from [m]'s state
      [from], landing somewhere in [bisimilar].

      Among the actions out of [from] carrying [label], only those whose
      destinations meet [bisimilar] are kept, each restricted to that
      intersection; of those, the one with the shortest annotation wins
      {i (fewest steps left to perform)}, and its least destination is the
      answer.

      A {e silent} [label] is answered differently when [silent] is given
      (the {b unsaturated} FSM's edges, which still hold the silent steps):
      by the nearest state in [bisimilar] that [from] reaches by {e one or
      more} silent steps, annotated with that path. Zero steps -- standing
      still -- is the caller's case, decided before asking. Without [silent],
      a silent [label] finds nothing, since saturation keeps only weak moves
      with a visible action.

      @raise NoBisimilarResponse when nothing qualifies. *)
  val respond : ?silent:edgemap -> fsm -> state -> label -> states -> transition

  (** [simulation a b b_saturated] is the greatest weak simulation from [a]'s
      states to [b]'s: [(x, y)] is in it when every strong move
      [x -l-> x'] of [a] has an answer [y =l=> y'] ([=ε=>], zero steps
      included, for a silent [l]) with [(x', y')] in it again -- [weak_sim]'s
      own definition. [b] is the unsaturated FSM (for silent closures),
      [b_saturated] gives the visible weak moves. Coarser than
      bisimilarity: [a.b] is simulated by [a.(b + c)], not bisimilar to it. *)
  val simulation : fsm -> fsm -> fsm -> Pair.Set.t

  (** [successors a b pi p] is every game state reachable from [p] in one
      move, where [a] is the {b unsaturated} left-hand FSM {i (its
      transitions are the obligations, one per move the left-hand system can
      make)}, [b] is the {b saturated} right-hand FSM {i (its actions are the
      weak transitions available in reply)} and [pi] is the bisimilar
      partition.

      Mirrors what the proof solver does with one [weak_sim] goal: a silent
      move to a state already bisimilar to the right-hand one is answered by
      standing still, and everything else by {!val:respond}, given [silent]
      so that a silent move can also be answered by moving silently. An
      answer bisimilar to the obligation's target is preferred; failing one,
      and given [sim] (a state's simulators, from {!val:simulation}), any
      state that simulates it -- the same fallback the solver takes for a
      [weak_sim] goal between states that are not bisimilar. An obligation
      with no bisimilar response is dropped rather than raising -- in a
      genuine bisimulation there are none, and a caller that wants to know
      should compare the lengths.

      [refl] says whether both sides use the same LTS. If so, a pair of equal
      states has no successors: the solver closes [weak_sim x x] by
      [weak_sim_refl] before anything else, so nothing past it is visited. *)
  val successors
    :  ?silent:edgemap
    -> ?sim:(state -> states)
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> Pair.t list

  (** [reachable ~refl a b pi root] is the set of game states reachable from
      [root], by breadth-first closure over {!val:successors}.

      This is the whole relation the proof needs, computed once, before any
      proof step runs. The proof solver instead discovers it depth-first
      while building the proof term, which is why it re-derives pairs it has
      already proved whenever the product is not a tree. See
      [ASSISTED-CHANGES.md], 2026-09-29, and backlog item B2. *)
  val reachable
    :  ?silent:edgemap
    -> ?sim:(state -> states)
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> Pair.Set.t

  (** The four FSMs of a {e bisimulation} game: each system as given (its
      transitions are the obligations it sets) and saturated (its weak
      transitions answer the other's). *)
  type game =
    { a : fsm
    ; a_saturated : fsm
    ; b : fsm
    ; b_saturated : fsm
    }

  (** [successors_bisim ~refl g pi p] is {!val:successors} for a
      [weak_bisimilar] goal, which has two obligations ([bisim_l] and
      [bisim_r]): [p]'s left state's moves answered by [b], as in
      {!val:successors}, and its right state's moves answered by [a], which is
      the same game with the systems swapped, its pairs swapped back. Silent
      moves can be answered by moving silently on either side. *)
  val successors_bisim : refl:bool -> game -> partition -> Pair.t -> Pair.t list

  (** [reachable_bisim ~refl g pi root]: {!val:reachable} over
      {!val:successors_bisim}, the relation a mutual cofix for a
      [weak_bisimilar] goal needs. *)
  val reachable_bisim : refl:bool -> game -> partition -> Pair.t -> Pair.Set.t

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

  (** [estimate ?cap_factor ~refl a b pi root] measures both strategies on the
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
  val estimate
    :  ?cap_factor:int
    -> ?silent:edgemap
    -> ?sim:(state -> states)
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> cost

  (** [estimate_bisim ?cap_factor ~refl g pi root]: {!val:estimate} over
      {!val:successors_bisim}. *)
  val estimate_bisim
    :  ?cap_factor:int
    -> refl:bool
    -> game
    -> partition
    -> Pair.t
    -> cost

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
        and type info = C.Info.t)
    (Saturation :
       Saturation.S
       with type state = C.State.t
        and type edgemap = C.EdgeMap.t'
        and type annotation = C.Annotation.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type label = C.Label.t
   and type transition = C.Transition.t
   and type fsm = FSM.t
   and type partition = C.Partition.t
   and type edgemap = C.EdgeMap.t'
