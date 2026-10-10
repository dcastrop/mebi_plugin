(** The simulation game's response choice, and the product it generates.

    This is the decision the proof solver makes at every step: given that the
    left-hand FSM has moved [from --label-> _], which state must the
    right-hand FSM move to? It used to live inside
    {!Mebi_plugin.Proof_solver_step}, tangled with the {b Rocq} goal it was
    read out of, which meant it could only ever be run one proof step at a
    time and could not be tested without a Rocq runtime.

    It is pure model code, so it lives here. {i See [ASSISTED-CHANGES.md],
    2026-09-29: an offline re-implementation of {!S.respond} that mirrored
    the original rather than sharing it got the right pairs but the wrong
    response in 8 of 16 cases, because the tie-breaks run through the
    ordering of actions ([Action.compare]). Everything that needs this
    decision must call this function, never reproduce it.} *)
module type S = sig
  (** See {!Components.S.State.t}. *)
  type state

  (** See {!Components.S.State.Set.t}. *)
  type states

  (** See {!Components.S.Label.t}. *)
  type label

  (** See {!Components.S.Transition.t}. *)
  type transition

  (** See [FSM.S.t]. *)
  type fsm

  (** See {!Components.S.Partition.t}. *)
  type partition

  (** A state of the simulation game: a state of the left-hand FSM paired
      with the state of the right-hand FSM that is simulating it. This is
      what one [weak_sim _ _ x y] goal corresponds to. *)
  module Pair : sig
    type t = state * state

    (** [compare p q] is [p] against [q] lexicographically: the left
        states, then the right ones. Raises nothing. *)
    val compare : t -> t -> int

    (** [equal p q] is whether [compare p q] is [0]. Raises nothing. *)
    val equal : t -> t -> bool

    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t
  end

  (** Raised by {!val:respond} when no action out of the given state, under
      the given label, can reach any of the states it was asked to stay
      bisimilar to. *)
  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  (** See {!Components.S.EdgeMap.t'}. *)
  type edgemap

  (** [respond ?silent m from label bisimilar] is the transition [m] takes
      from its state [from] in answer to a [label]-move by the other system,
      landing in [bisimilar].

      Among the weak actions out of [from] under [label], only those whose
      destinations meet [bisimilar] are kept, each restricted to that
      intersection; of those, the one with the shortest annotation wins
      {i (fewest steps left to perform)}, ties going to the least action by
      [Action.compare], and its least destination is the answer. [from] is
      saturated first if [m] is saturated on demand.

      A {e silent} [label] is answered differently when [silent] is given
      (the {b unsaturated} FSM's edges, which still hold the silent steps):
      by the nearest state in [bisimilar] that [from] reaches by {e one or
      more} silent steps, annotated with that path. Zero steps -- standing
      still -- is the caller's case, decided before asking. Without
      [silent], a silent [label] finds nothing, since saturation keeps only
      weak moves with a visible action.

      @raise NoBisimilarResponse when nothing qualifies (raised here). *)
  val respond : ?silent:edgemap -> fsm -> state -> label -> states -> transition

  (** [simulation a b b_saturated root] is the greatest weak simulation
      from [a]'s states to [b]'s among the pairs reachable from [root] in
      the simulation game: [(x, y)] is in it when every strong move
      [x -l-> x'] of [a] has an answer [y =l=> y'] ([=ε=>], zero steps
      included, for a silent [l]) with [(x', y')] in it again -- [weak_sim]'s
      own definition. So [root] is in it iff [fst root] is weakly simulated
      by [snd root].

      [b] is the unsaturated FSM (for silent closures) and [b_saturated]
      gives the visible weak moves. Coarser than bisimilarity: [a.b] is
      simulated by [a.(b + c)], not bisimilar to it.

      @raise Game_too_large
        past {!val:with_cap}'s cap (propagated from the
        game walk). *)
  val simulation : fsm -> fsm -> fsm -> Pair.t -> Pair.Set.t

  (** How [b] answers a move: by standing still (a silent move, when [b]'s
      state already qualifies), or by making a transition. *)
  type answer =
    | Stay
    | Move of transition

  (** [answer ?silent ?sim b pi y label x'] is how [b], at [y], answers the
      other system's move [-label-> x'], or [None] if it cannot: the single
      place answers are chosen, for the proof solver and {!val:successors}
      alike.

      A silent move is answered by standing still if [y] is bisimilar to
      [x'] (by [pi]); otherwise by {!val:respond} into [x']'s bisimilarity
      class. Failing both, and given [sim], the same two tries against
      [x']'s simulators.

      Raises nothing ({!val:respond}'s {!NoBisimilarResponse} is caught). *)
  val answer
    :  ?silent:edgemap
    -> ?sim:(state -> states)
    -> fsm
    -> partition
    -> state
    -> label
    -> state
    -> answer option

  (** [successors ?silent ?sim ~refl a b pi p] is every game state
      reachable from [p] in one move: for each move of [p]'s left state in
      [a], the pair it leads to with {!val:answer}'s answer from [b].

      [a] is the {b unsaturated} left-hand FSM {i (its transitions are the
      obligations, one per move the left-hand system can make)}, [b] the
      {b saturated} right-hand FSM {i (its weak transitions are the
      answers)}, and [pi] the bisimilarity partition. This mirrors what the
      proof solver does with one [weak_sim] goal; [silent] lets a silent
      move be answered by moving silently, and [sim] (a state's simulators,
      from {!val:simulation}) is the fallback for a [weak_sim] goal between
      states that are not bisimilar. A move with no answer is dropped, not
      an error: in a genuine bisimulation there are none.

      [refl] says whether both sides use the same LTS. If so, a pair of
      equal states has no successors: the solver closes [weak_sim x x] by
      [weak_sim_refl] before anything else, so nothing past it is visited.

      Raises nothing. *)
  val successors
    :  ?silent:edgemap
    -> ?sim:(state -> states)
    -> refl:bool
    -> fsm
    -> fsm
    -> partition
    -> Pair.t
    -> Pair.t list

  (** Raised by a game walk ({!val:reachable}, the planners) that visits
      more pairs than the cap {!val:with_cap} set. *)
  exception Game_too_large of int

  (** [with_cap n f] is [f ()], run with every game walk capped at [n]
      pairs. For FSMs saturated on demand, whose walks saturate as they go
      ([MeBi Config Bounds Game]). The previous cap is restored afterwards.

      @raise Game_too_large
        if a walk inside [f] passes [n] pairs
        (propagated, like anything else [f] raises). *)
  val with_cap : int -> (unit -> 'a) -> 'a

  (** [reachable ?silent ?sim ~refl a b pi root] is the set of game states
      reachable from [root] ([root] included), by breadth-first closure over
      {!val:successors}.

      This is the whole relation the proof needs, computed once, before any
      proof step runs. The proof solver instead discovers it depth-first
      while building the proof term, which is why it re-derives pairs it has
      already proved whenever the product is not a tree. See
      [ASSISTED-CHANGES.md], 2026-09-29, and backlog item B2.

      @raise Game_too_large past {!val:with_cap}'s cap (raised by the walk). *)
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
      [bisim_r]): the moves of [p]'s left state answered by [b], as in
      {!val:successors}, then the moves of its right state answered by [a]
      (the same game with the systems swapped, its pairs swapped back).
      Silent moves can be answered by moving silently on either side.

      Raises nothing. *)
  val successors_bisim : refl:bool -> game -> partition -> Pair.t -> Pair.t list

  (** [reachable_bisim ~refl g pi root] is {!val:reachable} over
      {!val:successors_bisim}: the relation a mutual cofix for a
      [weak_bisimilar] goal needs.

      @raise Game_too_large past {!val:with_cap}'s cap (raised by the walk). *)
  val reachable_bisim : refl:bool -> game -> partition -> Pair.t -> Pair.Set.t

  (** Policies for choosing answers. [Default] is {!val:answer}'s choice,
      the solver's behaviour unless [MeBi Config Solver Answers] says
      otherwise. Any other policy is turned, at [MeBi Sim Begin], into a
      {!type:Policy.plan}: the answer for every move the game can reach, and
      the pairs that makes. The solver then answers from the plan, and the
      mutual block and the cofix-strategy estimate read its pairs, so the
      three cannot disagree. *)
  module Policy : sig
    (** [Default]: {!val:answer}'s choices. [Greedy]: breadth first,
        preferring an answer whose pair was already reached. [Minimal]: from
        every pair any answer reaches, delete pairs while every remaining
        pair can still answer all its moves within what remains (minimal by
        inclusion), then answer each move with its cheapest remaining
        option. *)
    type t =
      | Default
      | Greedy
      | Minimal

    (** [name p] is [p] in lower case: "default", "greedy" or "minimal".
        Raises nothing. *)
    val name : t -> string

    (** One possible answer: the pair it leads to, its witness length (weak
        transitions to justify; 0 for standing still), and the answer. *)
    type choice =
      { next : Pair.t
      ; cost : int
      ; answer : answer
      }

    (** A move, the way the solver meets it: whether the roles are swapped
        ([bisim_r]), the moving system's state, the move, and the answering
        system's state. *)
    type key =
      { swapped : bool
      ; mover : state
      ; answerer : state
      ; label : label
      ; target : state
      }

    (** One move to answer: the answer {!val:answer} picks ([default]) and
        every valid one ([candidates]). *)
    type obligation =
      { key : key
      ; default : choice option
      ; candidates : choice list
      }

    (** A game, as each pair's obligations. *)
    type game_of = Pair.t -> obligation list

    (** [sim_game ?silent ?sim ~refl a b pi] is the [weak_sim] game as
        {!val:successors} plays it: each pair's obligations, with
        {!val:answer}'s choice as the default and every valid answer as a
        candidate.

        Raises nothing (as a function, nor when applied). *)
    val sim_game
      :  ?silent:edgemap
      -> ?sim:(state -> states)
      -> refl:bool
      -> fsm
      -> fsm
      -> partition
      -> game_of

    (** [bisim_game ~refl g pi] is the [weak_bisimilar] game as
        {!val:successors_bisim} plays it: the left-hand obligations, then
        the right-hand ones, keyed as swapped.

        Raises nothing (as a function, nor when applied). *)
    val bisim_game : refl:bool -> game -> partition -> game_of

    (** Pairs reached, moves answered (one closure each), total witness
        length, and moves left without an answer. *)
    type measure =
      { pairs : int
      ; moves : int
      ; witness : int
      ; unanswered : int
      }

    (** Maps keyed by move. *)
    module KeyMap : Map.S with type key = key

    (** A policy's answers over a whole game: the [relation] (pairs reached
        from [root]), the answer [chosen] for each move, each pair's
        successors ([next]), and the {!type-measure}. *)
    type plan =
      { policy : t
      ; root : Pair.t
      ; relation : Pair.Set.t
      ; chosen : choice KeyMap.t
      ; next : Pair.t list Pair.Map.t
      ; measure : measure
      }

    (** [plan p game_of root] is policy [p]'s answer to every move of the
        game reachable from [root] (breadth first), with the pairs and
        measure they make.

        @raise Game_too_large past {!val:with_cap}'s cap (raised by the
                              walk). *)
    val plan : t -> game_of -> Pair.t -> plan

    (** [minimal_relation game_of root] is the relation [Minimal] answers
        within: from every pair any answer reaches, pairs removed while
        every remaining pair can still answer all its moves within what
        remains (the first removable in {!Pair.Set} order each time), then
        trimmed to what [root] reaches. If even that starting relation
        leaves some move unanswered, it is returned as is. Exposed for
        [tests.exe]'s check against the original algorithm.

        @raise Game_too_large
          past {!val:with_cap}'s cap (raised by the walk
          over the starting relation). *)
    val minimal_relation : game_of -> Pair.t -> Pair.Set.t

    (** [measure p game_of root] is the {!type:measure} of
        [plan p game_of root].

        @raise Game_too_large as {!val:plan} (propagated). *)
    val measure : t -> game_of -> Pair.t -> measure

    (** [predicted m] is the iterations a plan with measure [m] is predicted
        to cost: [3 pairs + 6 moves + 3.3 witness], fitted to the 41
        checked-in proofs' real counts. Raises nothing. *)
    val predicted : measure -> float

    (** [best game_of root] is the plan with the lowest {!val:predicted}
        cost among the three policies, among those answering every move:
        [Default] unless another is strictly cheaper.

        @raise Game_too_large as {!val:plan} (propagated). *)
    val best : game_of -> Pair.t -> plan

    (** [choose p k] is the answer plan [p] gives to the move [k], or [None]
        if [p] does not reach it. Raises nothing. *)
    val choose : plan -> key -> answer option

    (** [successors p x] is the pairs [p]'s answers lead to from [x] (none
        if [p] does not reach [x]): a game step, for {!val:estimate_plan}.
        Raises nothing. *)
    val successors : plan -> Pair.t -> Pair.t list
  end

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

  (** [estimate ?cap_factor ?silent ?sim ~refl a b pi root] is what a proof
      of the product reachable from [root] costs under each strategy,
      measured without running a single proof step.

      A mutual cofix over the whole relation visits each game state once and
      each move once: [pairs + moves]. A fresh nested cofix per newly-seen
      pair can only close a repeat that is an {e ancestor} on the current
      branch, so it re-derives any state reachable by a second route -- it
      walks the tree of simple paths, which is exponential on a product that
      is not a tree. [nested] counts exactly that walk, stopping once it
      passes [cap_factor] times the mutual cost (default 4): past that point
      only the fact that it is larger matters. Equal means the product is a
      tree and the two strategies do identical work; [None] means only the
      mutual cofix will finish.

      @raise Game_too_large past {!val:with_cap}'s cap (raised by the walk). *)
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

  (** [estimate_bisim ?cap_factor ~refl g pi root] is {!val:estimate} over
      {!val:successors_bisim}.

      @raise Game_too_large past {!val:with_cap}'s cap (raised by the walk). *)
  val estimate_bisim
    :  ?cap_factor:int
    -> refl:bool
    -> game
    -> partition
    -> Pair.t
    -> cost

  (** [estimate_plan ?cap_factor p] is {!val:estimate} over the plan's own
      successors.

      @raise Game_too_large
        past {!val:with_cap}'s cap (raised by the walk,
        which re-walks the plan's pairs). *)
  val estimate_plan : ?cap_factor:int -> Policy.plan -> cost

  (** [prefer_mutual c] is whether the nested walk costs more than a mutual
      cofix would, [c.nested = None] included. Raises nothing. *)
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
