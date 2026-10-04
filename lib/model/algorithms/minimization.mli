(** {i See {!Model.S.Minimization}.} *)
module type S = sig
  type state
  type states
  type label
  type labels
  type edgemap
  type partition
  type fsm

  (** An FSM and the partition of its states into bisimilarity classes. *)
  type t =
    { fsm : fsm
    ; pi : partition
    }

  include Json.S with type k = t (** @closed *)

  exception CannotSplitEmptyBlock of unit

  (** [ensure_nonempty b] checks that the block [b] has a state.

      @raise CannotSplitEmptyBlock if [b] is empty (raised here). *)
  val ensure_nonempty : states -> unit

  (** [split_block_by reach s block] is [block] split by [s]: the states
      that [reach] maps to the same set of blocks as [s] ([s] included),
      and, if there are any, the rest.

      @raise CannotSplitEmptyBlock
        if [block] is empty (propagated from
        {!ensure_nonempty}). *)
  val split_block_by
    :  (state -> partition)
    -> state
    -> states
    -> states * states option

  (** [split_block pi s edges block] is [block] split by [s]: the states
      that reach, by one step of [edges], the same blocks of [pi] as [s]
      does, and, if there are any, the rest.

      @raise CannotSplitEmptyBlock
        if [block] is empty (propagated from
        {!split_block_by}). *)
  val split_block
    :  partition
    -> state
    -> edgemap
    -> states
    -> states * states option

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (states * states)

  (** [ensure_equal a b] checks that a split which found nothing to split
      off returned the block it was given.

      @raise Split_OnlyReturnedOneBlock_ButNeqBlock
        if [a] and [b] differ
        (raised here). *)
  val ensure_equal : states -> states -> unit

  (** [for_each_label pi changed edges block label] refines [block] once by
      one visible [label]: it splits [block] by its least state, on [edges]
      restricted to [label], and on a split replaces [block] in [pi] by the
      two halves, makes [block] the half with that state, and sets
      [changed].

      @raise Not_found if [block] is empty (propagated from [States.min_elt]).
      @raise Split_OnlyReturnedOneBlock_ButNeqBlock
        should a split that found
        nothing return a different block (propagated from {!ensure_equal}). *)
  val for_each_label
    :  partition ref
    -> bool ref
    -> edgemap
    -> states ref
    -> label
    -> unit

  (** [silent_closures edges] is a function from a state to the states it
      reaches by zero or more silent steps of [edges] (Milner's [=ε=>], so
      always including the state itself). Memoised; it reads only the
      silent edges, so an unsaturated FSM's edges will do.

      Raises nothing. *)
  val silent_closures : edgemap -> state -> states

  (** [for_each_block ?closure pi changed alphabet edges block] refines one
      block of [pi] by every visible label of [alphabet] (over [edges]) and,
      given [closure], by the blocks each state reaches by [=ε=>], updating
      [pi] and [changed] as {!for_each_label} does.

      @raise Not_found if [block] is empty (propagated from
                       {!for_each_label}).
      @raise Split_OnlyReturnedOneBlock_ButNeqBlock
        as {!for_each_label}
        (propagated). *)
  val for_each_block
    :  ?closure:(state -> states)
    -> partition ref
    -> bool ref
    -> labels
    -> edgemap
    -> states
    -> unit

  (** [partition_states ?silent x] is the coarsest partition of [x]'s states
      in which the states of a block reach the same blocks by each visible
      label and, given [silent], by [=ε=>] computed from [silent]'s silent
      edges.

      Naive partition refinement from one block, until no block splits.
      Weak bisimilarity needs both kinds of move: on a saturated FSM without
      [silent] the result is coarser, and cannot tell [τ.a + b] from
      [a + b]. [silent] is normally the {e unsaturated} FSM's edges, since
      saturation drops silent steps. [x] is used as given, not saturated
      here.

      Raises nothing in practice (blocks are never empty); would propagate
      {!for_each_block}'s exceptions. *)
  val partition_states : ?silent:edgemap -> fsm -> partition

  (** [fsm x] is [x] with the partition of its states into weak
      bisimilarity classes: [x] saturated ({!FSM.saturate}, a no-op without
      silent labels) and partitioned by {!partition_states}, splitting by
      [=ε=>] from [x]'s own silent steps.

      Raises nothing in practice; as {!partition_states}. *)
  val fsm : fsm -> t
end

module Make
    (C : Components.S)
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
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type partition = C.Partition.t
   and type fsm = FSM.t
