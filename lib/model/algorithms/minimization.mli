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

  (** @raise CannotSplitEmptyBlock on an empty block. *)
  val ensure_nonempty : states -> unit

  (** [split_block_by reach s block] splits [block] by [s]: the states that
      [reach] maps to the same set of blocks as [s], and ([Some]) the rest,
      if any. *)
  val split_block_by
    :  (state -> partition)
    -> state
    -> states
    -> states * states option

  (** [split_block pi s edges block] splits [block] by [s]: the states that
      reach, by [edges], the same blocks of [pi] as [s] does, and
      ([Some]) the rest, if any. *)
  val split_block
    :  partition
    -> state
    -> edgemap
    -> states
    -> states * states option

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (states * states)

  (** @raise Split_OnlyReturnedOneBlock_ButNeqBlock
        if a split that found
        nothing to split off returned a different block. *)
  val ensure_equal : states -> states -> unit

  (** One refinement step: split [block] by one visible [label] (edges
      restricted to it), updating [pi] and setting [changed] if it split. *)
  val for_each_label
    :  partition ref
    -> bool ref
    -> edgemap
    -> states ref
    -> label
    -> unit

  (** [silent_closures edges] maps a state to the states it reaches by zero
      or more silent steps of [edges] (Milner's [=ε=>], so it always contains
      the state itself). Memoised; reads only silent edges. *)
  val silent_closures : edgemap -> state -> states

  (** Refine one block of [pi] by every visible label of the alphabet and,
      given [closure], by the blocks each state reaches by [=ε=>]. *)
  val for_each_block
    :  ?closure:(state -> states)
    -> partition ref
    -> bool ref
    -> labels
    -> edgemap
    -> states
    -> unit

  (** [partition_states ?silent x] refines the one-block partition of [x]'s
      states until no block splits (naive partition refinement): the coarsest
      partition in which states of a block reach the same blocks by each
      visible label and, given [silent], by [=ε=>] computed from [silent]'s
      silent edges. Weak bisimilarity needs both: on a saturated FSM without
      [silent] the result is coarser, and cannot tell [τ.a + b] from [a + b].
      [silent] is normally the {e unsaturated} FSM's edges, since saturation
      drops silent steps. The FSM is used as given, not saturated here. *)
  val partition_states : ?silent:edgemap -> fsm -> partition

  (** [fsm x] saturates [x] ({!FSM.saturate}, a no-op without silent labels)
      and partitions it, splitting by [=ε=>] from [x]'s own silent steps. *)
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
