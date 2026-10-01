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

  (** Refine one block of [pi] by every visible label of the alphabet. *)
  val for_each_block
    :  partition ref
    -> bool ref
    -> labels
    -> edgemap
    -> states
    -> unit

  (** [partition_states x] refines the one-block partition of [x]'s states
      until no block splits (naive partition refinement): the coarsest
      partition in which states of a block reach the same blocks by each
      visible label. On a {e saturated} FSM that is weak bisimilarity. The
      FSM is used as given, not saturated here. *)
  val partition_states : fsm -> partition

  (** [fsm x] saturates [x] ({!FSM.saturate}, a no-op without silent labels)
      and partitions it. *)
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
