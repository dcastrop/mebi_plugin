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
      bisimilarity classes: [x] saturated ([FSM.S.saturate], a no-op without
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
