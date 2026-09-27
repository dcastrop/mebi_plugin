(** {i See {!Model.S.Minimization}.} *)
module type S = sig
  type state
  type states
  type label
  type labels
  type edgemap
  type partition
  type fsm

  type t =
    { fsm : fsm
    ; pi : partition
    }

  include Json.S with type k = t (** @closed *)

  exception CannotSplitEmptyBlock of unit

  val ensure_nonempty : states -> unit

  val split_block
    :  partition
    -> state
    -> edgemap
    -> states
    -> states * states option

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (states * states)

  val ensure_equal : states -> states -> unit

  val for_each_label
    :  partition ref
    -> bool ref
    -> edgemap
    -> states ref
    -> label
    -> unit

  val for_each_block
    :  partition ref
    -> bool ref
    -> labels
    -> edgemap
    -> states
    -> unit

  val partition_states : fsm -> partition
  val fsm : fsm -> t
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.States.t
        and type labels = C.Labels.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) :
  S
  with type state = C.State.t
   and type states = C.States.t
   and type label = C.Label.t
   and type labels = C.Labels.t
   and type edgemap = C.EdgeMap.t'
   and type partition = C.Partition.t
   and type fsm = FSM.t
