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
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type base = Base.t)
    (Labels : Labels.S with type elt = Label.t)
    (Action : Action.S with type label = Label.t)
    (ActionMap :
       Actionmap.S with type action = Action.t and type states = States.t)
    (EdgeMap :
       Edgemap.S
       with type state = State.t
        and type actionmap = ActionMap.t'
        and type label = Label.t)
    (Partition :
       State_partition.S
       with type elt = States.t
        and type edgemap = EdgeMap.t'
        and type state = State.t)
    (Info : Info.S with type base = State.base and type labels = Labels.t)
    (FSM :
       FSM.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t'
        and type info = Info.t) :
  S
  with type state = State.t
   and type states = States.t
   and type label = Label.t
   and type labels = Labels.t
   and type edgemap = EdgeMap.t'
   and type partition = Partition.t
   and type fsm = FSM.t
