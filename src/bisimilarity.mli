module RCP : sig
  module Examples : sig
    val exa_1 : Fsm.fsm * Fsm.fsm
    val exa_2 : Fsm.fsm * Fsm.fsm
  end

  module KS90 : sig
    exception EmptyBlock of Fsm.States.t
    exception PartitionsNotDisjoint of Fsm.Partition.t

    val reachable_partition
      :  Fsm.States.t Fsm.Actions.t
      -> Fsm.Partition.t
      -> Fsm.Partition.t

    val split
      :  Fsm.States.t
      -> Fsm.action
      -> Fsm.Partition.t
      -> Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.Partition.t

    exception SplitEmpty of Fsm.Partition.t
    exception SplitTooMany of Fsm.Partition.t
    exception MultipleActionsSameLabel of Fsm.States.t Fsm.Actions.t Fsm.Edges.t

    exception
      OldStateHasNoNewState of (Fsm.state * (Fsm.state, Fsm.state) Hashtbl.t)

    val run : Fsm.fsm -> Fsm.fsm -> bool * Fsm.Partition.t
  end

  module PT87 : sig end
end

val bisim_foo : int
