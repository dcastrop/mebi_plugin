type bisim_result =
  { are_bisimilar : bool
  ; bisimilar_states : Fsm.Partition.t
  ; non_bisimilar_states : Fsm.Partition.t
  }

module RCP : sig
  module Examples : sig
    type example =
      { name : string
      ; s : Fsm.fsm
      ; t : Fsm.fsm
      }

    val exa : string -> Fsm.fsm -> Fsm.fsm -> example
    val exa_1 : example
    val exa_2 : example
  end

  module KS90 : sig
    exception EmptyBlock of Fsm.States.t
    exception PartitionsNotDisjoint of Fsm.Partition.t

    val reachable_partitions
      :  ?coq:bool
      -> ?show:bool
      -> ?debug:bool
      -> Fsm.States.t Fsm.Actions.t
      -> Fsm.Partition.t
      -> Fsm.Partition.t

    val split
      :  ?coq:bool
      -> ?show:bool
      -> ?debug:bool
      -> Fsm.States.t
      -> Fsm.action
      -> Fsm.Partition.t
      -> Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.States.t * Fsm.States.t

    val run
      :  ?coq:bool
      -> ?show:bool
      -> ?debug:bool
      -> Fsm.fsm
      -> Fsm.fsm
      -> bisim_result
  end

  module PT87 : sig end
end
