type bisim_result =
  { are_bisimilar : bool
  ; bisimilar_states : Fsm.Partition.t
  ; non_bisimilar_states : Fsm.Partition.t
  }

module RCP : sig
  module KS90 : sig
    exception EmptyBlock of Fsm.States.t
    exception PartitionsNotDisjoint of Fsm.Partition.t

    val reachable_blocks
      :  ?params:Utils.logging_params
      -> Fsm.States.t Fsm.Actions.t
      -> Fsm.Partition.t
      -> Fsm.Partition.t

    val split
      :  ?params:Utils.logging_params
      -> Fsm.States.t
      -> Fsm.action
      -> Fsm.Partition.t
      -> Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.States.t * Fsm.States.t

    val main_loop
      :  ?params:Utils.logging_params
      -> Fsm.Alphabet.t * Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.Partition.t ref
      -> bool ref
      -> unit

    val split_bisimilar
      :  ?params:Utils.logging_params
      -> Fsm.States.t * Fsm.States.t
      -> (Fsm.state, Fsm.state) Hashtbl.t
      -> Fsm.Partition.t
      -> Fsm.Partition.t * Fsm.Partition.t

    val run : ?params:Utils.logging_params -> Fsm.fsm -> Fsm.fsm -> bisim_result
  end

  module PT87 : sig end
end
