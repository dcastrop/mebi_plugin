type bisim_result =
  { are_bisimilar : bool
  ; bisimilar_states : Fsm.Partition.t
  ; non_bisimilar_states : Fsm.Partition.t
  }

module RCP : sig
  module KS90 : sig
    exception EmptyBlock of Fsm.States.t
    exception PartitionsNotDisjoint of Fsm.Partition.t

    module DebugMessages : sig
      val reachable_blocks
        :  ?show:bool
        -> ?details:bool
        -> ?debug:bool
        -> Fsm.States.t
        -> unit

      val split
        :  ?show:bool
        -> ?details:bool
        -> ?debug:bool
        -> Fsm.States.t
        -> Fsm.action
        -> Fsm.Partition.t
        -> Fsm.States.t Fsm.Actions.t Fsm.Edges.t
        -> unit

      val run_merged
        :  ?show:bool
        -> ?details:bool
        -> ?debug:bool
        -> Fsm.fsm
        -> unit

      val run_iter
        :  ?show:bool
        -> ?details:bool
        -> ?debug:bool
        -> Fsm.action
        -> Fsm.States.t
        -> Fsm.Partition.t
        -> unit

      val run_exit
        :  ?show:bool
        -> ?details:bool
        -> ?debug:bool
        -> (Fsm.state, Fsm.state) Hashtbl.t
        -> Fsm.States.t
        -> Fsm.States.t
        -> unit

      val run_check
        :  ?show:bool
        -> ?details:bool
        -> ?debug:bool
        -> Fsm.state
        -> Fsm.state
        -> Fsm.state
        -> Fsm.state
        -> Fsm.States.t
        -> Fsm.States.t
        -> unit
    end

    val reachable_blocks
      :  ?show:bool
      -> ?details:bool
      -> ?debug:bool
      -> Fsm.States.t Fsm.Actions.t
      -> Fsm.Partition.t
      -> Fsm.Partition.t

    val split
      :  ?show:bool
      -> ?details:bool
      -> ?debug:bool
      -> Fsm.States.t
      -> Fsm.action
      -> Fsm.Partition.t
      -> Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.States.t * Fsm.States.t

    val main_loop
      :  ?show:bool
      -> ?details:bool
      -> ?debug:bool
      -> Fsm.Alphabet.t * Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.Partition.t ref
      -> bool ref
      -> unit

    val split_bisimilar
      :  ?show:bool
      -> ?details:bool
      -> ?debug:bool
      -> Fsm.States.t * Fsm.States.t
      -> (Fsm.state, Fsm.state) Hashtbl.t
      -> Fsm.Partition.t
      -> Fsm.Partition.t * Fsm.Partition.t

    val run
      :  ?show:bool
      -> ?details:bool
      -> ?debug:bool
      -> Fsm.fsm
      -> Fsm.fsm
      -> bisim_result
  end

  module PT87 : sig end
end
