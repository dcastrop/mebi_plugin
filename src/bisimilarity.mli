type bisim_result =
  { are_bisimilar : bool
  ; merged_fsm : Fsm.fsm
  ; bisimilar_states : Fsm.Partition.t
  ; non_bisimilar_states : Fsm.Partition.t
  }

type minim_result = Fsm.fsm * Fsm.Partition.t

type of_bisim_result =
  | OfMerged of
      ((Fsm.fsm * Fsm.fsm)
      * (Fsm.fsm * (Fsm.state, Fsm.state) Hashtbl.t)
      * Fsm.Partition.t ref)
  | OfMinimized of (Fsm.fsm * Fsm.Partition.t ref)

type result =
  | BisimResult of bisim_result
  | MinimResult of minim_result

val default_params : Utils.Logging.params

module PStr : sig
  val bisim_result
    :  ?params:Utils.Logging.params
    -> ?merged_from:Fsm.fsm * Fsm.fsm
    -> bisim_result
    -> string
end

module RCP : sig
  module KS90 : sig
    exception EmptyBlock of Fsm.States.t
    exception PartitionsNotDisjoint of Fsm.Partition.t

    val reachable_blocks
      :  ?params:Utils.Logging.params
      -> Fsm.States.t Fsm.Actions.t
      -> Fsm.Partition.t
      -> Fsm.Partition.t

    val reach_same_blocks : Fsm.Partition.t -> Fsm.Partition.t -> bool

    val lengths_gtr_0
      :  Fsm.States.t Fsm.Actions.t
      -> Fsm.States.t Fsm.Actions.t
      -> bool * bool

    val split
      :  ?params:Utils.Logging.params
      -> Fsm.States.t
      -> Fsm.action
      -> Fsm.Partition.t
      -> Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.States.t * Fsm.States.t

    val inner_loop
      :  ?params:Utils.Logging.params
      -> Fsm.States.t
      -> Fsm.States.t
      -> Fsm.States.t ref
      -> Fsm.Partition.t ref
      -> bool ref
      -> unit

    val main_loop
      :  ?params:Utils.Logging.params
      -> Fsm.Alphabet.t * Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.Partition.t ref
      -> bool ref
      -> unit

    val run_main_loop
      :  ?params:Utils.Logging.params
      -> Fsm.Alphabet.t
      -> Fsm.States.t Fsm.Actions.t Fsm.Edges.t
      -> Fsm.Partition.t ref
      -> unit

    type bisim_input =
      | ToMerge of (Fsm.fsm * Fsm.fsm)
      | Merged of
          (Fsm.fsm * Fsm.fsm * Fsm.fsm * (Fsm.state, Fsm.state) Hashtbl.t)
      | Minimize of Fsm.fsm

    exception RunInputNotExpected of bisim_input

    val run : ?params:Utils.Logging.params -> bisim_input -> of_bisim_result

    type state_origins =
      { s : bool
      ; t : bool
      }

    val origins_of
      :  ?params:Utils.Logging.params
      -> Fsm.States.t
      -> Fsm.States.t * Fsm.States.t
      -> (Fsm.state, Fsm.state) Hashtbl.t
      -> state_origins

    val split_bisimilar
      :  ?params:Utils.Logging.params
      -> Fsm.States.t * Fsm.States.t
      -> (Fsm.state, Fsm.state) Hashtbl.t
      -> Fsm.Partition.t ref
      -> Fsm.Partition.t * Fsm.Partition.t

    val result : ?params:Utils.Logging.params -> of_bisim_result -> result
  end

  module PT87 : sig end
end
