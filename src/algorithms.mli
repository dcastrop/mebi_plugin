module Bisimilarity : sig
  type t = Fsm.pair * Fsm.t option
  type result = (Fsm.pair * Fsm.t) * (Model.Partition.t * Model.Partition.t)

  val block_has_shared_origin : Model.States.t -> Fsm.t -> Fsm.t -> bool

  val split_bisimilar
    :  Model.Partition.t
    -> Fsm.t
    -> Fsm.t
    -> Model.Partition.t * Model.Partition.t

  val add_to_block_option
    :  Model.State.t
    -> Model.States.t option
    -> Model.States.t option

  val split_block
    :  Model.States.t
    -> Model.States.t Model.Actions.t Model.Edges.t
    -> Model.Partition.t
    -> Model.States.t * Model.States.t option

  val iterate
    :  Model.Alphabet.t
    -> Model.States.t Model.Actions.t Model.Edges.t
    -> Model.Partition.t ref
    -> bool ref
    -> bool
    -> unit

  val handle_run_params : t -> bool -> Fsm.t
  val run : ?weak:bool -> t -> result
end

type t = Bisim of (bool * Bisimilarity.t)
type result = Bisim of Bisimilarity.result

val run : t -> result
val result_to_bool : result -> bool
