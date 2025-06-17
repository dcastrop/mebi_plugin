type run_params = Fsm.pair * Fsm.t option

type result_kind =
  (Fsm.pair * Fsm.t) * (Model.Partition.t * Model.Partition.t)

exception CouldNotFindOriginOfState of Model_state.t

val block_has_shared_origin :
  Model.States.t -> Fsm.t -> Fsm.t -> bool

val split_bisimilar :
  Model.Partition.t ->
  Fsm.t ->
  Fsm.t ->
  Model.Partition.t * Model.Partition.t

val handle_run_params : run_params -> bool -> Fsm.t

val add_to_block_option :
  Model_state.t ->
  Model.States.t option ->
  Model.States.t option

val split_block :
  Model.States.t ->
  Model.States.t Model.Actions.t Model.Edges.t ->
  Model.Partition.t ->
  Model.States.t * Model.States.t option

val iterate :
  Model.Alphabet.t ->
  Model.States.t Model.Actions.t Model.Edges.t ->
  Model.Partition.t ref ->
  bool ref ->
  bool ->
  unit

val run : ?weak:bool -> run_params -> result_kind
