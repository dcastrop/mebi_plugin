type fsm_pair = Fsm.fsm * Fsm.fsm

type run_params =
  fsm_pair * Fsm.Merge.merged_fsm_result_kind option

type result_kind =
  fsm_pair
  * Fsm.Merge.merged_fsm_result_kind
  * (Fsm.Partition.t * Fsm.Partition.t)

val get_pair_from_result : fsm_pair * 'a * 'b -> fsm_pair
val resolve : result_kind -> bool

exception CouldNotFindOriginOfState of Fsm.state

val block_has_shared_origin :
  Fsm.States.t ->
  Fsm.States.t ->
  Fsm.States.t ->
  Fsm.Merge.merged_state_map ->
  bool

val split_bisimilar :
  Fsm.Partition.t ->
  Fsm.States.t ->
  Fsm.States.t ->
  Fsm.Merge.merged_state_map ->
  Fsm.Partition.t * Fsm.Partition.t

val saturate_fsms : Fsm.fsm -> Fsm.fsm -> fsm_pair

val handle_run_params :
  run_params -> bool -> Fsm.Merge.merged_fsm_result_kind

val reachable_blocks :
  Fsm.States.t Fsm.Actions.t ->
  Fsm.Partition.t ->
  Fsm.Partition.t option

val add_to_block_option :
  Fsm.state -> Fsm.States.t option -> Fsm.States.t option

val split_block :
  Fsm.States.t ->
  Fsm.action ->
  Fsm.States.t Fsm.Actions.t Fsm.Edges.t ->
  Fsm.Partition.t ->
  Fsm.States.t * Fsm.States.t option

val update_if_changed : unit

val iterate :
  Fsm.Alphabet.t ->
  Fsm.States.t Fsm.Actions.t Fsm.Edges.t ->
  Fsm.Partition.t ref ->
  bool ref ->
  bool ->
  unit

val run : ?weak:bool -> run_params -> result_kind
