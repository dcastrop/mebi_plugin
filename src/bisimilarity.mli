type fsm_pair = Fsm.fsm * Fsm.fsm
type run_params = fsm_pair * Fsm.Merge.merged_fsm_result_kind option

type result_params =
  fsm_pair * Fsm.Merge.merged_fsm_result_kind * (Fsm.Partition.t * Fsm.Partition.t)

type result_kind =
  | Pass of result_params
  | Fail of result_params

type state_origins = { s : bool; t : bool }

val origins_of :
  Fsm.States.t ->
  Fsm.States.t * Fsm.States.t ->
  (Fsm.state, Fsm.state) Hashtbl.t ->
  state_origins

val split_bisimilar :
  Fsm.States.t * Fsm.States.t ->
  (Fsm.state, Fsm.state) Hashtbl.t ->
  Fsm.Partition.t ref ->
  Fsm.Partition.t * Fsm.Partition.t

val main_loop :
  Fsm.Alphabet.t ->
  Fsm.States.t Fsm.Actions.t Fsm.Edges.t ->
  Fsm.Partition.t ref ->
  bool ->
  unit

val saturate_fsms : Fsm.fsm -> Fsm.fsm -> fsm_pair
val run : ?weak:bool -> run_params -> result_kind
