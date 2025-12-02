module Minimize : sig
  type result = Model.Fsm.t * Model.Partition.t

  val add_to_block_option :
    Model_state.t ->
    Model.States.t option ->
    Model.States.t option

  val split_block :
    Model.States.t ->
    Model_label.t ->
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

  val run : ?weak:bool -> Model.Fsm.t -> result
  val to_string : result -> string
end

module Bisimilar : sig
  type result = {
    the_fsm_1 : fsm_pair;
    the_fsm_2 : fsm_pair;
    merged_fsm : Model.Fsm.t;
    bisim_states : Model.Partition.t;
    non_bisim_states : Model.Partition.t;
  }

  and fsm_pair = {
    original : Model.Fsm.t;
    saturated : Model.Fsm.t;
  }

  val the_cached_result : result option ref
  val set_the_result : result -> unit

  exception MeBi_Bisim_ResultNotCached of unit

  val get_the_result : unit -> result

  val block_has_shared_origin :
    Model.States.t -> Model.Fsm.t -> Model.Fsm.t -> bool

  val split_bisimilar :
    Model.Partition.t ->
    Model.Fsm.t ->
    Model.Fsm.t ->
    Model.Partition.t * Model.Partition.t

  val run : ?weak:bool -> Model.Fsm.pair -> result
  val result_to_bool : result -> bool
  val to_string : result -> string
end
