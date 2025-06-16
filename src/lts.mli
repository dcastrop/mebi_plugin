
type t = {
  init : Model_state.t option;
  states : Model.States.t;
  transitions : Model_transition.t list;
  info : Utils.model_info option;
}

val add_state : t -> Model_state.t -> t
val add_state_list : t -> Model_state.t list -> t
val add_states : t -> Model.States.t -> t
