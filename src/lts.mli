type t = {
  init : Model_state.t option;
  states : Model.States.t;
  transitions : Model.Transitions.t;
  info : Utils.model_info option;
}

val create :
  Model_state.t option ->
  Model.States.t ->
  Model.Transitions.t ->
  Utils.model_info option ->
  t

val add_state : t -> Model_state.t -> t
val add_state_list : t -> Model_state.t list -> t
val add_states : t -> Model.States.t -> t

val add_transition :
  t ->
  Model_state.t ->
  Model_label.t ->
  Model_state.t ->
  Model_label.meta option ->
  t

val add_transition_from_action :
  t -> Model_state.t -> Model_action.t -> Model_state.t -> t

val pstr :
  ?skip_leading_tab:bool -> ?indents:int -> t -> string
