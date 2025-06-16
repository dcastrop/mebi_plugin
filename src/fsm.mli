type t = {
  init : Model_state.t option;
  mutable alphabet : Model.Alphabet.t;
  mutable states : Model.States.t;
  mutable edges : Model.States.t Model.Actions.t Model.Edges.t;
  info : Utils.model_info option;
}

val clone : t -> t
val add_label : t -> Model_label.t -> t
val add_label_list : t -> Model_label.t list -> t
val add_state : t -> Model_state.t -> t
val add_state_list : t -> Model_state.t list -> t
val add_states : t -> Model.States.t -> t

val add_edge :
  t -> Model_state.t -> Model_action.t -> Model_state.t -> t

val add_edge_list : t -> Model_edge.t list -> t

val add_action :
  t -> Model_state.t -> Model_label.t -> Model_state.t -> t

val merge : t -> t -> t
