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
  ?meta:Model_label.meta option ->
  t ->
  Model_state.t ->
  Model_label.t ->
  Model_state.t ->
  t

val add_action_list :
  t ->
  (Model_state.t
  * Model_label.t
  * Model_state.t
  * Model_label.meta option)
  list ->
  t

val get_actions_from :
  t -> Model_state.t -> Model.States.t Model.Actions.t

val has_state : t -> Model_state.t -> bool
val max_revisit_num : int

val can_revisit :
  Model_state.t -> (Model_state.t, int) Hashtbl.t -> bool

val log_visit :
  Model_state.t -> (Model_state.t, int) Hashtbl.t -> unit

exception CannotSaturateActionWithNoNamedAction of unit

val annotate_action :
  Model_action.annotation_pair ->
  Model_action.t option ->
  Model_action.annotation ->
  Model_action.t

exception
  CannotSaturateActionsWithUnknownVisibility of Model_action.t

val get_annotated_actions :
  t ->
  (Model_state.t, int) Hashtbl.t ->
  Model_action.annotation ->
  Model.States.t ->
  Model_action.t option ->
  Model.action_pair list ->
  Model.action_pair list

val saturate_actions :
  t ->
  Model_state.t ->
  Model.States.t Model.Actions.t ->
  Model.States.t Model.Actions.t

val saturate_edges : t -> t
val saturate : t -> t

type pair = t * t

val saturate_pair : pair -> pair

exception StateOriginNotFound of (pair * Model_state.t)

val state_origin_opt : pair -> Model_state.t -> int option
val state_origin : pair -> Model_state.t -> int
val merge : pair -> t

val pstr :
  ?skip_leading_tab:bool -> ?indents:int -> t -> string
