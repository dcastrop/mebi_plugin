type t = {
  init : Model.State.t option;
  mutable terminals : Model.States.t;
  mutable alphabet : Model.Alphabet.t;
  mutable states : Model.States.t;
  mutable edges : Model.States.t Model.Actions.t Model.Edges.t;
  info : Model.Info.t option;
}

val to_model : t -> Model.t

val create :
  Model.State.t option ->
  Model.States.t ->
  Model.Alphabet.t ->
  Model.States.t ->
  Model.States.t Model.Actions.t Model.Edges.t ->
  Model.Info.t option ->
  t

val create_from : Model.t -> t
val clone : t -> t
val update_info : t -> t
val add_action : t -> Model.Action.t -> t
val add_action_list : t -> Model.Action.t list -> t
val add_state : t -> Model.State.t -> t
val add_state_list : t -> Model.State.t list -> t
val add_states : t -> Model.States.t -> t

val add_edge :
  t -> Model.State.t -> Model.Action.t -> Model.State.t -> t

val add_edge_list : t -> Model.Edge.t list -> t

val add_edge_from_label :
  ?meta:Model.Action.MetaData.t option ->
  t ->
  Model.State.t ->
  Model.Alphabet.elt ->
  Model.State.t ->
  t

val add_edges_from_label_list :
  t ->
  (Model.State.t
  * Model.Alphabet.elt
  * Model.State.t
  * Model.Action.MetaData.t option)
  list ->
  t

val get_actions_from :
  t -> Model.State.t -> Model.States.t Model.Actions.t

val get_states_reachable_from :
  t -> Model.State.t -> Model.States.t

val has_state : t -> Model.State.t -> bool
val _prune_unreachable_edges : t -> t
val _prune_unreachable_states : t -> t
val max_visit_num : int

val can_revisit :
  Model.State.t -> (Model.State.t, int) Hashtbl.t -> bool

val log_visit :
  Model.State.t -> (Model.State.t, int) Hashtbl.t -> unit

exception
  CannotSaturateActionsWithUnknownVisibility of Model.Action.t

val resolve_saturated_action :
  ?named:Model.Action.t option ->
  Model.ActionPairs.t ->
  Model.State.t ->
  Model.Action.annotation ->
  Model.ActionPairs.t

val saturate_action_from :
  ?named:Model.Action.t option ->
  Model.ActionPairs.t ->
  Model.State.t ->
  Model.Action.annotation ->
  (Model.State.t, int) Hashtbl.t ->
  Model.States.t Model.Actions.t Model.Edges.t ->
  Model.ActionPairs.t

val saturate_dest_actions :
  ?named:Model.Action.t option ->
  Model.ActionPairs.t ->
  Model.States.t ->
  Model.Action.annotation ->
  (Model.State.t, int) Hashtbl.t ->
  Model.States.t Model.Actions.t Model.Edges.t ->
  Model.ActionPairs.t

val saturate_actions_from :
  ?named:Model.Action.t option ->
  Model.ActionPairs.t ->
  Model.State.t ->
  Model.States.t Model.Actions.t ->
  Model.Action.annotation ->
  (Model.State.t, int) Hashtbl.t ->
  Model.States.t Model.Actions.t Model.Edges.t ->
  Model.ActionPairs.t

val saturate_edges_from :
  ?named:Model.Action.t option ->
  Model.States.t Model.Actions.t Model.Edges.t ->
  Model.State.t ->
  Model.States.t Model.Actions.t ->
  Model.ActionPairs.t

val saturate_edges :
  t -> Model.States.t Model.Actions.t Model.Edges.t

val saturate : t -> t

type pair = t * t

val saturate_pair : pair -> pair

exception StateOriginNotFound of (pair * Model.State.t)

val state_origin_opt : pair -> Model.State.t -> int option
val state_origin : pair -> Model.State.t -> int
val merge : pair -> t
val saturate_and_merge : ?weak:bool -> pair -> pair * t

val to_string :
  ?pstr:bool ->
  ?skip_leading_tab:bool ->
  ?indents:int ->
  t ->
  string

val pstr :
  ?skip_leading_tab:bool -> ?indents:int -> t -> string
