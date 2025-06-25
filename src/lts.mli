type t =
  { init : Model.State.t option
  ; terminals : Model.States.t
  ; alphabet : Model.Alphabet.t
  ; states : Model.States.t
  ; transitions : Model.Transitions.t
  ; info : Model.Info.t option
  }

val to_model : t -> Model.t

val create
  :  Model.State.t option
  -> Model.States.t
  -> Model.Alphabet.t
  -> Model.States.t
  -> Model.Transitions.t
  -> Model.Info.t option
  -> t

val create_from : Model.t -> t
val add_action : t -> Model.Action.t -> t
val add_action_list : t -> Model.Action.t list -> t
val add_state : t -> Model.State.t -> t
val add_state_list : t -> Model.State.t list -> t
val add_states : t -> Model.States.t -> t

val add_transition
  :  t
  -> Model.State.t
  -> Model.Alphabet.elt
  -> Model.State.t
  -> Model.Action.MetaData.t option
  -> t

val add_transition_from_action
  :  t
  -> Model.State.t
  -> Model.Action.t
  -> Model.State.t
  -> t

val to_string
  :  ?pstr:bool
  -> ?skip_leading_tab:bool
  -> ?indents:int
  -> t
  -> string

val pstr : ?skip_leading_tab:bool -> ?indents:int -> t -> string
