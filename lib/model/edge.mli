module type S = sig
  type state
  type label
  type action

  type t =
    { from : state
    ; goto : state
    ; action : action
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val is_labelled : label -> t -> bool
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (Label : Label.S)
    (Action : Action.S with type label = Label.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type action = Action.t
