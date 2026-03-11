module type S = sig
  type label
  type labels

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val labelled : t -> label -> t
  val labels : t -> labels
end

module Make
    (Log : Logger.S)
    (Label : Label.S)
    (Labels : Labels.S with type elt = Label.t)
    (Action : Action.S with type label = Label.t) :
  S with type elt = Action.t and type label = Label.t and type labels = Labels.t
