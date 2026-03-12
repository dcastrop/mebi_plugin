module type S = sig
  type labels

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val labels : t -> labels
end

module Make
    (Log : Logger.S)
    (Labels : Labels.S)
    (Transition : Transition.S with type label = Labels.elt) :
  S with type elt = Transition.t and type labels = Labels.t
