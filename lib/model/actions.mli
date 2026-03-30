module type S = sig
  type label
  type labels

  include Set.S (** @closed *)

  include Json.S with type k = t (** @closed *)

  val labelled : t -> label -> t
  val labels : t -> labels
end

module Make
    (Log : Logger.S)
    (Label : Label.S)
    (Labels : Labels.S with type elt = Label.t)
    (Action : Action.S with type label = Label.t) :
  S with type elt = Action.t and type label = Label.t and type labels = Labels.t
