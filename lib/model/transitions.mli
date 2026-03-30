module type S = sig
  type labels

  include Set.S (** @closed *)

  include Json.S with type k = t (** @closed *)

  val labels : t -> labels
end

module Make
    (Log : Logger.S)
    (Labels : Labels.S)
    (Transition : Transition.S with type label = Labels.elt) :
  S with type elt = Transition.t and type labels = Labels.t
