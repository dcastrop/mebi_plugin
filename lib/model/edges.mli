module type S = sig
  type label

  include Set.S
  include Json.S with type k = t

  val labelled : t -> label -> t
end

module Make (Log : Logger.S) (Edge : Edge.S) :
  S with type elt = Edge.t and type label = Edge.label
