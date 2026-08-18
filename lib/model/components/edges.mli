module type S = sig
  type label

  include Set.S (** @closed *)

  include Json.S with type k = t (** @closed *)

  val labelled : t -> label -> t
end

module Make (Edge : Edge.S) :
  S with type elt = Edge.t and type label = Edge.label
