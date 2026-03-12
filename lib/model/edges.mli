module type S = sig
  type label

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val labelled : t -> label -> t
end

module Make (Log : Logger.S) (Edge : Edge.S) :
  S with type elt = Edge.t and type label = Edge.label
