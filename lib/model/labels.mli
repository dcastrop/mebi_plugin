module type S = sig
  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val non_silent : t -> t
end

module Make : (Log : Logger.S) (Label : Label.S) -> S with type elt = Label.t
