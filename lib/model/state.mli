module type S = sig
  type base
  type t = { base : base }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (Log : Logger.S) (Base : Base_term.S) : S with type base = Base.t
