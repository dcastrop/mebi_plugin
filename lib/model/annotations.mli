module type S = sig
  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val extrapolate : elt -> t
end

module Make
    (Log : Logger.S)
    (Note : Annotation_note.S)
    (Annotation : Annotation.S with type note = Note.t) :
  S with type elt = Annotation.t
