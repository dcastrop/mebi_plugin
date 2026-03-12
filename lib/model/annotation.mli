module type S = sig
  type label
  type note

  type t =
    { this : note
    ; next : t option
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_empty : t -> bool
  val opt_is_empty : ?fail_if_none:bool -> t option -> bool
  val length : t -> int
  val opt_length : ?fail_if_none:bool -> t option -> int
  val shorter : t -> t -> t
  val exists : note -> t -> bool
  val exists_label : label -> t -> bool
  val append : note -> t -> t
  val last : t -> note

  exception CannotDropLastOfSingleton of t

  val drop_last : t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type base = Base.t)
    (Note : Annotation_note.S with type label = Label.t) :
  S with type label = Label.t and type note = Note.t
