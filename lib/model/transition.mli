module type S = sig
  type state
  type label
  type tree
  type annotation

  type t =
    { from : state
    ; goto : state
    ; label : label
    ; tree : tree option
    ; annotation : annotation option
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (Label : Label.S with type base = Base.t)
    (Annotation : Annotation.S with type label = Label.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type tree = Base.Tree.t
   and type annotation = Annotation.t
