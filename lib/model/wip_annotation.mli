module type S = sig
  type state
  type label
  type trees
  type annotation
  type action

  type t =
    { from : state
    ; via : label
    ; trees : trees
    }

  include Json.S with type k = t (** @closed *)

  val is_silent : t -> bool
  val is_named : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val create : state -> action -> t

  exception IsEmptyList

  val list_to_annotation : state -> t list -> annotation
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (Label : Label.S with type base = Base.t)
    (Note :
       Annotation_note.S
       with type state = State.t
        and type label = Label.t
        and type trees = Base.Trees.t)
    (Annotation : Annotation.S with type label = Label.t and type note = Note.t)
    (Action :
       Action.S
       with type label = Label.t
        and type annotation = Annotation.t
        and type trees = Base.Trees.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type annotation = Annotation.t
   and type trees = Base.Trees.t
   and type action = Action.t
