module type S = sig
  type state
  type label
  type annotation
  type wip

  type t =
    { this : wip
    ; next : next option
    }

  and next =
    | Next of t
    | Goto of state

  include Json.S with type k = t (** @closed *)

  val create : wip -> t
  val compare : t -> t -> int
  val compare_next : next -> next -> int

  exception Invalid

  val has_named : ?validate:bool -> t -> bool
  val validate : t -> unit

  exception CouldNotFindGoto

  val get_goto : t -> state

  exception CouldNotFindNamed

  val get_named : t -> label
  val get_named_opt : t -> label option

  exception FailAdd_AlreadyNamed
  exception FailAdd_AlreadyHasGoto

  val add : wip -> t -> t

  exception FailSetGoto_AlreadyHasGoto

  val set_goto : state -> t -> t

  exception FailSeq_AlreadyNamed
  exception FailSeq_AlreadyHasGoto

  val seq : t -> t -> t
  val seq_opt : t option -> t -> t
  val get : wip -> t -> t
  val upto_named : t -> t

  exception GotoNotSet

  val to_annotation : t -> annotation
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
    (WIP :
       Wip_annotation.S
       with type state = State.t
        and type label = Label.t
        and type annotation = Annotation.t
        and type trees = Base.Trees.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type annotation = Annotation.t
   and type wip = WIP.t
