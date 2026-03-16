module type S = sig
  include Set.S
  include Json.S with type k = t

  val extrapolate : elt -> t
end

module Make
    (Log : Logger.S)
    (Note : Annotation_note.S)
    (Annotation : Annotation.S with type note = Note.t) :
  S with type elt = Annotation.t
