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

(** [module WIP] is a lightweight counterpart of [Note.t] that forms some "work-in-progress" [Annotation.t]. Once we stop saturating an action, we check if we are able to yield a new saturated action and convert the [wip list] to an [Annotation.t].
*)
module Make
    (Base : Base_term.S)
    (C : Components.S with type trees = Base.Trees.t) :
  S
  with type state = C.State.t
   and type label = C.Label.t
   and type annotation = C.Annotation.t
   and type trees = C.trees
   and type action = C.Action.t
