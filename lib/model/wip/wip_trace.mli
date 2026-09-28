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

(** [module Trace] ... we keep track of the total sum of traces we have already checked. This is useful for checking if, from a state and action, we have already explored the rest of this trace and so can just use what we have already learned, e.g., if we are in some "subtrace".
*)
module Make
    (C : Components.S)
    (WIP :
       Wip_annotation.S
       with type state = C.State.t
        and type label = C.Label.t
        and type annotation = C.Annotation.t
        and type trees = C.trees) :
  S
  with type state = C.State.t
   and type label = C.Label.t
   and type annotation = C.Annotation.t
   and type wip = WIP.t
