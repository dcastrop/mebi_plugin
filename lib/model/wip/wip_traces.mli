module type S = sig
  type wip

  include Set.S (** @closed *)

  include Json.S with type k = t (** @closed *)

  val get : wip -> t -> t
end

module Make
    (C : Components.S)
    (WIP : Wip_annotation.S with type state = C.State.t)
    (Trace : Wip_trace.S with type state = C.State.t and type wip = WIP.t) :
  S with type elt = Trace.t and type wip = WIP.t
