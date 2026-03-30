module type S = sig
  type wip

  include Set.S (** @closed *)

  include Json.S with type k = t (** @closed *)

  val get : wip -> t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (WIP : Wip_annotation.S with type state = State.t)
    (Trace : Wip_trace.S with type state = State.t and type wip = WIP.t) :
  S with type elt = Trace.t and type wip = WIP.t
