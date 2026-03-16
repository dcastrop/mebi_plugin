module type S = sig
  include Set.S
  include Json.S with type k = t

  val add_to_opt : elt -> t option -> t

  exception StateHasNoOrigin of (elt * t * t)

  val origin_of_state : elt -> t -> t -> int
  val has_shared_origin : t -> t -> t -> bool
end

module Make : (Log : Logger.S) (State : State.S) -> S with type elt = State.t
