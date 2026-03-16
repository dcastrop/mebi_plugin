module type S = sig
  include Set.S
  include Json.S with type k = t

  val non_silent : t -> t
end

module Make : (Log : Logger.S) (Label : Label.S) -> S with type elt = Label.t
