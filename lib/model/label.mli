module type S = sig
  type base

  type t =
    { base : base
    ; is_silent : bool option
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val is_silent : t -> bool
end

module Make (Log : Logger.S) (Base : Base_term.S) : S with type base = Base.t
