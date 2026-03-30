module type S = sig
  type t

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
end
