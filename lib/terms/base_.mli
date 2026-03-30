module type S = sig
  (** @canonical Terms.Base.S *)

  type t

  include Json.S with type k = t (** @closed *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
end
