(** {i See {!Model.S.State}.} *)
module type S = sig
  (** @canonical Model.State *)

  type base
  type t = { base : base }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (Log : Logger.S) (Base : Base_term.S) : S with type base = Base.t
