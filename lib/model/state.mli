(** {i See {!Model.S.State}.} *)
module type S = sig
  (** A {!Base_term.S.t} *)
  type base

  (** Simple wrapper type for representing states. {i (Currently just a single field {!field:base}, but during development has also been extended to store other useful information.)}
  *)
  type t =
    { base : base
      (** The only field used in functions {!equal}, {!compare} and {!hash}. *)
    }

  include Json.S with type k = t (** @closed *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (Log : Logger.S) (Base : Base_term.S) : S with type base = Base.t
