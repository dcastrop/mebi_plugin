(** What the trees over a type of terms need of it: JSON, equality and
    order. Interface only; {!Base_term.Make} builds one. *)
module type S = sig
  (** @canonical Terms.Base.S *)

  type t

  include Json.S with type k = t (** @closed *)

  (** [equal a b] is whether [a] and [b] are the same term. *)
  val equal : t -> t -> bool

  (** [compare a b] orders terms. *)
  val compare : t -> t -> int
end
