(** A constructor's transition: its action, its target and its
    derivation. *)
module type S = sig
  type base
  type tree

  (** [(action, goto, tree)]. *)
  type t = base * base * tree

  include Json.S with type k = t (** @closed *)

  (** [equal a b] is whether [a] and [b] agree in all three parts. *)
  val equal : t -> t -> bool

  (** [compare a b] orders by action, then target, then tree. *)
  val compare : t -> t -> int
end

(** [Make (Base) (Tree)] is {!S} over [Base] and [Tree]. *)
module Make (Base : Base_.S) (Tree : Tree.S with type base = Base.t) :
  S with type base = Base.t and type tree = Tree.t
