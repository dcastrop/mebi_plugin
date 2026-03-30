module type S = sig
  type base
  type tree
  type t = base * base * tree

  include Json.S with type k = t (** @closed *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make
    (Log : Logger.S)
    (Base : Base_.S)
    (Tree : Tree.S with type base = Base.t) :
  S with type base = Base.t and type tree = Tree.t
