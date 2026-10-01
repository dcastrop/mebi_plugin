(** {i See {!Base_term.S.Tree}.} *)
module type S = sig
  (** @canonical Tree.S *)

  (** See {!Base_.S.t}*)
  type base

  module Node : sig
    type t = base * int

    include Json.S with type k = t (** @closed *)

    val compare : t -> t -> int
    val equal : t -> t -> bool
  end

  type 'a tree = N of 'a * 'a tree list
  type t = Node.t tree

  include Json.S with type k = t (** @closed *)

  val equal : t -> t -> bool
  val compare : t -> t -> int

  (** [preorder t] is the sequence of constructors that replays the
      derivation [t]: depth-first, left to right. A node's children are the
      derivations of its constructor's LTS premises, in premise order, and
      applying a constructor leaves those premises as goals in that same
      order, focused on the first -- so this is exactly the order in which
      the proof solver's focused goal asks for constructors. (It replaced
      [minimize], which kept only a node's shortest child, as if children
      were alternatives: backlog item A6.) Alternative derivations live in
      {!Trees}, not within a tree. *)
  val preorder : t -> Node.t list

  (** [size t] is the number of constructors in [t], i.e. the length of
      [preorder t]. *)
  val size : t -> int
end

module Make (Base : Base_.S) : S with type base = Base.t
