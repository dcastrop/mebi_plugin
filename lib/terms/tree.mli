(** {i See {!Base_term.S.Tree}.} *)
module type S = sig
  (** @canonical Tree.S *)

  (** See {!Base_.S.t}*)
  type base

  (** A constructor: its LTS (by encoding) and its index in it. *)
  module Node : sig
    type t = base * int

    include Json.S with type k = t (** @closed *)

    (** [compare a b] orders nodes by LTS, then index. *)
    val compare : t -> t -> int

    (** [equal a b] is whether [a] and [b] are the same constructor. *)
    val equal : t -> t -> bool
  end

  (** A tree: a node and its subtrees. *)
  type 'a tree = N of 'a * 'a tree list

  (** A derivation: the constructor applied at the root, and the
      derivations of its LTS premises, in premise order. *)
  type t = Node.t tree

  include Json.S with type k = t (** @closed *)

  (** [equal a b] is whether [a] and [b] are the same derivation. *)
  val equal : t -> t -> bool

  (** [compare a b] orders derivations by root node, then by their
      subtrees, lexicographically. *)
  val compare : t -> t -> int

  (** [preorder t] is the sequence of constructors that replays the
      derivation [t]: depth-first, left to right. A node's children are the
      derivations of its constructor's LTS premises, in premise order, and
      applying a constructor leaves those premises as goals in that same
      order, focused on the first -- so this is exactly the order in which
      the proof solver's focused goal asks for constructors. (It replaced
      [minimize], which kept only a node's shortest child, as if children
      were alternatives: backlog item A6.) Alternative derivations live in
      {!Trees}, not within a tree. Raises nothing. *)
  val preorder : t -> Node.t list

  (** [size t] is the number of constructors in [t], i.e. the length of
      [preorder t]. Raises nothing. *)
  val size : t -> int
end

(** [Make (Base)] is {!S} over the terms [Base]. *)
module Make (Base : Base_.S) : S with type base = Base.t
