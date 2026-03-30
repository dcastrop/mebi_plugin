module type S = sig
  type t

  include Json.S with type k = t (** @closed *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  module Tree : Tree.S with type base = t
  module Trees : Trees.S with type tree = Tree.t

  module Constructor_tree :
    Constructor_tree.S with type base = t and type tree = Tree.t

  module Constructor_trees :
    Constructor_trees.S with type constructor_tree = Constructor_tree.t
end

module type Args = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

module Make (Log : Logger.S) (X : Args) : S with type t = X.t
