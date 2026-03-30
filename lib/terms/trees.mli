module type S = sig
  type tree

  include Set.S with type elt = tree
  include Json.S with type k = t

  exception EmptyHasNoMin

  val min : t -> tree
  val min_opt : t -> tree option
end

module Make (Log : Logger.S) (Tree : Tree.S) : S with type tree = Tree.t
