module type S = sig
  type t
include Json.S with type k = t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  type e = t

  module Tree : sig
    module Node : sig
      type t = e * int
include Json.S with type k = t
      val compare : t -> t -> int
      val equal : t -> t -> bool
    end

    type 'a tree = N of 'a * 'a tree list
    type t = Node.t tree
include Json.S with type k = t
    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> Node.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> Node.t list
  end

  module Trees : sig
    include Set.S with type elt = Tree.t
include Json.S with type k = t

    exception EmptyHasNoMin

    val min : t -> Tree.t
    val min_opt : t -> Tree.t option
  end

  module Constructor_tree : sig
    type t = e * e * Tree.t
include Json.S with type k = t
  end

  module Constructor_trees : sig
    type t = Constructor_tree.t list
include Json.S with type k = t
  end
end

module Make : (Log : Logger.S)
    (X : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val to_string : t -> string
     end)
    -> S with type t = X.t
