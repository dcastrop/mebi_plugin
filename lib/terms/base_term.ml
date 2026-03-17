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

module type Args = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

module Make (Log : Logger.S) (X : Args) : S with type t = X.t = struct
  module Y = struct
    type t = X.t
    type e = t

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Term"

          let json ?(as_elt : bool = false) (x : t) : Yojson.t =
            `String (X.to_string x)
          ;;
        end)

    let equal = X.equal
    let compare = X.compare
    let hash = X.hash
  end

  include Y
  module Tree = Tree.Make (Log) (Y)
  module Trees = Trees.Make (Log) (Tree)
  module Constructor_tree = Constructor_tree.Make (Log) (Y) (Tree)
  module Constructor_trees = Constructor_trees.Make (Log) (Constructor_tree)
end
