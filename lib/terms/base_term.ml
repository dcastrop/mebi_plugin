module type S = sig
  type t

  include Json.S with type k = t

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

module Make (Log : Logger.S) (X : Args) : S with type t = X.t = struct
  module Base : Base_.S with type t = X.t = struct
    type t = X.t

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
  end

  let hash = X.hash

  include Base
  module Tree = Tree.Make (Log) (Base)
  module Trees = Trees.Make (Log) (Tree)
  module Constructor_tree = Constructor_tree.Make (Log) (Base) (Tree)
  module Constructor_trees = Constructor_trees.Make (Log) (Constructor_tree)
end
