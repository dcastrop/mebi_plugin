module Make : (Enc : Encoding.SEncoding) -> sig
  module type STreeNode = sig
    type t = Enc.t * int

    val to_string : t -> string
  end

  module TreeNode : sig
    type t = Enc.t * int

    val to_string : t -> string
  end

  type 'a tree = Node of 'a * 'a tree list
  type t = TreeNode.t tree

  val add : t -> t -> t
  val add_list : t -> t list -> t list
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val minimize : t -> TreeNode.t list

  exception CannotMinimizeEmptyList of unit

  val min : t list -> TreeNode.t list
  val to_string : t -> string
  val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
end
