module type S = sig
  module Enc : Encoding.SEncoding

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

module Make : (E : Encoding.SEncoding) -> sig
  module Enc : sig
    type t = E.t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val counter : t ref
    val reset : unit -> unit
    val incr : unit -> t
  end

  module type STreeNode = sig
    type t = E.t * int

    val to_string : t -> string
  end

  module TreeNode : sig
    type t = E.t * int

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
