module Tree : sig
  module Node : sig
    type t = Mebi_setup.Enc.t * int

    val to_string : ?args:Utils.Strfy.style_args -> t -> string
  end

  type 'a tree = Node of 'a * 'a tree list
  type t = Node.t tree

  val add : t -> t -> t
  val add_list : t -> t list -> t list
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val minimize : t -> Node.t list

  exception Mebi_constr_Tree_EmptyList of unit

  val min : t list -> Node.t list
  val to_string : ?args:Utils.Strfy.style_args -> t -> string
  val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
end

type t = Evd.econstr * Evd.econstr * Tree.t

val to_string
  :  Environ.env
  -> Evd.evar_map
  -> ?args:Utils.Strfy.style_args
  -> t
  -> string
