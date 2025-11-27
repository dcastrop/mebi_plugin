module Tree : sig
  type 'a tree = Node of 'a * 'a tree list
  type node = Mebi_setup.Enc.t * int
  type t = node tree

  val add : t -> t -> t
  val add_list : t -> t list -> t list
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val minimize : t -> node list
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
