module Tree : sig
  type 'a tree = Node of 'a * 'a tree list
  type t = (Mebi_setup.Enc.t * int) tree

  val add : t -> t -> t
  val add_list : t -> t list -> t list
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val pstr : t -> string
end

type t = Evd.econstr * Evd.econstr * Tree.t

val to_string :
  ?indent:int -> Environ.env -> Evd.evar_map -> t -> string
