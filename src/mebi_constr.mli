module Tree : sig
  type 'a tree = Node of 'a * 'a tree list
  type t = (Mebi_setup.Enc.t * int) tree

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val pstr : t -> string
end
type t = EConstr.t * EConstr.t * Tree.t
