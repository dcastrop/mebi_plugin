type 'a tree = Node of 'a * 'a tree list
type t = (int * int) tree

val eq : t -> t -> bool
val compare : t -> t -> int
val pstr : t -> string
