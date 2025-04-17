type 'a tree = Node of 'a * 'a tree list
type t = int tree

val eq : t -> t -> bool
val pstr : t -> string
