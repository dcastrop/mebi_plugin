module Tree : sig
  type 'a tree = Node of 'a * 'a tree list
  type t = (Mebi_setup.Enc.t * int) tree

  val add : t -> t -> t
  val add_list : t -> t list -> t list
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : ?args:Utils.Strfy.style_args -> t -> string
end

type t = EConstr.t * EConstr.t * Tree.t

val to_string :
  Environ.env ->
  Evd.evar_map ->
  ?args:Utils.Strfy.style_args ->
  t ->
  string
