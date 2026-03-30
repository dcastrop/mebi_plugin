module type S = sig
  type constructor_tree
  type t = constructor_tree list

  include Json.S with type k = t (** @closed *)
end

module Make (Log : Logger.S) (Constructor_tree : Constructor_tree.S) :
  S with type constructor_tree = Constructor_tree.t
