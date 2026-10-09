(** Lists of constructors' transitions ({!Constructor_tree}). *)
module type S = sig
  type constructor_tree
  type t = constructor_tree list

  include Json.S with type k = t (** @closed *)
end

(** [Make (Constructor_tree)] is {!S}: lists of [Constructor_tree]s. *)
module Make (Constructor_tree : Constructor_tree.S) :
  S with type constructor_tree = Constructor_tree.t
