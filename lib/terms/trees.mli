(** {i See {!Base_term.S.Trees}.} *)
module type S = sig
  (** @canonical Trees.S *)

  (** See {!Base_term.S.Tree.t} *)
  type tree

  include Set.S with type elt = tree (** @closed *)

  include Json.S with type k = t (** @closed *)

  exception EmptyHasNoMin

  val min : t -> tree
  val min_opt : t -> tree option
end

module Make (Log : Logger.S) (Tree : Tree.S) : S with type tree = Tree.t
