(** {i See {!Base_term.S.Trees}.} *)
module type S = sig
  (** @canonical Trees.S *)

  (** See {!Base_term.S.Tree.t} *)
  type tree

  include Set.S with type elt = tree (** @closed *)

  include Json.S with type k = t (** @closed *)

  (** Raised by {!min}: the set is empty. *)
  exception EmptyHasNoMin

  (** [min ts] is the derivation of [ts] with the fewest constructors to
      apply ([Tree.size]); on a tie, the least in the set's order.

      @raise EmptyHasNoMin if [ts] is empty (raised here). *)
  val min : t -> tree

  (** [min_opt ts] is {!min}, or [None] if [ts] is empty. Raises
      nothing. *)
  val min_opt : t -> tree option
end

(** [Make (Tree)] is {!S}: sets of [Tree]s. *)
module Make (Tree : Tree.S) : S with type tree = Tree.t
