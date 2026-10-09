(** A type of terms with the trees over it: the derivation trees of
    transitions ({!S.Tree}), sets of alternative derivations ({!S.Trees}),
    and constructors' transitions ({!S.Constructor_tree},
    {!S.Constructor_trees}). Each is printable, comparable and has JSON. *)
module type S = sig
  type t

  include Json.S with type k = t (** @closed *)

  (** [equal a b] is whether [a] and [b] are the same term. *)
  val equal : t -> t -> bool

  (** [compare a b] orders terms. *)
  val compare : t -> t -> int

  (** [hash x] is a hash of [x], consistent with {!equal}. *)
  val hash : t -> int

  (** Derivation trees over these terms. *)
  module Tree : Tree.S with type base = t

  (** Sets of alternative derivations. *)
  module Trees : Trees.S with type tree = Tree.t

  (** A constructor's transition and its derivation. *)
  module Constructor_tree :
    Constructor_tree.S with type base = t and type tree = Tree.t

  (** Lists of those. *)
  module Constructor_trees :
    Constructor_trees.S with type constructor_tree = Constructor_tree.t
end

(** What {!Make} needs of a type of terms: equality, order, a hash and a
    printer. *)
module type Args = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

(** [Make (X)] is {!S} for [X.t]: a term's JSON is [X.to_string] of it,
    as a string. *)
module Make (X : Args) : S with type t = X.t
