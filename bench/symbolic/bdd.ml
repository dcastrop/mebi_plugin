(** The BDD operations the prototype needs ({!Bdd_cudd} implements them).

    Variables are numbered from 0, and the variable order is the numbering
    (no reordering), so results are reproducible. *)

module type S = sig
  type t

  val name : string

  (** [init ~vars] starts a fresh manager over [vars] variables. Every
      BDD made before it is invalid afterwards. *)
  val init : vars:int -> unit

  val dtrue : unit -> t
  val dfalse : unit -> t
  val var : int -> t
  val dnot : t -> t
  val dand : t -> t -> t
  val dor : t -> t -> t
  val is_false : t -> bool
  val equal : t -> t -> bool

  (** [cube vs] is the conjunction of the variables [vs], for quantifying. *)
  val cube : int list -> t

  (** [exists c f] is [f] with the variables of the cube [c] quantified. *)
  val exists : t -> t -> t

  (** [and_exists c f g] is [exists c (dand f g)], in one pass. *)
  val and_exists : t -> t -> t -> t

  (** [permute f p] renames variable [i] of [f] to [p.(i)] ([p] a
      permutation of all the manager's variables). *)
  val permute : t -> int array -> t

  (** [size f] is the number of nodes of [f]. *)
  val size : t -> int

  (** [satcount n f] is the number of assignments to [n] variables
      (numbered [0 .. n-1]) satisfying [f]. *)
  val satcount : int -> t -> float

  (** [nodes ()] is the number of nodes the manager holds now; [peak ()]
      the most it has held. *)
  val nodes : unit -> int

  val peak : unit -> int
end
