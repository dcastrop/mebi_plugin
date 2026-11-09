(** Terms as containers (Abbott, Altenkirch and Ghani 2005;
    [doc/skeleton.md], "Terms as containers").

    A {i container} [S ◁ P] is a set of shapes [S] and, for each shape [s],
    a set of positions [P(s)]; an element of its extension is a shape and a
    value at each of its positions. The symbolic route sees a state this
    way, in three stages:

    + {b The term language} (this module, {!view}): a closed term is a
      constructor applied to parameters and fields, or something else (a
      value that is not a constructor application). Nothing here depends on
      the rules.
    + {b Refinement by the rules} (this module, {!split}, given the
      structural constructors that [Skeleton] classifies from the rules):
      the {b shape} of a term is its top part made of structural
      constructors; its {b positions} are the paths to where that part
      stops; its {b values} are the subterms there. A value never starts
      with a structural constructor, so the split is canonical: if the
      state type is [μX. F_s(X) + F_v(X)], with [F_s] the structural
      constructors, a term is an element of [⟦F_s*⟧(V)], the free monad
      on [F_s] (itself a container: shapes are trees of structural
      constructors, positions their leaves) applied to the values [V].
    + {b Serialisation} ([lib/learn], [Learn.encode]): a shape becomes a
      number, a position (a path) a vector index, a value a number; a state
      is then a vector [shape; value at each position], as Sylvan's list
      decision diagrams store it.

    Positions are paths, not slot numbers, so that a step that changes the
    shape at one place leaves every value elsewhere at its position: the
    vector changes only below that place. *)

(** a position: the fields followed from the root, by index *)
type path = int list

type shape =
  | Slot of int (** a position, numbered 0, 1, ... left to right *)
  | Node of node

and node =
  { cons : Names.constructor
  ; univ : UVars.Instance.t
  ; params : Constr.t list
  ; kids : shape list
  }

(** {1 Stage 1: the term language} *)

(** [nparams env c]: the parameters of [c]'s inductive. Raises nothing. *)
val nparams : Environ.env -> Names.constructor -> int

(** [view env t]: [t] as a fully applied constructor, its universe
    instance, parameters and fields; [None] if [t] is not one. Raises
    nothing. *)
val view
  :  Environ.env
  -> Constr.t
  -> (Names.constructor * UVars.Instance.t * Constr.t list * Constr.t list)
       option

(** {1 Stage 2: refinement by the structural constructors} *)

(** [split env structural t]: [t]'s shape (positions numbered 0, 1, ...
    left to right) and the value at each position, in order: the canonical
    split. Raises nothing. *)
val split
  :  Environ.env
  -> Names.constructor list
  -> Constr.t
  -> shape * Constr.t list

(** [rebuild s values]: the term of shape [s] with position [i] holding
    [values.(i)], the inverse of {!split}. Raises [Invalid_argument] if
    [values] is too short. *)
val rebuild : shape -> Constr.t array -> Constr.t

(** [key s]: a string identifying [s]'s structure (positions unnumbered).
    Raises nothing. *)
val key : shape -> string

(** [slots s]: [s]'s position numbers, left to right. Raises nothing. *)
val slots : shape -> int list

(** [renumber s]: [s] with its positions numbered 0, 1, ... left to right.
    Raises nothing. *)
val renumber : shape -> shape

(** {1 Positions as paths} *)

(** [leaf_paths s]: the path of each of [s]'s positions, in position order.
    Raises nothing. *)
val leaf_paths : shape -> path list

(** [prefix p q]: [p] is a prefix of [q] (the place [q] is at or below
    [p]). Raises nothing. *)
val prefix : path -> path -> bool

(** [at s p]: the part of [s] at the path [p] (renumbered), if [p] is a
    place of [s]. Raises nothing. *)
val at : shape -> path -> shape option

(** [agree_outside s p s']: [s] and [s'] are the same structure except at
    or below the path [p], which is a place of both. Raises nothing. *)
val agree_outside : shape -> path -> shape -> bool
