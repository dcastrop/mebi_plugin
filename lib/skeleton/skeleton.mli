(** From the user's rules to a vector, for one start term
    ([doc/skeleton.md]).

    Generic: everything comes from the user's rules and the start term.

    - A constructor is {b structural} if some rule keeps it at the same
      place in its target (CCS's [par]; Proc's [cpar], kept by [do_parl]
      although [do_par_end] dissolves it).
    - A term splits canonically into a {b shape} (its structural
      constructors on top) and the values of its {b slots} below: a slot's
      value never starts with a structural constructor.
    - The state is a shape plus its slots' values: the shape is a finite
      control component. A {b summand} is a rule chain that can fire from a
      shape; its {b target} says which shape results and where each slot's
      value comes from: moved from a source slot, or computed (by the
      explicit route) from some.
    - The reachable shapes are found by following the summands' targets,
      up to a bound. A value computed into a slot whose type has structural
      constructors may need splitting at run time: such summands are marked
      [dynamic]. *)

(** a shape, as the container representation has it ({!Container}) *)
type shape = Container.shape =
  | Slot of int
  | Node of node

and node = Container.node =
  { cons : Names.constructor
  ; univ : UVars.Instance.t
  ; params : Constr.t list
  ; kids : shape list
  }

(** Where a target slot's value comes from. *)
type origin =
  | Moved of int (** the source slot's value, unchanged *)
  | Computed of int list (** computed from these source slots *)

type target =
  | TSlot of origin
  | TNode of node_t

and node_t =
  { tcons : Names.constructor
  ; tuniv : UVars.Instance.t
  ; tparams : Constr.t list
  ; tkids : target list
  }

type summand =
  { chain : string list
    (** rule names, outermost first; [<lts>] a step the explicit route
        computes *)
  ; reads : int list (** source slots read *)
  ; target : target
  ; lhead : Names.constructor option
    (** the label's head constructor, where the rules fix it *)
  ; dynamic : bool
    (** a computed value may need splitting into structure at run time *)
  }

type shape_info =
  { shape : shape (** slots numbered 0, 1, ... left to right *)
  ; summands : summand list
  }

type t =
  { lts : Names.inductive
  ; structural : Names.constructor list
  ; never_kept : Names.constructor list
    (** constructors in rules' sources that no rule keeps *)
  ; start : shape
  ; values : (int * Constr.t) list (** the start shape's slots' values *)
  ; shapes : shape_info list (** the reachable shapes, the start's first *)
  ; complete : bool (** false if [max_shapes] was reached *)
  }

(** [build env sigma ?max_shapes lts t0]: the shapes of the (closed) start
    term [t0] for [lts], at most [max_shapes] (default 200). Raises what
    [Rules.of_lts] raises. *)
val build
  :  Environ.env
  -> Evd.evar_map
  -> ?max_shapes:int
  -> Names.inductive
  -> Constr.t
  -> t

(** [pr env sigma s]: the constructors' classes, the start shape and its
    values, the reachable shapes (at most 15) and the locality. Raises
    nothing. *)
val pr : Environ.env -> Evd.evar_map -> t -> Pp.t

(** {1 Classifying summands} *)

(** A summand is {b local} if its target keeps the source's shape (a step
    below the shape: values change, the structure does not), {b structural} if it changes the shape by moving or dropping values
    (CCS-like [0 | 0 -> 0], Proc's [do_par_end], [do_assoc]), {b dynamic}
    if a value it computes may itself have structure, so that the target's
    shape is known only when the value is (a process that becomes a
    parallel composition). *)
type kind =
  | Local
  | Structural
  | Dynamic

(** [kind src sm]: the class of the summand [sm] of the shape [src].
    Raises nothing. *)
val kind : shape -> summand -> kind

(** {1 Reorderings: structural congruence written as silent rules} *)

(** [reorderings env sigma lts]: the rules of [lts] that only reorder,
    structural congruence written as silent steps (Proc's [do_comm],
    [do_assocl], [do_assocr]; any swap or rotation): no premise, the label
    [None], the source a structure over variables each once, the target
    the same variables each once, a different term; kept only if the
    inverse rule (up to the names of variables) is among them too, so that
    the reorderings are reversible. Raises what [Rules.of_lts] raises. *)
val reorderings
  :  Environ.env
  -> Evd.evar_map
  -> Names.inductive
  -> Names.Id.t list

(** [set_congruence lts names]: the rules of [lts] to take as its
    structural congruence, given by hand ([MeBi Congruence]), instead of
    {!reorderings}. Raises nothing. *)
val set_congruence : Names.inductive -> Names.Id.t list -> unit

(** [congruence env sigma lts]: the rules given by hand for [lts], or
    else its {!reorderings}; [true] if given by hand. Raises what
    [Rules.of_lts] raises. *)
val congruence
  :  Environ.env
  -> Evd.evar_map
  -> Names.inductive
  -> Names.Id.t list * bool

(** {1 Splitting terms, for the symbolic route (Phase 2)} *)

(** [structural_of env sigma lts]: [lts]'s structural constructors. Raises
    what [Rules.of_lts] raises. *)
val structural_of
  :  Environ.env
  -> Evd.evar_map
  -> Names.inductive
  -> Names.constructor list

(** [split env structural t]: the closed term [t]'s shape (slots numbered
    0, 1, ... left to right) and its slots' values, in order: the canonical
    split, through [structural]. Raises nothing. *)
val split
  :  Environ.env
  -> Names.constructor list
  -> Constr.t
  -> shape * Constr.t list

(** [rebuild s values]: the term of shape [s] with slot [i] holding
    [values.(i)], the inverse of {!split}. Raises [Invalid_argument] if
    [values] is too short. *)
val rebuild : shape -> Constr.t array -> Constr.t

(** [key s]: a string identifying [s]'s structure (slots unnumbered). *)
val key : shape -> string

(** [slots s]: [s]'s slot numbers, left to right. *)
val slots : shape -> int list

(** [summands_of env sigma lts s]: the summands of [lts] from the shape [s]
    (its slots renumbered 0, 1, ...). Raises what [Rules.of_lts] raises. *)
val summands_of
  :  Environ.env
  -> Evd.evar_map
  -> Names.inductive
  -> shape
  -> summand list

(** [target_slots t]: where each of the target's slots comes from, left to
    right. *)
val target_slots : target -> origin list

(** [target_shape t]: the target's shape (slots all numbered 0: use
    {!key}). *)
val target_shape : target -> shape
