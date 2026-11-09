(** The constructors of an LTS, read as rules (S2,
    [doc/s2-linearisation.md]).

    An LTS is an inductive [M -> option A -> M -> Prop] without parameters
    ({!Rocq_utils.lts_shaped}). Each of its constructors is one rule: for
    all values of its variables, if its premises hold, its source steps to
    its target with its label. A premise is either a step of an LTS, or a
    proposition with an instance of stdlib's [Decidable] class
    ([Stdlib.Classes.DecidableClass]), whose boolean witness the symbolic
    side uses later.

    Terms keep Rocq's own variable numbering: [src], [lbl], [tgt] and every
    premise are over the whole of [ctx] (de Bruijn indices). *)

type premise =
  | Step of
      { lts : Names.inductive
      ; src : Constr.t
      ; lbl : Constr.t
      ; tgt : Constr.t
      } (** a step of an LTS, e.g. [step p l p'] *)
  | Decide of
      { prop : Constr.t
      ; witness : Constr.t
      }
  (** a decidable proposition, e.g. [allowed ns l = true], with its
      [Decidable_witness], a boolean over the rule's variables *)
  | Opaque of Constr.t
  (** any other proposition, e.g. [exists k, k = n]: the rules cannot
      evaluate it, so a step through it comes from the explicit route,
      which searches for its derivation *)

type rule =
  { name : Names.Id.t (** the constructor's name *)
  ; ctx : Constr.rel_context
    (** every binder, variables and premises, innermost first, as Rocq
        stores a constructor's type *)
  ; vars : (Names.Name.t * Constr.types) list
    (** the variables (the binders that are not premises), outermost
        first *)
  ; premises : premise list (** outermost first *)
  ; src : Constr.t
  ; lbl : Constr.t
  ; tgt : Constr.t
  }

(** An LTS and its rules, one per constructor, in order. *)
type lts =
  { ind : Names.inductive
  ; rules : rule list
  }

(** Raised by {!of_lts}: [ind] is not an inductive [M -> option A -> M -> Prop] without parameters.
*)
exception Not_an_lts of Names.inductive

(** Raised by {!of_lts}: stdlib's [Decidable] class is not loaded
    ([theories/loader.v] Requires it). *)
exception Decidable_not_loaded

(** [of_lts env sigma ind] is the rules of [ind], then of every LTS its
    rules' step premises use, transitively, first met first.

    @raise Not_an_lts if [ind], or an LTS a premise steps by, is not one
    @raise Decidable_not_loaded
      if the [Decidable] class is not loaded (all raised here). *)
val of_lts : Environ.env -> Evd.evar_map -> Names.inductive -> lts list

(** [pr env sigma ltss] prints the rules, for [Print MeBi Rules]: per LTS
    its name and type, then per rule its name and variables, its premises
    (a decidable one marked [[Decidable]]) and its conclusion, as
    [source --label--> target]. Raises nothing. *)
val pr : Environ.env -> Evd.evar_map -> lts list -> Pp.t
