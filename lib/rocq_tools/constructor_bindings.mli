(** Explicit bindings for applying an LTS constructor in a proof: for each
    constructor, where each of its binders sits in its source, label and
    target ({!Bindings}), and, given the terms of a concrete step, the
    [with] bindings that fix those binders. *)
module type S = sig
  type 'a mm
  type ind
  type instructions
  type bindings
  type constrmap

  (** One constructor: its index (as [constructor i] counts, from 1), name
      and binder locations. *)
  type t =
    { index : int
    ; name : string
    ; bindings : bindings
    }

  include Json.S with type k = t (** @closed *)

  (** [extract_info ind] is, for each constructor of the LTS [ind] (the
      last first), its index, name and binder locations
      ({!Bindings.extract}), read from its source, label and target.

      Raises nothing directly; when run, propagates whatever
      {!Bindings.extract} raises, and [Rocq_utils]'s errors for a
      constructor whose type is not an application of its LTS. *)
  val extract_info : ind -> t list mm

  (** [get_quantified_hyp n] is the binder name [n] as a quantified
      hypothesis for a [with] binding; an anonymous binder becomes [AnonHyp 0] (a placeholder). Raises nothing.
  *)
  val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

  (** Raised by {!get_bound_term}: following a path into a term that is not
      an application. *)
  exception BindingInstruction_NotApp of EConstr.t

  (** Raised by {!get_bound_term}: the path is [Undefined]; carries the term
      it was applied to, twice (see {!get_bound_term}). *)
  exception BindingInstruction_Undefined of EConstr.t * EConstr.t

  (** Raised by {!get_bound_term}: the path's argument index is past the
      application's arguments. *)
  exception BindingInstruction_IndexOutOfBounds of EConstr.t * int

  (** Raised by {!get_bound_term}: the application's head is not the one the
      path expects. *)
  exception BindingInstruction_NEQ of EConstr.t * Constr.t

  (** [get_bound_term x path] is the subterm of [x] at [path]: at each [Arg]
      step, [x] must be an application whose head is the step's [root], and
      the path continues into argument [index]; [Done] is [x] itself.

      @raise BindingInstruction_Undefined
        if [path] is [Undefined], at once when applied, or when run if
        reached deeper, naming [x] as the outer term (raised here).
      @raise BindingInstruction_NotApp
        when run, if a step meets a term that
        is not an application (raised here).
      @raise BindingInstruction_NEQ
        when run, if an application's head is not
        the step's [root] (raised here).
      @raise BindingInstruction_IndexOutOfBounds
        when run, if [index] is past
        the arguments (raised here). *)
  val get_bound_term : EConstr.t -> instructions -> EConstr.t mm

  (** [get_explicit_bindings (x, map)] is a [with] binding for each binder
      [map] locates in [x] (none without [map]), each binder bound to its
      subterm of [x] ({!get_bound_term}).

      Raises whatever {!get_bound_term} raises (propagated). *)
  val get_explicit_bindings
    :  EConstr.t * constrmap option
    -> EConstr.t Tactypes.explicit_bindings mm

  (** [get from action goto b] is the bindings for applying a constructor
      whose binder locations are [b], at a step from [from] (with label
      [action] and target [goto], when known): none for [No_Bindings];
      otherwise the explicit bindings of each of [from], [action] and
      [goto] that has a map ({!get_explicit_bindings}), each binder bound
      once (in a consistent proof every occurrence gives it the same
      value), or none if there are no binders.

      Raises whatever {!get_explicit_bindings} raises (propagated). *)
  val get
    :  EConstr.t
    -> EConstr.t option
    -> EConstr.t option
    -> bindings
    -> EConstr.t Tactypes.bindings mm
end

module Make
    (M : Rocq_monad_utils.S)
    (Bindings : Bindings.S with type 'a mm = 'a M.mm) :
  S
  with type 'a mm = 'a M.mm
   and type ind = M.Ind.t
   and type instructions = Bindings.Instructions.t
   and type bindings = Bindings.t
   and type constrmap = Bindings.ConstrMap.t'
