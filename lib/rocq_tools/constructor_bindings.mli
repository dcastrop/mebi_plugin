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
      ([Bindings.S.extract]), read from its source, label and target.

      Raises nothing directly; when run, propagates whatever
      [Bindings.S.extract] raises, and {!Rocq_utils}'s errors for a
      constructor whose type is not an application of its LTS. *)
  val extract_info : ind -> t list mm

  (** Raised by {!get_bound_term}: following a path into a term that is not
      an application. *)
  exception BindingInstruction_NotApp of EConstr.t

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
