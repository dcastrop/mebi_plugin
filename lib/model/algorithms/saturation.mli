(** {i See {!Model.S.Saturation}.} *)
module type S = sig
  type state
  type states
  type labels
  type edgemap
  type annotation

  (** [edges labels states old_edges] is the saturation of [old_edges]: for
      each state with outgoing edges, its weak actions, paired with the
      states left with none (the new terminals). [labels] and [states] are
      not read.

      Built by silent closure rather than by enumerating paths; see the
      implementation, and [ASSISTED-CHANGES.md], 2026-09-28, for why the
      earlier depth-first version was both exponential and an
      under-approximation.

      Raises nothing. *)
  val edges : labels -> states -> edgemap -> edgemap * states

  (** [silent_paths edges s] is every state [s] reaches by zero or more
      silent steps of [edges] (Milner's [=ε=>], so [s] itself included),
      each with the annotation and the length of a shortest such path
      ([None] and [0] for [s] itself). The proof solver answers a silent
      move with one of these when standing still will not do (see
      [Product.S.respond]).

      Raises nothing. *)
  val silent_paths : edgemap -> state -> (state * annotation option * int) list

  type actionmap

  (** [on_demand old_edges] is a function that saturates one state at a
      time: applied to [s], it is [s]'s weak actions exactly as {!edges}
      would compute them (from the same code), or [None] if it has none.
      Silent closures are shared between calls, in a cache emptied whenever
      it holds [closure_cap] states (default 4096), so memory stays bounded
      however many states are asked about.

      Raises nothing. *)
  val on_demand : ?closure_cap:int -> edgemap -> state -> actionmap option
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type trees = Base.Trees.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type annotation = C.Annotation.t
   and type actionmap = C.Action.Map.t'
