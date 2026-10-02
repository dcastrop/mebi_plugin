(** {i See {!Model.S.Saturation}.} *)
module type S = sig
  type state
  type states
  type labels
  type edgemap
  type annotation

  (** [edges labels states old_edges] returns a saturated [edgemap], paired
      with the states that now have no outgoing actions.

      Implemented by silent closure rather than by enumerating paths -- see
      the implementation, and [ASSISTED-CHANGES.md]'s 2026-09-28 entry for
      why the previous depth-first version was both exponential and an
      under-approximation. *)
  val edges : labels -> states -> edgemap -> edgemap * states

  (** [silent_paths edges s] is every state [s] reaches by zero or more
      silent steps of [edges] (Milner's [=ε=>], so [s] itself included), each
      with the length and the annotation of a shortest such path ([None] for
      [s] itself). The proof solver answers a silent move with one of these
      when standing still will not do (see {!Product.respond}). *)
  val silent_paths : edgemap -> state -> (state * annotation option * int) list
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
