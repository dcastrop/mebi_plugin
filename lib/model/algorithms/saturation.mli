(** {i See {!Model.S.Saturation}.} *)
module type S = sig
  type state
  type states
  type labels
  type edgemap

  (** [edges labels states old_edges] returns a saturated [edgemap], paired
      with the states that now have no outgoing actions.

      Implemented by silent closure rather than by enumerating paths -- see
      the implementation, and [ASSISTED-CHANGES.md]'s 2026-09-28 entry for
      why the previous depth-first version was both exponential and an
      under-approximation. *)
  val edges : labels -> states -> edgemap -> edgemap * states
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type trees = Base.Trees.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
