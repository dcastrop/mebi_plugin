(** {i See {!Model.S.LTS}.} *)
module type S = sig
  (** See {!Components.S.State.t} *)
  type state

  (** See {!Components.S.State.Set.t} *)
  type states

  (** See {!Components.S.Label.Set.t} *)
  type labels

  (** See {!Components.S.Transition.Set.t} *)
  type transitions

  (** See {!Components.S.Info.t} *)
  type info

  (** LTS type *)
  type t =
    { init : state option
      (** Initial state. {i {b Note:} is an [option] type to mirror [FSM.S.t.init], which uses [None] when two [FSM.S.t] are {b merged}}.
      *)
    ; alphabet : labels
      (** {!Components.S.Label.Set.t} that may be found in {!field:transitions}.
      *)
    ; states : states (** {!Components.S.State.Set.t} of the system. *)
    ; transitions : transitions
      (** {!Components.S.Transition.Set.t} for the system. *)
    ; terminals : states
      (** Subset of {!field:states} for states with no {b outgoing edges}, i.e., that do not appear in any [Transition.from] in {!field:transitions}.
      *)
    ; info : info (** {!Components.S.Info.t} of the system. *)
    }

  include Json.S with type k = t (** @closed *)
end

module Make (C : Components.S) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type transitions = C.Transition.Set.t
   and type info = C.Info.t
