(** {i See {!Model.S.LTS}.} *)
module type S = sig
  (** See {!Model.S.State.t} *)
  type state

  (** See {!Model.S.States.t} *)
  type states

  (** See {!Model.S.Labels.t} *)
  type labels

  (** See {!Model.S.Transitions.t} *)
  type transitions

  (** See {!Model.S.Info.t} *)
  type info

  (** LTS type *)
  type t =
    { init : state option
      (** Initial state. {i {b Note:} is an [option] type to mirror {!Model.S.FSM.t.init}, which uses [None] when two {!Model.S.FSM.t} are {b merged}}.
      *)
    ; alphabet : labels
      (** {!Model.S.Labels.t} that may be found in {!field:transitions}. *)
    ; states : states (** {!Model.S.States.t} of the system. *)
    ; transitions : transitions (** {!Model.S.Transitions.t} for the system. *)
    ; terminals : states
      (** Subset of {!field:states} for states with no {b outgoing edges}, i.e., that do not appear in any {!Model.S.Transition.from} in {!field:transitions}.
      *)
    ; info : info (** {!Model.S.Info.t} of the system. *)
    }

  include Json.S with type k = t (** @closed *)
end

module Make (C : Components.S) :
  S
  with type state = C.State.t
   and type states = C.States.t
   and type labels = C.Labels.t
   and type transitions = C.Transitions.t
   and type info = C.Info.t
