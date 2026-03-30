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

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (Labels : Labels.S)
    (Transitions : Transitions.S with type labels = Labels.t)
    (Info : Info.S with type base = State.base and type labels = Labels.t) :
  S
  with type state = State.t
   and type states = States.t
   and type labels = Labels.t
   and type transitions = Transitions.t
   and type info = Info.t
