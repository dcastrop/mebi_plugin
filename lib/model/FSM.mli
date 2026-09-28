(** {i See {!Model.S.FSM}.} *)
module type S = sig
  (** See {!Model.S.State.t} *)
  type state

  (** See {!Model.S.States.t} *)
  type states

  (** See {!Model.S.Labels.t} *)
  type labels

  (** See {!Model.S.EdgeMap.t'} *)
  type edgemap

  (** See {!Model.S.Info.t} *)
  type info

  (** See {!Model.S.LTS.t} *)
  type lts

  type t =
    { init : state option
      (** Initial state (see {!Model.S.State.t}). {i {b Note:} we use an [option] type as this is [None] when we {!val:merge} two FSMs.}.
      *)
    ; alphabet : labels
      (** {!Model.S.Labels.t} that may be found in the {!Model.S.Action}s of {!field:edges}.
      *)
    ; states : states (** {!Model.S.States.t} of the system. *)
    ; edges : edgemap
      (** See {!Model.S.EdgeMap.t'} for the system. Maps from a {!Model.S.State.t} to a {!Model.S.ActionMap.t'} (which in turn maps from an {!Model.S.Action.t} to a {!Model.S.States.t} of {i destinations}).
      *)
    ; terminals : states
      (** Subset of {!field:states} for states with no {b outgoing edges}, i.e., states that are {i {b not a key}} in {!field:edges}.
      *)
    ; info : info (** {!Model.S.Info.t} of the system. *)
    }

  include Json.S with type k = t (** @closed *)

  (** Converts a given {!Model.S.LTS.t} to an FSM {!t}. *)
  val of_lts : lts -> t

  (** Merges two FSMs. {i {b Note:} {!field:init} is set to [None], and parts of {!field:info} are lost or made redundant.}
  *)
  val merge : t -> t -> t

  (** Returns [true] if {!Model.S.Info.weak_labels} of {!field:info} is non-empty.
  *)
  val is_weak_mode : t -> bool

  (** Saturates a given FSM. {i See {!Model.S.Saturation}.} *)
  val saturate : ?only_if_weak:bool -> t -> t
end

module Make
    (C : Components.S)
    (LTS :
       LTS.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type transitions = C.EdgeMap.transitions
        and type info = C.Info.t)
    (Saturation :
       Saturation.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t') :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type info = C.Info.t
   and type lts = LTS.t
