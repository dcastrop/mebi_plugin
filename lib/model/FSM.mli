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
    ; fill : (state -> unit) option
      (** [Some f] for an FSM saturated on demand ({!saturate_on_demand}):
          {!field:edges} holds only the states asked about so far, and [f s]
          adds [s]'s. Read such an FSM's edges for [s] only after {!ensure}. *)
    }

  include Json.S with type k = t (** @closed *)

  (** [of_lts l] is the LTS [l] as an FSM: the same states, terminals,
      alphabet and information, its transitions as edges, one action per
      transition. Raises nothing. *)
  val of_lts : lts -> t

  (** [merge a b] is the FSM with the states, terminals, alphabet and edges
      of both [a] and [b] (whose states should be disjoint), with no initial
      state, information merged ({!Model.S.Info.merge}) with fresh counts,
      and not saturated on demand: a merged FSM holds the edges [a] and [b]
      hold now. Raises nothing. *)
  val merge : t -> t -> t

  (** [is_weak_mode x] is whether [x] has weak (silent) labels, so needs
      saturating. Raises nothing. *)
  val is_weak_mode : t -> bool

  (** [saturate ?only_if_weak x] is [x] with every weak action added to its
      edges ({!Model.S.Saturation}), and the states that become terminal
      added to its terminals; [x] itself if it has no weak labels and
      [only_if_weak] (the default). [x]'s edges are copied, not changed.
      Raises nothing. *)
  val saturate : ?only_if_weak:bool -> t -> t

  (** [ensure x s] makes [x.edges] hold [s]'s edges: a no-op unless [x] is
      saturated on demand. Every read of a saturated FSM's edges for one
      state goes through it. Raises nothing. *)
  val ensure : t -> state -> unit

  (** [saturate_on_demand ~budget x] is [saturate x] without the up-front
      cost: each state is saturated when first asked about ({!ensure}), from
      the same code as {!saturate}, so with the same weak actions and
      witnesses. At most about [budget] weak actions are held at a time
      (default 1,000,000), the oldest states dropped first and recomputed if
      asked about again. [terminals] stays [x]'s: states that only become
      terminal by saturating are not found without saturating them. For FSMs
      too large to saturate whole; see [notes/13]. Raises nothing. *)
  val saturate_on_demand : ?budget:int -> t -> t

  (** [rename f x] is [x] with every state [s] replaced by [f s]: states,
      initial state, terminals, and edges (sources and destinations). For an
      FSM as extracted, before saturation: annotations, which name states,
      are left as they are. Raises nothing. *)
  val rename : (state -> state) -> t -> t
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
        and type edgemap = C.EdgeMap.t'
        and type actionmap = C.Action.Map.t') :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type info = C.Info.t
   and type lts = LTS.t
