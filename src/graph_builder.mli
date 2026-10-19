module type S = sig
  type t
  type 'a mm
  type enc
  type tree
  type action
  type constructor
  type states

  (** [get_new_constrs g s] is the steps of [g]'s LTS from the state [s],
      each as label, target and derivation tree
      ({!Rocq_monad_utils.S.Unification.collect_valid_constructors}). The
      evars made while matching are discarded afterwards, unless a step
      found has one.

      Raises, when run, as
      {!Rocq_monad_utils.S.Unification.collect_valid_constructors}
      (propagated). *)
  val get_new_constrs : t -> enc -> constructor list mm

  (** [update_to_visit g x] queues [x] to be visited unless it is already a
      state of [g] or has steps in it. Raises nothing. *)
  val update_to_visit : t -> enc -> unit

  (** [update_transitions g s (t, tree) a] adds the step [s -a-> t], derived
      by [tree], to [g]. Raises nothing. *)
  val update_transitions : t -> enc -> enc * tree -> action -> unit

  (** [get_action g l tree] is the action with label [l] (silent or not, by
      {!Graph_type.S.is_silent_label}) and derivation tree [tree]. Raises
      nothing when run. *)
  val get_action : t -> enc -> tree -> action mm

  (** [get_new_states g s] is [s] and the targets of its steps, after adding
      those steps to [g] and queueing the targets not yet explored.

      Raises as {!get_new_constrs}, when run (propagated). *)
  val get_new_states : t -> enc -> states mm

  (** [stop g] is whether [g] has passed its bound: more states, or more
      transitions, than allowed. Raises nothing. *)
  val stop : t -> bool

  (** [build ?stop g] is [g] explored breadth first, one queued state at a
      time ({!get_new_states}), until its queue is empty or [stop] (by
      default {!stop}) holds.

      Raises as {!get_new_states}, when run (propagated). *)
  val build : ?stop:(t -> bool) -> t -> t mm
end

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Model :
       Model.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (G :
       Graph_type.S
       with type enc = Enc.t
        and type tree = Enc.Tree.t
        and type action = Model.Action.t
        and type ind = M.Ind.t
        and module B = M.B
        and module F = M.F
        and type indmap = M.Ind.t M.B.t
        and type 'a mm = 'a M.mm) :
  S
  with type t = G.t
   and type 'a mm = 'a M.mm
   and type enc = Enc.t
   and type tree = Enc.Tree.t
   and type action = Model.Action.t
   and type constructor = M.Constructor.t
   and type states = G.States.t
