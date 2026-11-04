module type S = sig
  type t
  type 'a mm
  type enc
  type tree
  type action
  type constructor
  type states

  (** [update_to_visit g x] queues [x] to be visited unless it is already a
      state of [g] or has steps in it. Raises nothing. *)
  val update_to_visit : t -> enc -> unit

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
