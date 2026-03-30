module type S = sig
  type t
  type 'a mm
  type enc
  type tree
  type action
  type constructor
  type states

  val get_new_constrs : t -> enc -> constructor list mm
  val update_to_visit : t -> enc -> unit
  val update_transitions : t -> enc -> enc * tree -> action -> unit
  val get_action : t -> enc -> tree -> action mm
  val get_new_states : t -> enc -> states mm
  val stop : t -> bool
  val build : ?stop:(t -> bool) -> t -> t mm
end

module Make
    (Log : Logger.S)
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
