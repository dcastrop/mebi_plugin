module type S = sig
  type label
  type action
  type actions
  type states
  type actionpairs

  include Hashtbl.S with type key = action (** @closed *)

  type t' = states t

  include Json.S with type k = t' (** @closed *)

  val size : t' -> int
  val update : t' -> action -> states -> unit
  val destinations : t' -> states
  val reduce_by_label : t' -> label -> t'
  val to_actions : t' -> actions
  val to_actionpairs : t' -> actionpairs
  val of_actionpairs : actionpairs -> t'
  val merge : t' -> t' -> t'
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (States : States.S)
    (Label : Label.S with type base = Base.t)
    (Action : Action.S with type label = Label.t and type trees = Base.Trees.t)
    (Actions : Actions.S with type elt = Action.t)
    (ActionPairs : Actionpairs.S with type elt = Action.t * States.t) :
  S
  with type label = Label.t
   and type action = Action.t
   and type actions = Actions.t
   and type states = States.t
   and type actionpairs = ActionPairs.t
