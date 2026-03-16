module type S = sig
  type state
  type states
  type label
  type transitions
  type action
  type actions
  type actionmap
  type edges

  include Hashtbl.S with type key = state

  type t' = actionmap t

  include Json.S with type k = t'

  val update : t' -> state -> action -> states -> unit
  val destinations : t' -> state -> states
  val get_actions : t' -> state -> actions
  val reduce_by_label : t' -> label -> t'
  val get_edges : t' -> state -> edges
  val to_edges : t' -> edges
  val of_edges : edges -> t'
  val of_transitions : transitions -> t'
  val merge : t' -> t' -> t'
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (States : States.S with type elt = State.t)
    (Transition :
       Transition.S with type state = State.t and type tree = Base.Tree.t)
    (Transitions : Transitions.S with type elt = Transition.t)
    (Action :
       Action.S
       with type label = Transition.label
        and type annotation = Transition.annotation
        and type trees = Base.Trees.t)
    (Actions : Actions.S with type elt = Action.t)
    (ActionPairs : Actionpairs.S with type elt = Action.t * States.t)
    (ActionMap :
       Actionmap.S
       with type label = Action.label
        and type action = Action.t
        and type states = States.t
        and type actionpairs = ActionPairs.t)
    (Edge : Edge.S with type state = State.t and type action = Action.t)
    (Edges : Edges.S with type elt = Edge.t) :
  S
  with type state = State.t
   and type states = States.t
   and type label = Transition.label
   and type label = Action.label
   and type label = ActionMap.label
   and type transitions = Transitions.t
   and type action = Action.t
   and type actions = Actions.t
   and type actionmap = ActionMap.t'
   and type edges = Edges.t
