module type S = sig
  type state
  type label
  type edgemap

  include Set.S
  include Json.S with type k = t

  val get_bisimilar : state -> t -> elt
  val filter_reachable : elt -> t -> t
  val reachable : state -> edgemap -> t -> t
  val reachable_by_label : state -> label -> edgemap -> t -> t
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (ActionMap : Actionmap.S with type states = States.t)
    (EdgeMap :
       Edgemap.S
       with type state = State.t
        and type states = States.t
        and type label = ActionMap.label
        and type action = ActionMap.action
        and type actions = ActionMap.actions
        and type actionmap = ActionMap.t') :
  S
  with type elt = States.t
   and type state = State.t
   and type label = ActionMap.label
   and type label = EdgeMap.label
   and type edgemap = EdgeMap.t'
