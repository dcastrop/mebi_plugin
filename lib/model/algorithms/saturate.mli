module type S = sig
  type state
  type states
  type label
  type labels
  type annotation
  type trees
  type action
  type actionpairs
  type actionmap
  type edgemap

  module WIP :
    Wip_annotation.S
    with type state = state
     and type label = label
     and type annotation = annotation
     and type trees = trees
     and type action = action

  module Trace :
    Wip_trace.S
    with type state = state
     and type label = label
     and type annotation = annotation
     and type wip = WIP.t

  module Traces : Wip_traces.S with type elt = Trace.t and type wip = WIP.t

  type data =
    { named : label option
    ; current : Trace.t option
    ; visited : states
    ; traces : Traces.t ref
    ; can_collect_traces : bool ref
    ; old_edges : edgemap
    }

  val initial_data : Traces.t ref -> edgemap -> data
  val has_named : data -> bool
  val update_traces : data -> Trace.t -> unit
  val update_named : action -> data -> data
  val update_current : WIP.t -> data -> data
  val update_visited : state -> data -> data
  val already_visited : state -> data -> bool
  val skip_action : action -> data -> bool
  val get_old_actions : state -> data -> actionmap option
  val update_acc : Trace.t -> label -> actionpairs -> actionpairs
  val stop : data -> state -> actionpairs -> actionpairs
  val finish_with_trace : Trace.t -> data -> label -> actionpairs -> actionpairs

  val finish_with_trace_upto
    :  Trace.t
    -> data
    -> label
    -> actionpairs
    -> actionpairs

  val check_from : data -> state -> actionpairs -> actionpairs
  val check_actions : data -> state -> actionmap -> actionpairs -> actionpairs

  val collect_from_traces
    :  data
    -> state
    -> action
    -> states
    -> actionpairs
    -> actionpairs

  val continue_check_destinations
    :  data
    -> state
    -> action
    -> states
    -> actionpairs
    -> actionpairs

  val check_destinations : data -> state -> states -> actionpairs -> actionpairs
  val edge_action_destinations : data -> state -> states -> actionpairs

  val edge_actions
    :  state
    -> actionmap
    -> edgemap
    -> Traces.t ref
    -> actionpairs

  val edge : actionmap -> state -> actionmap -> edgemap -> Traces.t ref -> unit
  val edges : labels -> states -> edgemap -> edgemap * states
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type base = Base.t)
    (Labels : Labels.S with type elt = Label.t)
    (Note :
       Annotation_note.S
       with type state = State.t
        and type label = Label.t
        and type trees = Base.Trees.t)
    (Annotation : Annotation.S with type label = Label.t and type note = Note.t)
    (Annotations : Annotations.S with type elt = Annotation.t)
    (Action :
       Action.S
       with type label = Label.t
        and type annotation = Annotation.t
        and type trees = Base.Trees.t)
    (ActionPair :
       Actionpair.S with type action = Action.t and type states = States.t)
    (ActionPairs :
       Actionpairs.S with type states = States.t and type elt = ActionPair.t)
    (ActionMap :
       Actionmap.S
       with type label = Label.t
        and type action = Action.t
        and type states = States.t
        and type actionpairs = ActionPairs.t)
    (EdgeMap :
       Edgemap.S
       with type state = State.t
        and type states = States.t
        and type label = Label.t
        and type action = Action.t
        and type actionmap = ActionMap.t') :
  S
  with type state = State.t
   and type states = States.t
   and type label = Label.t
   and type labels = Labels.t
   and type annotation = Annotation.t
   and type trees = Base.Trees.t
   and type action = Action.t
   and type actionpairs = ActionPairs.t
   and type actionmap = ActionMap.t'
   and type edgemap = EdgeMap.t'
