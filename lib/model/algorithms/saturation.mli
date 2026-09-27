(** {i See {!Model.S.Saturation}.} *)
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

  (** {1 Saturation by Work-In-Progress Annotations} *)

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

  (** {1 Saturation Algorithm Data} *)

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

  (** {2 Stopping} *)

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

  (** {2 Exploration} *)

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

  (** {2 Main Loop} *)

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
    (Base : Base_term.S)
    (C : Components.S with type trees = Base.Trees.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type label = C.Label.t
   and type labels = C.Label.Set.t
   and type annotation = C.Annotation.t
   and type trees = C.trees
   and type action = C.Action.t
   and type actionpairs = C.Action.Pair.Set.t
   and type actionmap = C.Action.Map.t'
   and type edgemap = C.EdgeMap.t'
