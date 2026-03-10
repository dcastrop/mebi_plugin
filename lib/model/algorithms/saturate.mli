module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Labels : Labels.S with type elt = Label.t)
    (Note : sig
       type t =
         { from : State.t
         ; label : Label.t
         ; using : Base.Trees.t
         ; goto : State.t
         }
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val last : t -> Note.t
     end)
    (Annotations : sig
       include Set.S with type elt = Annotation.t

       val extrapolate : Annotation.t -> t
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }

       val is_silent : t -> bool
     end)
    (ActionPair : sig
       type t = Action.t * States.t

       val merge_lists : t list -> t list -> t list
     end)
    (ActionPairs : sig
       include Set.S with type elt = ActionPair.t

       val merge_list : t -> ActionPair.t list -> t
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       val update : t' -> Action.t -> States.t -> unit
     end)
    (EdgeMap : sig
       include Hashtbl.S with type key = State.t

       type t' = ActionMap.t' t
     end)
    -> sig
  module WIP :
      module type of
        Wip_annotation.Make (Log) (Base) (State) (Label) (Note) (Annotation)
          (Action)

  module Trace :
      module type of
        Wip_trace.Make (Log) (Base) (State) (Label) (Note) (Annotation) (WIP)

  module Traces :
      module type of Wip_traces.Make (Log) (Base) (State) (WIP) (Trace)

  type data =
    { named : Label.t option
    ; current : Trace.t option
    ; visited : States.t
    ; traces : Traces.t ref
    ; can_collect_traces : bool ref
    ; old_edges : EdgeMap.t'
    }

  val initial_data : Traces.t ref -> EdgeMap.t' -> data
  val has_named : data -> bool
  val update_traces : data -> Trace.t -> unit
  val update_named : Action.t -> data -> data
  val update_current : WIP.t -> data -> data
  val update_visited : State.t -> data -> data
  val already_visited : State.t -> data -> bool
  val skip_action : Action.t -> data -> bool
  val get_old_actions : State.t -> data -> ActionMap.t' option
  val update_acc : Trace.t -> Label.t -> ActionPairs.t -> ActionPairs.t
  val stop : data -> State.t -> ActionPairs.t -> ActionPairs.t

  val finish_with_trace
    :  Trace.t
    -> data
    -> Label.t
    -> ActionPairs.t
    -> ActionPairs.t

  val finish_with_trace_upto
    :  Trace.t
    -> data
    -> Label.t
    -> ActionPairs.t
    -> ActionPairs.t

  val check_from : data -> State.t -> ActionPairs.t -> ActionPairs.t

  val check_actions
    :  data
    -> State.t
    -> ActionMap.t'
    -> ActionPairs.t
    -> ActionPairs.t

  val collect_from_traces
    :  data
    -> State.t
    -> Action.t
    -> States.t
    -> ActionPairs.t
    -> ActionPairs.t

  val continue_check_destinations
    :  data
    -> State.t
    -> Action.t
    -> States.t
    -> ActionPairs.t
    -> ActionPairs.t

  val check_destinations
    :  data
    -> State.t
    -> States.t
    -> ActionPairs.t
    -> ActionPairs.t

  val edge_action_destinations : data -> State.t -> States.t -> ActionPairs.t

  val edge_actions
    :  State.t
    -> ActionMap.t'
    -> EdgeMap.t'
    -> Traces.t ref
    -> ActionPairs.t

  val edge
    :  ActionMap.t'
    -> State.t
    -> ActionMap.t'
    -> EdgeMap.t'
    -> Traces.t ref
    -> unit

  val edges : Labels.t -> States.t -> EdgeMap.t' -> EdgeMap.t'
end
