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
  val edges : labels -> states -> edgemap -> edgemap
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
   and type edgemap = EdgeMap.t' = struct
  type state = State.t
  type states = States.t
  type label = Label.t
  type labels = Labels.t
  type annotation = Annotation.t
  type trees = Base.Trees.t
  type action = Action.t
  type actionpairs = ActionPairs.t
  type actionmap = ActionMap.t'
  type edgemap = EdgeMap.t'

  (** [module WIP] is a lightweight counterpart of [Note.t] that forms some "work-in-progress" [Annotation.t]. Once we stop saturating an action, we check if we are able to yield a new saturated action and convert the [wip list] to an [Annotation.t].
  *)
  module WIP =
    Wip_annotation.Make (Log) (Base) (State) (Label) (Note) (Annotation)
      (Action)

  (** [module Trace] ... we keep track of the total sum of traces we have already checked. This is useful for checking if, from a state and action, we have already explored the rest of this trace and so can just use what we have already learned, e.g., if we are in some "subtrace".
  *)
  module Trace =
    Wip_trace.Make (Log) (Base) (State) (Label) (Note) (Annotation) (WIP)

  module Traces = Wip_traces.Make (Log) (Base) (State) (WIP) (Trace)

  (** [data] ...
      @param named is ...
      @param notes is ...
      @param visited
        is the set of states encountered so far in this particular saturation.
      @param traces
        is the set traces of all saturated actions so-far, which enables us to more optimally explore the state-space with minimal repitition.
      @param old_edges is ... *)
  type data =
    { named : Label.t option
    ; current : Trace.t option
    ; visited : States.t
    ; traces : Traces.t ref
    ; can_collect_traces : bool ref
    ; old_edges : EdgeMap.t'
    }

  let initial_data (traces : Traces.t ref) (old_edges : EdgeMap.t') : data =
    { named = None
    ; (* notes = []; *) current = None
    ; visited = States.empty
    ; traces
    ; can_collect_traces = ref true
    ; old_edges
    }
  ;;

  let has_named (d : data) : bool = Option.has_some d.named

  let update_traces (d : data) (x : Trace.t) : unit =
    Log.trace __FUNCTION__;
    d.traces := Traces.add x !(d.traces);
    d.can_collect_traces := true
  ;;

  (****************************************************************************)

  (** returns a copy of [d] with the updated name *)
  let update_named (x : Action.t) (d : data) : data =
    Log.trace __FUNCTION__;
    let named : Label.t option =
      match d.named with
      | None -> if Action.is_silent x then None else Some x.label
      | Some y -> Some y
    in
    { d with named }
  ;;

  (** returns a copy of [d] with the updated notes *)
  (* let update_notes (x : WIP.t) (d : data) : data =
      Log.trace __FUNCTION__;
      { d with notes = x :: d.notes }
    ;; *)

  (** returns a copy of [d] with [x] added to [d.current] *)
  let update_current (x : WIP.t) (d : data) : data =
    Log.trace __FUNCTION__;
    match d.current with
    | None -> { d with current = Some (Trace.create x) }
    | Some current -> { d with current = Some (Trace.add x current) }
  ;;

  (** returns a copy of [d] with the updated visited *)
  let update_visited (x : State.t) (d : data) : data =
    Log.trace __FUNCTION__;
    let f (x : State.t) (d : data) : States.t = States.add x d.visited in
    { d with visited = f x d }
  ;;

  (****************************************************************************)

  let already_visited (x : State.t) (d : data) : bool = States.mem x d.visited

  (** [skip_action x d] is [true] if [x] is non-silent and [d.named] is already [Some _].
  *)
  let skip_action (x : Action.t) (d : data) : bool =
    if Action.is_silent x then false else Option.has_some d.named
  ;;

  let get_old_actions (from : State.t) (d : data) : ActionMap.t' option =
    Log.trace __FUNCTION__;
    EdgeMap.find_opt d.old_edges from
  ;;

  (****************************************************************************)

  (* exception Model_Saturate_WIP_HadNoNamedActions of WIP.t list
    exception Model_Saturate_WIP_HadMultipleNamedActions of WIP.t list

    let validate_wips (xs : WIP.t list) : unit =
      Log.trace __FUNCTION__;
      match
        List.filter
          (fun ({ via; _ } : WIP.t) -> Label.is_silent via |> Bool.not)
          xs
      with
      | [] -> raise (Model_Saturate_WIP_HadNoNamedActions xs)
      | _ :: [] -> ()
      | _ :: _ -> raise (Model_Saturate_WIP_HadMultipleNamedActions xs)
    ;; *)

  (****************************************************************************)

  let update_acc (trace : Trace.t) (label : Label.t) (acc : ActionPairs.t) =
    Log.trace __FUNCTION__;
    Trace.to_annotation trace
    |> Annotations.extrapolate
    |> Annotations.to_list
    |> List.map (fun (x : Annotation.t) : ActionPair.t ->
      let y : Action.t =
        { label; annotation = Some x; trees = Base.Trees.empty }
      in
      y, States.singleton (Annotation.last x).goto)
    |> ActionPairs.merge_list acc
  ;;

  (** [stop] *)
  let stop (d : data) (goto : State.t) (acc : ActionPairs.t) : ActionPairs.t =
    Log.trace __FUNCTION__;
    match d.current, d.named with
    | Some current, Some named ->
      let () = Trace.validate current in
      let trace : Trace.t = Trace.set_goto goto current in
      update_traces d trace;
      update_acc trace named acc
    (* NOTE: skip and return [acc] otherwise *)
    | _, _ -> acc
  ;;

  (****************************************************************************)

  let finish_with_trace
        (z : Trace.t)
        (d : data)
        (named : Label.t)
        (acc : ActionPairs.t)
    : ActionPairs.t
    =
    Log.trace __FUNCTION__;
    let z : Trace.t = Trace.seq_opt d.current z in
    update_traces d z;
    update_acc z named acc
  ;;

  let finish_with_trace_upto
        (z : Trace.t)
        (d : data)
        (named : Label.t)
        (acc : ActionPairs.t)
    : ActionPairs.t
    =
    Log.trace __FUNCTION__;
    try
      let z : Trace.t = Trace.upto_named z in
      finish_with_trace z d named acc
    with
    (* NOTE: stop here as [x] begins with named action. *)
    | Not_found -> acc
  ;;

  (** [check_from] explores the outgoing actions of state [from], which is some destination of another action.
  *)
  let rec check_from (d : data) (from : State.t) (acc : ActionPairs.t)
    : ActionPairs.t
    =
    Log.trace __FUNCTION__;
    if already_visited from d
    then stop d from acc
    else (
      let d : data = update_visited from d in
      match get_old_actions from d with
      | None -> stop d from acc
      | Some old_actions -> check_actions d from old_actions acc)

  and check_actions (d : data) (from : State.t) (xs : ActionMap.t')
    : ActionPairs.t -> ActionPairs.t
    =
    Log.trace __FUNCTION__;
    ActionMap.fold
      (fun (x : Action.t) (ys : States.t) (acc : ActionPairs.t) ->
        if skip_action x d
        then stop d from acc
        else (
          try
            if !(d.can_collect_traces)
            then collect_from_traces d from x ys acc
            else raise Not_found
          with
          | Not_found ->
            (* NOTE: continue exploring un-traced state-space *)
            continue_check_destinations d from x ys acc))
      xs

  and collect_from_traces
        (d : data)
        (from : State.t)
        (x : Action.t)
        (ys : States.t)
        (acc : ActionPairs.t)
    : ActionPairs.t
    =
    Log.trace __FUNCTION__;
    let wip : WIP.t = WIP.create from x in
    let traces : Traces.t = Traces.get wip !(d.traces) in
    (* NOTE: add all traces that already have named action (if we don't) -- keep exploring with traces *)
    Traces.fold
      (fun (z : Trace.t) (acc : ActionPairs.t) : ActionPairs.t ->
        (* Log.thing ~__FUNCTION__ Debug "z" z ( Trace.to_string); *)
        match d.named, Trace.get_named_opt z with
        | Some named, None ->
          Log.trace ~__FUNCTION__ "stop (data)";
          (* NOTE: stop as named is in some [current]. *)
          finish_with_trace z d named acc
        | None, Some named ->
          Log.trace ~__FUNCTION__ "stop (trace)";
          (* NOTE: stop since the trace is named (and already explored). *)
          finish_with_trace z d named acc
        | None, None ->
          Log.trace ~__FUNCTION__ "continue (full)";
          (* NOTE: continue exploring un-traced state-space as the [named] must occur earlier in the trace and has been pruned *)
          (* NOTE: we can only use the traces once *)
          d.can_collect_traces := false;
          continue_check_destinations d from x ys acc
        | Some named, Some _ ->
          Log.trace ~__FUNCTION__ "continue (upto)";
          (* NOTE: we can only continue with the trace up-to the named action *)
          finish_with_trace_upto z d named acc)
      traces
      acc

  and continue_check_destinations
        (d : data)
        (from : State.t)
        (x : Action.t)
        (ys : States.t)
    : ActionPairs.t -> ActionPairs.t
    =
    Log.trace __FUNCTION__;
    let wip : WIP.t = WIP.create from x in
    let d : data (* NOTE: copy [d] *) = update_current wip d in
    let d : data = update_named x d in
    check_destinations d from ys

  and check_destinations (d : data) (from : State.t) (xs : States.t)
    : ActionPairs.t -> ActionPairs.t
    =
    Log.trace __FUNCTION__;
    States.fold (check_from d) xs
  ;;

  (****************************************************************************)

  (** [edge_action_destinations] returns a list of saturated actions tupled with their respective destinations, which is the reflexive-transitive closure of visible actions that may weakly be performed from each of [the_destinations].
      edge -> edge_actions -> edge_action_destinations -> ( ... )
      @param ys
        is the set of destination [States.t] reachable from state [from] via actions that have already been recorded in [d.notes] as a [wip].
  *)
  let edge_action_destinations (d : data) (from : State.t) (ys : States.t)
    : ActionPairs.t
    =
    Log.trace __FUNCTION__;
    States.fold
      (fun (y : State.t) (acc : ActionPairs.t) ->
        (* Log.thing ~__FUNCTION__ Debug "y" y ( State.to_string); *)
        check_from d y ActionPairs.empty)
      ys
      ActionPairs.empty
  ;;

  (** [edge_actions] returns a list of saturated actions tupled with their respective destinations, obtained from [edge_action_destinations] which explores the reflexive-transitive closure
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge_actions
        (from : State.t)
        (old_actions : ActionMap.t')
        (old_edges : EdgeMap.t')
        (traces : Traces.t ref)
    : ActionPairs.t
    =
    Log.trace __FUNCTION__;
    ActionMap.fold
      (fun (x : Action.t) (ys : States.t) (acc : ActionPair.t list) ->
        (* Log.thing ~__FUNCTION__ Debug "x" x ( Action.to_string); *)
        let d : data =
          initial_data traces old_edges
          |> update_named x
          |> update_current (WIP.create from x)
        in
        edge_action_destinations d from ys
        |> ActionPairs.to_list
        |> ActionPair.merge_lists acc)
      old_actions
      []
    |> ActionPairs.of_list
  ;;

  (** [edge] updates [new_actions] with actions saturated by [edge_actions]
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge
        (new_actions : ActionMap.t')
        (from : State.t)
        (old_actions : ActionMap.t')
        (old_edges : EdgeMap.t')
        (traces : Traces.t ref)
    : unit
    =
    Log.trace __FUNCTION__;
    edge_actions from old_actions old_edges traces
    |> ActionPairs.iter
         (fun ((saturated_action, destinations) : Action.t * States.t) ->
         ActionMap.update new_actions saturated_action destinations)
  ;;

  (** [] *)
  let edges (labels : Labels.t) (states : States.t) (old_edges : EdgeMap.t')
    : EdgeMap.t'
    =
    Log.trace __FUNCTION__;
    let new_edges : EdgeMap.t' = EdgeMap.create 0 in
    let traces : Traces.t ref = ref Traces.empty in
    EdgeMap.iter
      (fun (from : State.t) (old_actions : ActionMap.t') ->
        (* Log.thing ~__FUNCTION__ Debug "from" from ( State.to_string); *)
        (* NOTE: populate [new_actions] with saturated [old_actions] *)
        let new_actions : ActionMap.t' = ActionMap.create 0 in
        let () = edge new_actions from old_actions old_edges traces in
        EdgeMap.replace new_edges from new_actions)
      old_edges;
    (* Log.thing ~__FUNCTION__ Debug "traces" !traces ( Traces.to_string); *)
    new_edges
  ;;
end
