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
   and type edgemap = C.EdgeMap.t' = struct
  module State = C.State
  module States = C.State.Set
  module Label = C.Label
  module Labels = C.Label.Set
  module Annotation = C.Annotation
  module Annotations = C.Annotation.Set
  module Action = C.Action
  module ActionPair = C.Action.Pair
  module ActionPairs = C.Action.Pair.Set
  module ActionMap = C.Action.Map
  module EdgeMap = C.EdgeMap
  module Note = C.Note

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
  module WIP = Wip_annotation.Make (Base) (C)

  (** [module Trace] ... we keep track of the total sum of traces we have already checked. This is useful for checking if, from a state and action, we have already explored the rest of this trace and so can just use what we have already learned, e.g., if we are in some "subtrace".
  *)
  module Trace = Wip_trace.Make (C) (WIP)

  module Traces = Wip_traces.Make (C) (WIP) (Trace)

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
    ; current = None
    ; visited = States.empty
    ; traces
    ; can_collect_traces = ref true
    ; old_edges
    }
  ;;

  let has_named (d : data) : bool = Stdlib.Option.is_some d.named

  let update_traces (d : data) (x : Trace.t) : unit =
    Logger.trace __FUNCTION__;
    d.traces := Traces.add x !(d.traces);
    d.can_collect_traces := true
  ;;

  (****************************************************************************)

  (** returns a copy of [d] with the updated name *)
  let update_named (x : Action.t) (d : data) : data =
    Logger.trace __FUNCTION__;
    let named : Label.t option =
      match d.named with
      | None -> if Action.is_silent x then None else Some x.label
      | Some y -> Some y
    in
    { d with named }
  ;;

  (** returns a copy of [d] with [x] added to [d.current] *)
  let update_current (x : WIP.t) (d : data) : data =
    Logger.trace __FUNCTION__;
    match d.current with
    | None -> { d with current = Some (Trace.create x) }
    | Some current -> { d with current = Some (Trace.add x current) }
  ;;

  (** returns a copy of [d] with the updated visited *)
  let update_visited (x : State.t) (d : data) : data =
    Logger.trace __FUNCTION__;
    let f (x : State.t) (d : data) : States.t = States.add x d.visited in
    { d with visited = f x d }
  ;;

  (****************************************************************************)

  let already_visited (x : State.t) (d : data) : bool = States.mem x d.visited

  (** [skip_action x d] is [true] if [x] is non-silent and [d.named] is already [Some _].
  *)
  let skip_action (x : Action.t) (d : data) : bool =
    if Action.is_silent x then false else Stdlib.Option.is_some d.named
  ;;

  let get_old_actions (from : State.t) (d : data) : ActionMap.t' option =
    Logger.trace __FUNCTION__;
    EdgeMap.find_opt d.old_edges from
  ;;

  (****************************************************************************)

  let update_acc (trace : Trace.t) (label : Label.t) (acc : ActionPairs.t) =
    Logger.trace __FUNCTION__;
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
    Logger.trace __FUNCTION__;
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
    Logger.trace __FUNCTION__;
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
    Logger.trace __FUNCTION__;
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
    Logger.trace __FUNCTION__;
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
    Logger.trace __FUNCTION__;
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
    Logger.trace __FUNCTION__;
    let wip : WIP.t = WIP.create from x in
    let traces : Traces.t = Traces.get wip !(d.traces) in
    (* NOTE: add all traces that already have named action (if we don't) -- keep exploring with traces *)
    Traces.fold
      (fun (z : Trace.t) (acc : ActionPairs.t) : ActionPairs.t ->
        match d.named, Trace.get_named_opt z with
        | Some named, None ->
          Logger.trace ~__FUNCTION__ "stop (data)";
          (* NOTE: stop as named is in some [current]. *)
          finish_with_trace z d named acc
        | None, Some named ->
          Logger.trace ~__FUNCTION__ "stop (trace)";
          (* NOTE: stop since the trace is named (and already explored). *)
          finish_with_trace z d named acc
        | None, None ->
          Logger.trace ~__FUNCTION__ "continue (full)";
          (* NOTE: continue exploring un-traced state-space as the [named] must occur earlier in the trace and has been pruned *)
          (* NOTE: we can only use the traces once *)
          continue_check_destinations
            { d with can_collect_traces = ref false }
            from
            x
            ys
            acc
        | Some named, Some _ ->
          Logger.trace ~__FUNCTION__ "continue (upto)";
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
    Logger.trace __FUNCTION__;
    let wip : WIP.t = WIP.create from x in
    let d : data (* NOTE: copy [d] *) = update_current wip d in
    let d : data = update_named x d in
    check_destinations d from ys

  and check_destinations (d : data) (from : State.t) (xs : States.t)
    : ActionPairs.t -> ActionPairs.t
    =
    Logger.trace __FUNCTION__;
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
    Logger.trace __FUNCTION__;
    States.fold (check_from d) ys ActionPairs.empty
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
    Logger.trace __FUNCTION__;
    ActionMap.fold
      (fun (x : Action.t) (ys : States.t) (acc : ActionPair.t list) ->
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
    Logger.trace __FUNCTION__;
    edge_actions from old_actions old_edges traces
    |> ActionPairs.iter
         (fun ((saturated_action, destinations) : Action.t * States.t) ->
         ActionMap.update new_actions saturated_action destinations)
  ;;

  (****************************************************************************)
  (* Closure-based saturation. Replaces the depth-first path enumeration
     above, which cost time exponential in the path count rather than the
     state count -- 101 states took 437 seconds on a grid (see
     [test/satscale.ml]). The specification it computes is unchanged, and is
     derived in [notes/5-saturation-rewrite.md]:

       for each [from], each visible label [a], and each [goto] such that
       [from -tau*-> s -a-> t -tau*-> goto], emit one weak action labelled
       [a] with destination [{goto}], annotated with the SHORTEST witness.

     That "shortest" is not a new choice: [ActionPair.try_update] merges
     [wk_equal] actions with equal destination sets by keeping
     [Annotation.shorter], so of the exponentially many paths the old
     traversal explored, only the shortest per destination ever survived. A
     breadth-first closure produces exactly those survivors directly. *)

  (** Silent steps leaving [s], each silent action paired with one of its
      destinations. *)
  let silent_steps (old_edges : EdgeMap.t') (s : State.t)
    : (Action.t * State.t) list
    =
    match EdgeMap.find_opt old_edges s with
    | None -> []
    | Some actions ->
      ActionMap.fold
        (fun (a : Action.t) (ds : States.t) (acc : (Action.t * State.t) list) ->
          if Action.is_silent a
          then States.fold (fun (d : State.t) acc -> (a, d) :: acc) ds acc
          else acc)
        actions
        []
  ;;

  let note_of (from : State.t) (a : Action.t) (goto : State.t) : Note.t =
    { from; label = a.label; using = a.trees; goto }
  ;;

  (** Reflexive-transitive silent closure of [src], breadth-first, pairing
      each reachable state with a shortest silent path to it. Paths are
      accumulated most-recent-first and reversed at the point of use. *)
  let silent_closure (old_edges : EdgeMap.t') (src : State.t)
    : (State.t * Note.t list) list
    =
    let rec bfs
              (frontier : (State.t * Note.t list) list)
              (seen : States.t)
              (acc : (State.t * Note.t list) list)
      : (State.t * Note.t list) list
      =
      match frontier with
      | [] -> acc
      | _ ->
        let next, seen =
          List.fold_left
            (fun ((next, seen) : (State.t * Note.t list) list * States.t)
              ((s, path) : State.t * Note.t list) ->
              List.fold_left
                (fun ((next, seen) : (State.t * Note.t list) list * States.t)
                  ((a, d) : Action.t * State.t) ->
                  if States.mem d seen
                  then next, seen
                  else (d, note_of s a d :: path) :: next, States.add d seen)
                (next, seen)
                (silent_steps old_edges s))
            ([], seen)
            frontier
        in
        bfs next seen (List.rev_append next acc)
    in
    let start : (State.t * Note.t list) list = [ src, [] ] in
    bfs start (States.singleton src) start
  ;;

  let rec annotation_of_notes : Note.t list -> Annotation.t option = function
    | [] -> None
    | x :: tl -> Some { this = x; next = annotation_of_notes tl }
  ;;

  (** The closure-based counterpart of [edge]. *)
  let edge_closure
        (new_actions : ActionMap.t')
        (from : State.t)
        (old_edges : EdgeMap.t')
    : unit
    =
    Logger.trace __FUNCTION__;
    let pairs : ActionPair.t list ref = ref [] in
    List.iter
      (fun ((s, pre_rev) : State.t * Note.t list) ->
        match EdgeMap.find_opt old_edges s with
        | None -> ()
        | Some actions ->
          ActionMap.fold
            (fun (a : Action.t) (ds : States.t) () ->
              if Action.is_silent a
              then ()
              else
                States.iter
                  (fun (t : State.t) ->
                    let mid : Note.t = note_of s a t in
                    List.iter
                      (fun ((goto, post_rev) : State.t * Note.t list) ->
                        let notes : Note.t list =
                          List.rev pre_rev @ (mid :: List.rev post_rev)
                        in
                        match annotation_of_notes notes with
                        | None -> ()
                        | Some ann ->
                          let act : Action.t =
                            { label = a.label
                            ; annotation = Some ann
                            ; trees = Base.Trees.empty
                            }
                          in
                          pairs := (act, States.singleton goto) :: !pairs)
                      (silent_closure old_edges t))
                  ds)
            actions
            ())
      (silent_closure old_edges from);
    ActionPair.merge_lists [] !pairs
    |> ActionPairs.of_list
    |> ActionPairs.iter
         (fun ((saturated_action, destinations) : Action.t * States.t) ->
         ActionMap.update new_actions saturated_action destinations)
  ;;

  (****************************************************************************)

  (** [] returns a saturated [EdgeMap.t'] paired with a set of terminals states {i (i.e., states that now have no outgoing actions, and if reached)}.*)
  let edges (labels : Labels.t) (states : States.t) (old_edges : EdgeMap.t')
    : EdgeMap.t' * States.t
    =
    Logger.trace __FUNCTION__;
    let new_edges : EdgeMap.t' = EdgeMap.create 0 in
    (* The trace memo existed only to curb re-exploration in the
       depth-first enumeration; the closure does not re-explore. Kept bound
       so the old implementation above still type-checks until it is
       deleted in a follow-up commit. *)
    let _traces : Traces.t ref = ref Traces.empty in
    let terminals : States.t =
      EdgeMap.fold
        (fun (from : State.t) (old_actions : ActionMap.t') (acc : States.t) ->
          (* NOTE: populate [new_actions] with saturated [old_actions] *)
          let new_actions : ActionMap.t' = ActionMap.create 0 in
          let () = ignore old_actions in
          let () = edge_closure new_actions from old_edges in
          if ActionMap.length new_actions > 0
          then (
            EdgeMap.replace new_edges from new_actions;
            acc)
          else States.add from acc)
        old_edges
        States.empty
    in
    new_edges, terminals
  ;;
end
