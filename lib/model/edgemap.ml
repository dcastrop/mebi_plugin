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
   and type edges = Edges.t = struct
  type state = State.t
  type states = States.t
  type label = Action.label
  type transitions = Transitions.t
  type action = Action.t
  type actions = Actions.t
  type actionmap = ActionMap.t'
  type edges = Edges.t

  module Map_ : Hashtbl.S with type key = State.t = Hashtbl.Make (State)
  include Map_

  type t' = ActionMap.t' t

  include
    Json.Map.Make
      (Log)
      (struct
        module Map = Map_

        type value = ActionMap.t'

        let name = "EdgeMap"
      end)
      (struct
        include State

        let name = "From"
      end)
      (struct
        include ActionMap

        let name = "Actions"
        let compare a b : int = 0
      end)

  let update
        (x : t')
        (from : State.t)
        (action : Action.t)
        (destinations : States.t)
    : unit
    =
    Log.trace __FUNCTION__;
    match find_opt x from with
    | None ->
      ActionPairs.singleton (action, destinations)
      |> ActionMap.of_actionpairs
      |> add x from
    | Some actions -> ActionMap.update actions action destinations
  ;;

  let destinations (x : t') (from : State.t) : States.t =
    Log.trace __FUNCTION__;
    match find_opt x from with
    | None ->
      (* State.log ~__FUNCTION__ ~m:Trace ~s:"no edges from" from; *)
      States.empty
    | Some ys -> ActionMap.destinations ys
  ;;

  let get_actions (x : t') (from : State.t) : Actions.t =
    Log.trace __FUNCTION__;
    find x from |> ActionMap.to_seq_keys |> Actions.of_seq
  ;;

  let reduce_by_label (x : t') (label : label) : t' =
    Log.trace __FUNCTION__;
    let y : t' = copy x in
    filter_map_inplace
      (fun (k : State.t) (vs : ActionMap.t') ->
        let vs' : ActionMap.t' = ActionMap.reduce_by_label vs label in
        if ActionMap.length vs' > 0 then Some vs' else None)
      y;
    y
  ;;

  let get_edges (x : t') (from : State.t) : Edges.t =
    Log.trace __FUNCTION__;
    ActionMap.fold
      (fun (action : Action.t) (v : States.t) (acc : Edges.t) : Edges.t ->
        States.fold
          (fun (goto : State.t) (acc : Edges.t) : Edges.t ->
            Edges.add { from; action; goto } acc)
          v
          acc)
      (find x from)
      Edges.empty
  ;;

  let to_edges (x : t') : Edges.t =
    Log.trace __FUNCTION__;
    fold
      (fun (from : State.t) (vs : ActionMap.t') : (Edges.t -> Edges.t) ->
        ActionMap.to_actionpairs vs
        |> ActionPairs.fold
             (fun
                 ((action, destinations) : ActionPairs.elt)
                  : (Edges.t -> Edges.t)
                ->
             States.fold
               (fun (goto : State.t) : (Edges.t -> Edges.t) ->
                 Edges.add { from; goto; action })
               destinations))
      x
      Edges.empty
  ;;

  let of_edges (xs : Edges.t) : t' =
    Log.trace __FUNCTION__;
    let ys : t' = create 0 in
    Edges.iter
      (fun ({ from; goto; action } : Edge.t) ->
        update ys from action (States.singleton goto))
      xs;
    ys
  ;;

  let of_transitions (xs : Transitions.t) : t' =
    Log.trace __FUNCTION__;
    let edges : t' = create 0 in
    Transitions.iter
      (fun ({ from; goto; label; annotation; tree } : Transition.t) ->
        (* NOTE: [ActionMap.update] handles merging of [constructor_trees] for [actions] with matching [labels] *)
        update
          edges
          from
          { label
          ; annotation
          ; trees = Option.cata Base.Trees.singleton Base.Trees.empty tree
          }
          (States.singleton goto))
      xs;
    edges
  ;;

  let merge (a : t') (b : t') : t' =
    Log.trace __FUNCTION__;
    let c : t' = copy a in
    iter
      (fun (k : State.t) (vs : ActionMap.t') ->
        match find_opt c k with
        | Some actions -> ActionMap.merge actions vs |> replace c k
        | None -> add c k vs)
      b;
    c
  ;;
end
