module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Annotation : sig
       type t
     end)
    (Transition :
       Transition.S
       with type t = (State.t, Label.t, Base.Tree.t, Annotation.t) Transition.t')
    (Transitions : sig
       include Set.S with type elt = Transition.t
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }
     end)
    (Actions : sig
       include Set.S with type elt = Action.t
     end)
    (ActionPair : sig
       type t = Action.t * States.t
     end)
    (ActionPairs : sig
       include Set.S with type elt = ActionPair.t
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       val json : ?as_elt:bool -> t' -> Yojson.t
       val update : t' -> Action.t -> States.t -> unit
       val destinations : t' -> States.t
       val reduce_by_label : t' -> Label.t -> t'
       val to_actionpairs : t' -> ActionPairs.t
       val of_actionpairs : ActionPairs.t -> t'
       val merge : t' -> t' -> t'
     end)
    (Edge : sig
       type t =
         { from : State.t
         ; goto : State.t
         ; action : Action.t
         }
     end)
    (Edges : sig
       include Set.S with type elt = Edge.t
     end) : sig
  include Hashtbl.S with type key = State.t

  type t' = ActionMap.t' t

  val json : ?as_elt:bool -> t' -> Yojson.t
  val to_string : ?pretty:bool -> t' -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
  val update : t' -> State.t -> Action.t -> States.t -> unit
  val destinations : t' -> State.t -> States.t
  val get_actions : t' -> State.t -> Actions.t
  val reduce_by_label : t' -> Label.t -> t'
  val get_edges : t' -> State.t -> Edges.t
  val to_edges : t' -> Edges.t
  val of_edges : Edges.t -> t'
  val of_transitions : Transitions.t -> t'
  val merge : t' -> t' -> t'
end = struct
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
        let kname = "From"
        let vname = "Actions"
        let kjson = State.json
        let vjson = ActionMap.json
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
      State.log ~__FUNCTION__ ~s:"has no edges" from;
      States.empty
    | Some ys -> ActionMap.destinations ys
  ;;

  let get_actions (x : t') (from : State.t) : Actions.t =
    Log.trace __FUNCTION__;
    find x from |> ActionMap.to_seq_keys |> Actions.of_seq
  ;;

  let reduce_by_label (x : t') (label : Label.t) : t' =
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
                 ((action, destinations) : ActionPair.t)
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
          ; constructor_trees =
              Option.cata Base.Trees.singleton Base.Trees.empty tree
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
