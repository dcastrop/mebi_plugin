module type S = sig
  type label
  type action
  type actions
  type states
  type actionpairs

  include Hashtbl.S with type key = action

  type t' = states t

  val json : ?as_elt:bool -> t' -> Yojson.t
  val to_string : ?pretty:bool -> t' -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t' -> unit
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
   and type actionpairs = ActionPairs.t = struct
  type label = Label.t
  type action = Action.t
  type actions = Actions.t
  type states = States.t
  type actionpairs = ActionPairs.t

  module Map_ : Hashtbl.S with type key = Action.t = Hashtbl.Make (Action)
  include Map_

  type t' = States.t t

  include
    Json.Map.Make
      (Log)
      (struct
        module Map = Map_

        type value = States.t

        let name = "ActionMap"
        let kname = "Action"
        let vname = "Destinations"
        let kjson = Action.json
        let vjson = States.json
      end)

  (** [update] ... if the action is already present, then along with merging the destination states, we also merge the constructor trees.
  *)
  let update (x : t') (action : Action.t) (states : States.t) : unit =
    Log.trace __FUNCTION__;
    if States.is_empty states
    then ()
    else (
      match find_opt x action with
      | None -> add x action states
      | Some old_states ->
        (* NOTE: also find the existing key to merge the [trees] *)
        let action : Action.t =
          to_seq_keys x
          |> Seq.filter (Action.equal action)
          |> Seq.fold_left
               (fun (action : Action.t) (y : Action.t) ->
                 { action with trees = Base.Trees.union action.trees y.trees })
               action
        in
        replace x action (States.union old_states states))
  ;;

  (** [destinations x f e] merges the values of [x] using [f], where [e] is some initial (i.e., "empty") collection of ['a].
  *)
  let destinations (x : t') : States.t =
    Log.trace __FUNCTION__;
    to_seq_values x |> List.of_seq |> List.fold_left States.union States.empty
  ;;

  let reduce_by_label (x : t') (label : Label.t) : t' =
    Log.trace __FUNCTION__;
    let y : t' = copy x in
    filter_map_inplace
      (fun (k : Action.t) (vs : States.t) ->
        if Label.equal k.label label then Some vs else None)
      y;
    y
  ;;

  let to_actions (x : t') : Actions.t = to_seq_keys x |> Actions.of_seq

  let to_actionpairs (x : t') : ActionPairs.t =
    Log.trace __FUNCTION__;
    fold
      (fun (k : Action.t) (vs : States.t) : (ActionPairs.t -> ActionPairs.t) ->
        ActionPairs.add (k, vs))
      x
      ActionPairs.empty
  ;;

  let of_actionpairs (xs : ActionPairs.t) : t' =
    Log.trace __FUNCTION__;
    let y : t' = create 0 in
    ActionPairs.iter (fun ((k, vs) : ActionPairs.elt) -> update y k vs) xs;
    y
  ;;

  let merge (a : t') (b : t') : t' =
    Log.trace __FUNCTION__;
    ActionPairs.union (to_actionpairs a) (to_actionpairs b) |> of_actionpairs
  ;;
end
