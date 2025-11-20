module Info = Model_info
module State = Model_state
module Label = Model_label
module Action = Model_action
module Transition = Model_transition
module Edge = Model_edge

module States = Set.Make (struct
    include State
  end)

let states_to_string (x : States.t) : string = "TODO: Model.states_to_string"

module Partition = Set.Make (States)

let partition_to_string (x : Partition.t) : string =
  "TODO: Model.partition_to_string"
;;

(** Alphabet *)
module Alphabet = Set.Make (struct
    include Label
  end)

let alphabet_to_string (x : Alphabet.t) : string =
  "TODO: Model.alphabet_to_string"
;;

let silent_label_opt (xs : Alphabet.t) : Label.t option =
  Alphabet.find_first_opt
    (fun x -> Option.cata (fun y -> y) false x.is_silent)
    xs
;;

exception Model_Alphabet_SilentLabelNotFound of Alphabet.t

let silent_label (xs : Alphabet.t) : Label.t =
  match silent_label_opt xs with
  | None -> raise (Model_Alphabet_SilentLabelNotFound xs)
  | Some x -> x
;;

let silent_action (xs : Alphabet.t) : Action.t =
  { label = silent_label xs; annotations_list = []; constructor_trees = [] }
;;

module Transitions = Set.Make (struct
    include Transition
  end)

let transition_to_string (x : Transitions.t) : string =
  "TODO: Model.transition_to_string"
;;

module Actions = Hashtbl.Make (struct
    include Action
  end)

let actions_to_string (x : States.t Actions.t) : string =
  "TODO: Model.actions_to_string"
;;

let update_destinations actions updated_action destinations : States.t =
  match Actions.find_opt actions updated_action with
  | None -> destinations
  | Some old_destinations -> States.union destinations old_destinations
;;

let update_action actions updated_action destinations : unit =
  if Bool.not (States.is_empty destinations)
  then
    Actions.replace
      actions
      updated_action
      (update_destinations actions updated_action destinations)
;;

module Edges = Hashtbl.Make (struct
    include State
  end)

let edges_to_string (x : States.t Actions.t Edges.t) : string =
  "TODO: Model.edges_to_string"
;;

module Convert = struct
  (** edges_to_transitions *)
  let rec edges_to_transitions (edges : States.t Actions.t Edges.t)
    : Transitions.t
    =
    Edges.fold
      (fun (from : State.t)
        (actions : States.t Actions.t)
        (acc : Transitions.t) -> actions_to_transitions from actions acc)
      edges
      Transitions.empty

  and actions_to_transitions (from : State.t) actions acc =
    Actions.fold
      (fun (action : Action.t)
        (destinations : States.t)
        (acc : Transitions.t) ->
        action_destinations_to_transitions from action destinations acc)
      actions
      acc

  and action_destinations_to_transitions
        (from : State.t)
        (action : Action.t)
        destinations
        acc
    =
    States.fold
      (fun (goto : State.t) (acc : Transitions.t) ->
        let new_transition : Transition.t =
          { from
          ; label = action.label
          ; goto
          ; constructor_trees = Some action.constructor_trees
          }
        in
        Transitions.add new_transition acc)
      destinations
      acc
  ;;
end

type kind =
  | LTS of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * Transitions.t
      * Info.t option)
  | FSM of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * States.t Actions.t Edges.t
      * Info.t option)

module Lts = struct
  type t =
    { init : State.t option
    ; terminals : States.t
    ; alphabet : Alphabet.t
    ; states : States.t
    ; transitions : Transitions.t
    ; info : Info.t option
    }

  let to_model ({ init; terminals; alphabet; states; transitions; info } : t)
    : kind
    =
    LTS (init, terminals, alphabet, states, transitions, info)
  ;;

  let of_model : kind -> t = function
    | LTS (init, terminals, alphabet, states, transitions, info) ->
      { init; terminals; alphabet; states; transitions; info }
    | FSM (init, terminals, alphabet, states, edges, info) ->
      let transitions = Convert.edges_to_transitions edges in
      { init; terminals; alphabet; states; transitions; info }
  ;;

  let to_string
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (x : t)
    : string
    =
    "TODO: Model.Lts.to_string"
  ;;
end

module Fsm = struct
  type t =
    { init : State.t option
    ; terminals : States.t
    ; alphabet : Alphabet.t
    ; states : States.t
    ; edges : States.t Actions.t Edges.t
    ; info : Info.t option
    }

  and pair = t * t

  let to_model ({ init; terminals; alphabet; states; edges; info } : t) : kind =
    FSM (init, terminals, alphabet, states, edges, info)
  ;;

  let clone (x : t) : t = { x with edges = Edges.copy x.edges }

  let merge (a : t) (b : t) : t =
    Logging.Log.warning "TODO: Model.Fsm.merge";
    a
  ;;

  exception Model_Fsm_OriginOfStateNotFound of (State.t * pair)

  (** similar to [compare], returns [-1] if [s] is only in [fst p], [1] if only in [snd p] and [0] if in both of [p]
  *)
  let origin_of_state_opt (s : State.t) (a : t) (b : t) : int option =
    match States.mem s a.states, States.mem s b.states with
    | true, true -> Some 0
    | true, false -> Some (-1)
    | false, true -> Some 1
    | false, false -> None
  ;;

  let origin_of_state (s : State.t) (a : t) (b : t) : int =
    match origin_of_state_opt s a b with
    | None -> raise (Model_Fsm_OriginOfStateNotFound (s, (a, b)))
    | Some i -> i
  ;;

  let to_string
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (x : t)
    : string
    =
    "TODO: Model.Fsm.to_string"
  ;;
end

module Saturate = struct
  exception
    Model_Saturate_CannotSaturateActionsWithUnknownVisibility of Action.t

  module StateTracker = Hashtbl.Make (struct
      include State
    end)

  let max_visit_num : int = 2

  let can_revisit (s : State.t) (visited_states : int StateTracker.t) : bool =
    match StateTracker.find_opt visited_states s with
    | None ->
      StateTracker.add visited_states s 0;
      true
    | Some n -> n < max_visit_num
  ;;

  let log_visit (s : State.t) (visited_states : int StateTracker.t) : unit =
    match StateTracker.find_opt visited_states s with
    | None -> StateTracker.add visited_states s 1
    | Some n -> StateTracker.replace visited_states s (n + 1)
  ;;

  let is_action_silent (x : Action.t) : bool =
    match x.label.is_silent with
    | None ->
      raise (Model_Saturate_CannotSaturateActionsWithUnknownVisibility x)
    | Some is_silent -> is_silent
  ;;

  let check_update_named (x : Action.t) : Action.t option -> Action.t option
    = function
    | Some y -> Some y
    | None -> if is_action_silent x then None else Some x
  ;;

  let update_named_annotations
        (named : Action.t)
        (annotations : Action.annotations)
    : Action.t
    =
    let annotations_list : Action.annotations list =
      if List.mem annotations named.annotations_list
      then named.annotations_list
      else annotations :: named.annotations_list
    in
    { named with annotations_list }
  ;;

  let stop
        ?(named : Action.t option = None)
        ?(annotations : Action.annotations = [])
        (from : State.t)
        (acc : (Action.t * States.t) list)
    : (Action.t * States.t) list
    =
    match named with
    | None -> acc
    | Some named_action ->
      (update_named_annotations named_action annotations, States.singleton from)
      :: acc
  ;;

  (** [check_from] explores the outgoing actions of state [from], which is some destination of another action.
  *)
  let rec check_from
            ?(named : Action.t option = None)
            ?(annotations : Action.annotations = [])
            (old_edges : States.t Actions.t Edges.t)
            (from : State.t)
            (visited : int StateTracker.t)
            (acc : (Action.t * States.t) list)
    : (Action.t * States.t) list
    =
    log_visit from visited;
    if can_revisit from visited
    then (
      match Edges.find_opt old_edges from with
      | None -> stop ~named ~annotations from acc
      | Some old_actions ->
        check_actions ~named ~annotations old_edges from old_actions visited acc)
    else stop ~named ~annotations from acc

  and check_actions
        ?(named : Action.t option = None)
        ?(annotations : Action.annotations = [])
        (old_edges : States.t Actions.t Edges.t)
        (from : State.t)
        (old_actions : States.t Actions.t)
        (visited : int StateTracker.t)
        (acc : (Action.t * States.t) list)
    : (Action.t * States.t) list
    =
    (* NOTE: flag used to make sure we don't add duplicates to [acc] in the case that multiple named-actions are outgoing from state [from] when a [named] action has already been identified. *)
    let skip : bool ref = ref false in
    Actions.fold
      (fun (the_action : Action.t)
        (destinations : States.t)
        (acc : (Action.t * States.t) list) ->
        if is_action_silent the_action
        then (
          (* NOTE: add to annotation, continue exploring outwards *)
          let annotations : Action.annotations =
            { from; via = the_action.label } :: annotations
          in
          check_destinations
            ~named
            ~annotations
            old_edges
            destinations
            visited
            acc)
        else
          (* NOTE: check if we have already found the named action or not *)
          check_named
            ~named
            ~annotations
            old_edges
            from
            the_action
            destinations
            visited
            acc
            skip)
      old_actions
      acc

  and check_named
        ?(named : Action.t option = None)
        ?(annotations : Action.annotations = [])
        (old_edges : States.t Actions.t Edges.t)
        (from : State.t)
        (the_action : Action.t)
        (destinations : States.t)
        (visited : int StateTracker.t)
        (acc : (Action.t * States.t) list)
        (skip : bool ref)
    : (Action.t * States.t) list
    =
    match named with
    | None ->
      (* NOTE: we have found the named action, continue *)
      check_destinations
        ~named:(Some the_action)
        ~annotations
        old_edges
        destinations
        visited
        acc
    | Some _ ->
      (* NOTE: we already found the named action, so stop *)
      if !skip
      then acc
      else (
        skip := true;
        stop ~named ~annotations from acc)

  and check_destinations
        ?(named : Action.t option = None)
        ?(annotations : Action.annotations = [])
        (old_edges : States.t Actions.t Edges.t)
        (the_destinations : States.t)
        (visited : int StateTracker.t)
        (acc : (Action.t * States.t) list)
    : (Action.t * States.t) list
    =
    States.fold
      (fun (destination : State.t) (acc : (Action.t * States.t) list) ->
        check_from ~named ~annotations old_edges destination visited acc)
      the_destinations
      acc
  ;;

  (** [merge_saturated_tuples a b] merges elements of [b] into [a], either by updating an element in [a] with additional annotations for a saturation tuple that describes the same action-destination, or in the case that the saturation tuple is not described within [a] by inserting it within [a].
  *)
  let rec merge_saturated_tuples
            (a : (Action.t * States.t) list)
              (* (b : (Action.t * States.t) list) *)
    : (Action.t * States.t) list -> (Action.t * States.t) list
    = function
    | [] -> a
    | h :: tl ->
      let (a : (Action.t * States.t) list) =
        match try_update_saturated_tuple h a with
        | None, a -> h :: a
        | Some updated, a -> updated :: a
      in
      merge_saturated_tuples a tl

  (** [try_update_saturated_tuple x a] returns [None, a] when [x] cannot be used to update a pre-existing tuple in [a], and [Some z, a'] where [z] is the updated tuple in [a] which has been removed in [a'].
  *)
  and try_update_saturated_tuple
        ((xaction, xdestinations) : Action.t * States.t)
        (a : (Action.t * States.t) list)
    : (Action.t * States.t) option * (Action.t * States.t) list
    =
    List.fold_left
      (fun ((updated_opt, acc) :
             (Action.t * States.t) option * (Action.t * States.t) list)
        (y : Action.t * States.t) ->
        match updated_opt with
        | Some opt -> Some opt, y :: acc
        | None ->
          let yaction, ydestinations = y in
          if
            Action.check_equal
              ~annotations:false
              ~constructor_trees:false
              xaction
              yaction
            && States.equal xdestinations ydestinations
          then (
            let zaction : Action.t =
              { yaction with
                annotations_list =
                  List.append yaction.annotations_list xaction.annotations_list
              ; constructor_trees =
                  List.append
                    yaction.constructor_trees
                    xaction.constructor_trees
              }
            in
            Some (zaction, ydestinations), acc)
          else None, y :: acc)
      (None, [])
      a
  ;;

  (** [edge_action_destinations] returns a list of saturated actions tupled with their respective destinations, which is the reflexive-transitive closure of visible actions that may weakly be performed from each of [the_destinations].
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge_action_destinations
        ?(named : Action.t option = None)
        ?(annotations : Action.annotations = [])
        (old_edges : States.t Actions.t Edges.t)
        (from : State.t)
        (the_destinations : States.t)
        (acc : (Action.t * States.t) list)
    : (Action.t * States.t) list
    =
    States.fold
      (fun (the_destination : State.t) (acc : (Action.t * States.t) list) ->
        let visited : int StateTracker.t = StateTracker.create 0 in
        log_visit from visited;
        let saturated_tuples : (Action.t * States.t) list =
          check_from ~named ~annotations old_edges the_destination visited []
        in
        merge_saturated_tuples acc saturated_tuples)
      the_destinations
      acc
  ;;

  (** [edge_actions] returns a list of saturated actions tupled with their respective destinations, obtained from [edge_action_destinations] which explores the reflexive-transitive closure
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge_actions
        ?(named : Action.t option = None)
        ?(annotations : Action.annotations = [])
        (old_edges : States.t Actions.t Edges.t)
        (from : State.t)
        (old_actions : States.t Actions.t)
        (acc : (Action.t * States.t) list)
    : (Action.t * States.t) list
    =
    Actions.fold
      (fun (the_action : Action.t)
        (the_destinations : States.t)
        (acc : (Action.t * States.t) list) ->
        let named : Action.t option = check_update_named the_action named in
        let annotations : Action.annotations =
          [ { from; via = the_action.label } ]
        in
        edge_action_destinations
          ~named
          ~annotations
          old_edges
          from
          the_destinations
          acc)
      old_actions
      acc
  ;;

  (** [edge] updates [new_actions] with actions saturated by [edge_actions]
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge
        (new_actions : States.t Actions.t)
        (old_edges : States.t Actions.t Edges.t)
        (from : State.t)
        (old_actions : States.t Actions.t)
    : unit
    =
    let updated_actions : (Action.t * States.t) list =
      edge_actions ~named:None old_edges from old_actions []
    in
    List.iter
      (fun ((saturated_action, destinations) : Action.t * States.t) ->
        update_action new_actions saturated_action destinations)
      updated_actions
  ;;

  let edges
        (labels : Alphabet.t)
        (states : States.t)
        (old_edges : States.t Actions.t Edges.t)
    : States.t Actions.t Edges.t
    =
    (* TODO: makes sense for this to be empty, as all actions must be saturated with a label *)
    (* let new_edges : States.t Actions.t Edges.t = Edges.copy old_edges in *)
    let new_edges : States.t Actions.t Edges.t = Edges.create 0 in
    Edges.iter
      (fun (from : State.t) (old_actions : States.t Actions.t) ->
        let new_actions : States.t Actions.t = Actions.create 0 in
        (* NOTE: populate [new_actions] with saturated [old_actions] *)
        let () = edge new_actions old_edges from old_actions in
        (* ! NOTE: only add to [new_edges] if we can saturate any *)
        (* TODO: check this later *)
        (* if Actions.length new_actions > 0 then *)
        Edges.replace new_edges from new_actions)
      old_edges;
    new_edges
  ;;

  let fsm (x : Fsm.t) : Fsm.t =
    { x with edges = edges x.alphabet x.states (Edges.copy x.edges) }
  ;;
end
