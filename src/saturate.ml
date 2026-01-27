module type S = sig
  module Model : Model.S

  val fsm : Model.FSM.t -> Model.FSM.t
end

module Make (M : Model.S) : S = struct
  module Model : Model.S = M

  (* *)
  module Tree = Model.Tree
  module Trees = Model.Trees
  module State = Model.State
  module Label = Model.Label
  module Labels = Model.Labels
  module Note = Model.Note
  module Annotation = Model.Annotation
  module Annotations = Model.Annotations
  module Action = Model.Action
  module States = Model.States
  module ActionMap = Model.ActionMap
  module EdgeMap = Model.EdgeMap
  module FSM = Model.FSM

  module type SActionPair = sig
    type t = Action.t * States.t

    val to_string : t -> string
    val compare : t -> t -> int
  end

  module ActionPair : SActionPair = struct
    type t = Action.t * States.t

    let to_string ((a, b) : t) =
      Utils.Strfy.record
        [ "action", Action.to_string a; "destinations", States.to_string b ]
    ;;

    let compare ((a, x) : t) ((b, y) : t) : int =
      Utils.compare_chain [ Action.compare a b; States.S.compare x y ]
    ;;
  end

  module type SActionPairs = sig
    module S : Set.S with type elt = ActionPair.t

    type t = S.t

    val merge_saturated_tuples
      :  ActionPair.t list
      -> ActionPair.t list
      -> ActionPair.t list
  end

  module ActionPairs : SActionPairs with type S.elt = ActionPair.t = struct
    module S = Set.Make (ActionPair)

    type t = S.t

    (** [merge_saturated_tuples a b] merges elements of [b] into [a], either by updating an element in [a] with additional annotation for a saturation tuple that describes the same action-destination, or in the case that the saturation tuple is not described within [a] by inserting it within [a].
    *)
    let rec merge_saturated_tuples (a : ActionPair.t list)
      : ActionPair.t list -> ActionPair.t list
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
          ((xaction, xdestinations) : ActionPair.t)
          (a : ActionPair.t list)
      : ActionPair.t option * ActionPair.t list
      =
      let f : Annotation.t option * Annotation.t option -> Annotation.t option =
        function
        | None, None -> None
        | None, y -> y
        | x, None -> x
        | Some x, Some y -> Some (Annotation.shorter x y)
      in
      List.fold_left
        (fun ((updated_opt, acc) :
               (Action.t * States.t) option * (Action.t * States.t) list)
          ((yaction, ydestinations) : Action.t * States.t) ->
          match updated_opt with
          | Some opt -> Some opt, (yaction, ydestinations) :: acc
          | None ->
            if
              Action.wk_equal xaction yaction
              && States.S.equal xdestinations ydestinations
            then (
              let annotation : Annotation.t option =
                f (yaction.annotation, xaction.annotation)
              in
              let zaction : Action.t =
                { label = yaction.label
                ; annotation
                ; constructor_trees =
                    Trees.union
                      yaction.constructor_trees
                      xaction.constructor_trees
                }
              in
              Some (zaction, ydestinations), acc)
            else None, (yaction, ydestinations) :: acc)
        (None, [])
        a
    ;;
  end

  type data =
    { mutable named : Action.t option
    ; mutable notes : wip list
    ; mutable visited : States.t
    ; old_edges : EdgeMap.t
    }

  and wip =
    { from : State.t
    ; via : Label.t
    ; trees : Trees.t
    }

  let wip (from : State.t) (action : Action.t) : wip =
    { from; via = action.label; trees = action.constructor_trees }
  ;;

  let initial_data (old_edges : EdgeMap.t) : data =
    { named = None; notes = []; visited = States.S.empty; old_edges }
  ;;

  (****************************************************************************)

  (** returns a copy of [d] with the updated name *)
  let update_named (x : Action.t) (d : data) : data =
    let f (x : Action.t) (d : data) : Action.t option =
      Option.cata
        (fun y -> Some y)
        (if Action.is_silent x then None else Some x)
        d.named
    in
    { d with named = f x d }
  ;;

  (** returns a copy of [d] with the updated notes *)
  let update_notes (from : State.t) (action : Action.t) (d : data) : data =
    let f (from : State.t) (action : Action.t) (d : data) : wip list =
      wip from action :: d.notes
    in
    { d with notes = f from action d }
  ;;

  (** returns a copy of [d] with the updated visited *)
  let update_visited (x : State.t) (d : data) : data =
    let f (x : State.t) (d : data) : States.t = States.S.add x d.visited in
    { d with visited = f x d }
  ;;

  (****************************************************************************)

  let already_visited (x : State.t) (d : data) : bool = States.S.mem x d.visited

  let skip_action (x : Action.t) (d : data) : bool =
    if Action.is_silent x then false else Option.has_some d.named
  ;;

  let get_old_actions (from : State.t) (d : data) : ActionMap.t option =
    EdgeMap.M.find_opt d.old_edges from
  ;;

  (****************************************************************************)

  exception Model_Saturate_WIP_IsEmptyList of unit

  let wip_to_annotation (goto : State.t) (xs : wip list) : Annotation.t =
    let rec f : wip list -> Annotation.t = function
      | [] -> raise (Model_Saturate_WIP_IsEmptyList ())
      | { from; via; trees } :: [] ->
        { this = { from; label = via; using = trees; goto }; next = None }
      | { from; via; trees } :: h :: tl ->
        let { from = goto; via = via2; trees = tree2 } = h in
        { this = { from; label = via; using = trees; goto }
        ; next = Some (f (h :: tl))
        }
    in
    f (List.rev xs)
  ;;

  (****************************************************************************)

  exception Model_Saturate_WIP_HadNoNamedActions of wip list
  exception Model_Saturate_WIP_HadMultipleNamedActions of wip list

  let validate_wips (xs : wip list) : unit =
    match
      List.filter (fun ({ via; _ } : wip) -> Label.is_silent via |> Bool.not) xs
    with
    | [] -> raise (Model_Saturate_WIP_HadNoNamedActions xs)
    | _ :: [] -> ()
    | _ :: _ -> raise (Model_Saturate_WIP_HadMultipleNamedActions xs)
  ;;

  (** returns all of the possible actions after the named action *)
  let extrapolate_annotations (x : Annotation.t) : Annotations.t =
    (* NOTE: skip pre-named action *)
    let rec skip ({ this; next } : Annotation.t) : Annotations.t =
      let xs =
        Option.cata
          (if Note.is_silent this then skip else get)
          Annotations.S.empty
          next
        |> Annotations.S.map (fun (y : Annotation.t) -> { this; next = Some y })
      in
      (* NOTE: don't forget to add this action if named *)
      if Note.is_silent this
      then xs
      else Annotations.S.add { this; next = None } xs
    (* NOTE: get every annotation from named action onwards *)
    and get : Annotation.t -> Annotations.t = function
      | { this; next = None } -> Annotations.S.singleton { this; next = None }
      | { this; next = Some next } ->
        get next
        |> Annotations.S.map (fun (y : Annotation.t) -> { this; next = Some y })
        |> Annotations.S.add { this; next = None }
    in
    Annotations.S.add x (skip x)
  ;;

  (****************************************************************************)

  (** [stop] *)
  let stop (d : data) (goto : State.t) (acc : ActionPairs.t) : ActionPairs.t =
    match d.named with
    | None -> acc
    | Some named ->
      let () = validate_wips d.notes in
      wip_to_annotation goto d.notes
      |> extrapolate_annotations
      |> Annotations.S.to_list
      |> List.map (fun (x : Annotation.t) : ActionPair.t ->
        let y : Action.t =
          { label = named.label
          ; annotation = Some x
          ; constructor_trees = Trees.empty
          }
        in
        y, States.S.singleton (Annotation.last x).goto)
      |> List.fold_left
           (fun (acc : ActionPairs.t) ((a, s) : ActionPair.t) ->
             (* NOTE: merge destinations of equal actions *)
             let matching =
               ActionPairs.S.filter
                 (fun ((b, t) : ActionPair.t) -> Action.equal a b)
                 acc
             in
             if ActionPairs.S.is_empty matching
             then ActionPairs.S.add (a, s) acc
             else (
               (* NOTE: update states of each matching *)
               let acc = ActionPairs.S.diff acc matching in
               matching
               |> ActionPairs.S.to_list
               |> List.map (fun (_, t) -> a, States.S.union s t)
               |> ActionPairs.S.of_list
               |> ActionPairs.S.union acc))
           acc
  ;;

  (****************************************************************************)

  (** [check_from] explores the outgoing actions of state [from], which is some destination of another action.
  *)
  let rec check_from (d : data) (from : State.t) (acc : ActionPairs.t)
    : ActionPairs.t
    =
    if already_visited from d
    then stop d from acc
    else (
      let d : data = update_visited from d in
      match get_old_actions from d with
      | None -> stop d from acc
      | Some old_actions -> check_actions d from old_actions acc)

  and check_actions (d : data) (from : State.t) (xs : ActionMap.t)
    : ActionPairs.t -> ActionPairs.t
    =
    ActionMap.M.fold
      (fun (x : Action.t) (ys : States.t) (acc : ActionPairs.t) ->
        if skip_action x d
        then stop d from acc
        else (
          let d : data (* NOTE: copy [d] *) = update_notes from x d in
          let d : data = update_named x d in
          check_destinations d from ys acc))
      xs

  and check_destinations (d : data) (from : State.t) (xs : States.t)
    : ActionPairs.t -> ActionPairs.t
    =
    States.S.fold (check_from d) xs
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
    States.S.fold
      (fun (y : State.t) (acc : ActionPairs.t) ->
        check_from d y ActionPairs.S.empty)
      ys
      ActionPairs.S.empty
  ;;

  (** [edge_actions] returns a list of saturated actions tupled with their respective destinations, obtained from [edge_action_destinations] which explores the reflexive-transitive closure
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge_actions
        (from : State.t)
        (old_actions : ActionMap.t)
        (old_edges : EdgeMap.t)
    : ActionPairs.t
    =
    ActionMap.M.fold
      (fun (x : Action.t) (ys : States.t) (acc : ActionPair.t list) ->
        let d : data =
          initial_data old_edges |> update_named x |> update_notes from x
        in
        edge_action_destinations d from ys
        |> ActionPairs.S.to_list
        |> ActionPairs.merge_saturated_tuples acc)
      old_actions
      []
    |> ActionPairs.S.of_list
  ;;

  (** [edge] updates [new_actions] with actions saturated by [edge_actions]
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge
        (new_actions : ActionMap.t)
        (from : State.t)
        (old_actions : ActionMap.t)
        (old_edges : EdgeMap.t)
    : unit
    =
    edge_actions from old_actions old_edges
    |> ActionPairs.S.iter
         (fun ((saturated_action, destinations) : Action.t * States.t) ->
         ActionMap.update new_actions saturated_action destinations)
  ;;

  (** [] *)
  let edges (labels : Labels.t) (states : States.t) (old_edges : EdgeMap.t)
    : EdgeMap.t
    =
    let new_edges : EdgeMap.t = EdgeMap.M.create 0 in
    EdgeMap.M.iter
      (fun (from : State.t) (old_actions : ActionMap.t) ->
        (* NOTE: populate [new_actions] with saturated [old_actions] *)
        let new_actions : ActionMap.t = ActionMap.M.create 0 in
        let () = edge new_actions from old_actions old_edges in
        EdgeMap.M.replace new_edges from new_actions)
      old_edges;
    new_edges
  ;;

  let fsm (x : FSM.t) : FSM.t =
    { x with edges = edges x.alphabet x.states (EdgeMap.M.copy x.edges) }
  ;;
end
