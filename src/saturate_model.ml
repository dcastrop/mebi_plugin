open Model

(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.Default

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

let fstate : State.t Logger.to_string = Args State.to_string
let fenc : Enc.t Logger.to_string = Of Enc.to_string

let fannotation : Note.annotation Logger.to_string =
  Args Note.annotation_to_string
;;

(***********************************************************************)

(** [merge_saturated_tuples a b] merges elements of [b] into [a], either by updating an element in [a] with additional annotation for a saturation tuple that describes the same action-destination, or in the case that the saturation tuple is not described within [a] by inserting it within [a].
*)
let rec merge_saturated_tuples (a : (Action.t * States.t) list)
  : (Action.t * States.t) list -> (Action.t * States.t) list
  =
  Log.trace __FUNCTION__;
  function
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
  Log.trace __FUNCTION__;
  let f
    : Note.annotation option * Note.annotation option -> Note.annotation option
    = function
    | None, None -> None
    | None, y -> y
    | x, None -> x
    | Some x, Some y -> Some (Note.shorter_annotation x y)
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
          && States.equal xdestinations ydestinations
        then (
          let annotation = f (yaction.annotation, xaction.annotation) in
          let zaction : Action.t =
            Action.create
              yaction.label
              ~annotation
              ~constructor_trees:
                (Merge.constructor_trees
                   yaction.constructor_trees
                   xaction.constructor_trees)
              ()
          in
          Some (zaction, ydestinations), acc)
        else None, (yaction, ydestinations) :: acc)
    (None, [])
    a
;;

type data =
  { mutable named : Action.t option
  ; mutable notes : wip list
  ; mutable visited : States.t
  ; old_edges : States.t Actions.t Edges.t
  }

and actionpair = Action.t * States.t
and actionpairs = actionpair list

and wip =
  { from : State.t
  ; via : Label.t
  ; tree : Tree.t list
  }

let _debug_wips ?(__FUNCTION__ : string = "") (xs : wip list) : unit =
  let f : Enc.t -> string = Mebi_setup.Enc.to_string in
  let g ({ from; via; tree } : wip) : string =
    Printf.sprintf "<From (%s) Via (%s)>" (f from.enc) (f via.enc)
  in
  Log.things ~__FUNCTION__ Debug "wips" (List.rev xs) (Of g)
;;

let _debug_notes ?(__FUNCTION__ : string = "") (d : data) : unit =
  let f : Enc.t -> string = Mebi_setup.Enc.to_string in
  let g ({ from; via; tree } : wip) : string =
    Printf.sprintf "<From (%s) Via (%s)>" (f from.enc) (f via.enc)
  in
  Log.things ~__FUNCTION__ Debug "notes" (List.rev d.notes) (Of g)
;;

let _debug_data
      ?(__FUNCTION__ : string = "")
      ({ named; notes; visited; old_edges } : data)
  : unit
  =
  let f (x : Action.t) : string = Enc.to_string x.label.enc in
  Log.thing ~__FUNCTION__ Debug "named" named (Of (Option.cata f "None"));
  _debug_wips notes;
  Log.debug (Printf.sprintf "data, visited: %s" (states_to_string visited));
  ()
;;

let wip (from : State.t) (action : Action.t) : wip =
  { from; via = action.label; tree = action.constructor_trees }
;;

let initial_data (old_edges : States.t Actions.t Edges.t) : data =
  { named = None; notes = []; visited = States.empty; old_edges }
;;

(****************************************************************************)

let f_update_named (x : Action.t) (d : data) : Action.t option =
  Option.cata
    (fun y -> Some y)
    (if is_action_silent x then None else Some x)
    d.named
;;

let _update_named (x : Action.t) (d : data) : data =
  d.named <- f_update_named x d;
  d
;;

(** returns a copy of [d] with the updated name *)
let _update_named' (x : Action.t) (d : data) : data =
  { d with named = f_update_named x d }
;;

(****************************************************************************)

let f_update_notes (from : State.t) (action : Action.t) (d : data) : wip list =
  wip from action :: d.notes
;;

let _update_notes (from : State.t) (action : Action.t) (d : data) : data =
  d.notes <- f_update_notes from action d;
  d
;;

(** returns a copy of [d] with the updated notes *)
let _update_notes' (from : State.t) (action : Action.t) (d : data) : data =
  { d with notes = f_update_notes from action d }
;;

(****************************************************************************)

let f_update_visited (x : State.t) (d : data) : States.t =
  States.add x d.visited
;;

let _update_visited (x : State.t) (d : data) : data =
  d.visited <- f_update_visited x d;
  d
;;

(** returns a copy of [d] with the updated visited *)
let _update_visited' (x : State.t) (d : data) : data =
  { d with visited = f_update_visited x d }
;;

(****************************************************************************)

let already_visited (x : State.t) (d : data) : bool = States.mem x d.visited

let skip_action (x : Action.t) (d : data) : bool =
  if is_action_silent x then false else Option.has_some d.named
;;

let get_old_actions (from : State.t) (d : data) : States.t Actions.t option =
  Edges.find_opt d.old_edges from
;;

let _has_only_named_actions (from : State.t) (d : data) : bool =
  match get_old_actions from d with
  | None -> false
  | Some actions ->
    Actions.to_seq_keys actions
    |> List.of_seq
    |> List.for_all (fun (x : Action.t) -> is_action_silent x |> Bool.not)
;;

exception Model_Saturate_WIP_IsEmptyList of unit

let wip_to_annotation (goto : State.t) (xs : wip list) : Note.annotation =
  let rec f : wip list -> Note.annotation =
    Log.trace __FUNCTION__;
    function
    | [] -> raise (Model_Saturate_WIP_IsEmptyList ())
    | { from; via; tree } :: [] ->
      { this = Note.create from via tree goto; next = None }
    | { from; via; tree } :: h :: tl ->
      let { from = goto; via = via2; tree = tree2 } = h in
      { this = Note.create from via tree goto; next = Some (f (h :: tl)) }
  in
  f (List.rev xs)
;;

exception Model_Saturate_WIP_HadNoNamedActions of wip list
exception Model_Saturate_WIP_HadMultipleNamedActions of wip list

let validate_wips (xs : wip list) : unit =
  match
    List.filter (fun ({ via; _ } : wip) -> Label.is_silent via |> Bool.not) xs
  with
  | [] ->
    _debug_wips xs;
    raise (Model_Saturate_WIP_HadNoNamedActions xs)
  | _ :: [] -> ()
  | _ :: _ ->
    _debug_wips xs;
    raise (Model_Saturate_WIP_HadMultipleNamedActions xs)
;;

(** [stop] *)
let stop (d : data) (goto : State.t) (acc : actionpairs) : actionpairs =
  Log.trace __FUNCTION__;
  match d.named with
  | None -> acc
  | Some named ->
    validate_wips d.notes;
    _debug_notes d;
    let annotation = Some (wip_to_annotation goto d.notes) in
    Log.option ~__FUNCTION__ Debug "annotation" annotation fannotation;
    let x : Action.t = Action.create named.label ~annotation () in
    (x, States.singleton goto) :: acc
;;

(** [check_from] explores the outgoing actions of state [from], which is some destination of another action.
*)
let rec check_from (d : data) (from : State.t) (acc : actionpairs) : actionpairs
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "from" from.enc fenc;
  if already_visited from d
  then (
    Log.thing ~__FUNCTION__ Debug "already visited" from.enc fenc;
    stop d from acc)
  else (
    let d : data = _update_visited' from d in
    _debug_data d;
    match get_old_actions from d with
    | None ->
      Log.thing ~__FUNCTION__ Debug "no actions from" from.enc fenc;
      stop d from acc
    | Some old_actions ->
      Log.thing ~__FUNCTION__ Debug "checking actions from" from.enc fenc;
      check_actions d from old_actions acc)

and check_actions (d : data) (from : State.t) (xs : States.t Actions.t)
  : actionpairs -> actionpairs
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "from" from.enc fenc;
  (* NOTE: [xsl] is just for debugging *)
  (* let () =
    let xsl : Action.t list = Actions.to_seq_keys xs |> List.of_seq in
    Log.things ~__FUNCTION__ Debug "actions" xsl factionenc
  in *)
  Actions.fold
    (fun (x : Action.t) (ys : States.t) (acc : actionpairs) ->
      if skip_action x d
      then (
        Log.thing ~__FUNCTION__ Debug "skipping" x.label.enc fenc;
        stop d from acc)
      else (
        Log.thing ~__FUNCTION__ Debug "continuing" x.label.enc fenc;
        let d : data (* NOTE: copy [d] *) = _update_notes' from x d in
        let d : data = _update_named' x d in
        check_destinations d from ys acc))
    xs

and check_destinations (d : data) (from : State.t) (xs : States.t)
  : actionpairs -> actionpairs
  =
  Log.trace __FUNCTION__;
  States.fold
    (fun (x : State.t) (acc : actionpairs) ->
      Log.thing ~__FUNCTION__ Debug "from" from.enc fenc;
      check_from d x acc)
    xs
;;

(** [edge_action_destinations] returns a list of saturated actions tupled with their respective destinations, which is the reflexive-transitive closure of visible actions that may weakly be performed from each of [the_destinations].
    edge -> edge_actions -> edge_action_destinations -> ( ... )
    @param ys
      is the set of destination [States.t] reachable from state [from] via actions that have already been recorded in [d.notes] as a [wip].
*)
let edge_action_destinations (d : data) (from : State.t) (ys : States.t)
  : actionpairs
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "from" from fstate;
  _debug_data d;
  States.fold (fun (y : State.t) (acc : actionpairs) -> check_from d y []) ys []
;;

(** [edge_actions] returns a list of saturated actions tupled with their respective destinations, obtained from [edge_action_destinations] which explores the reflexive-transitive closure
    edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
let edge_actions
      (from : State.t)
      (old_actions : States.t Actions.t)
      (old_edges : States.t Actions.t Edges.t)
  : actionpairs
  =
  Log.trace __FUNCTION__;
  Actions.fold
    (fun (x : Action.t) (ys : States.t) (acc : actionpairs) ->
      let d : data =
        initial_data old_edges |> _update_named' x |> _update_notes' from x
      in
      edge_action_destinations d from ys |> merge_saturated_tuples acc)
    old_actions
    []
;;

(** [edge] updates [new_actions] with actions saturated by [edge_actions]
    edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
let edge
      (new_actions : States.t Actions.t)
      (from : State.t)
      (old_actions : States.t Actions.t)
      (old_edges : States.t Actions.t Edges.t)
  : unit
  =
  Log.trace __FUNCTION__;
  edge_actions from old_actions old_edges
  |> List.iter (fun ((saturated_action, destinations) : Action.t * States.t) ->
    update_action new_actions saturated_action destinations)
;;

(** [] *)
let edges
      (labels : Alphabet.t)
      (states : States.t)
      (old_edges : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  Log.trace __FUNCTION__;
  let new_edges : States.t Actions.t Edges.t = Edges.create 0 in
  Edges.iter
    (fun (from : State.t) (old_actions : States.t Actions.t) ->
      (* NOTE: populate [new_actions] with saturated [old_actions] *)
      let new_actions : States.t Actions.t = Actions.create 0 in
      let () = edge new_actions from old_actions old_edges in
      Edges.replace new_edges from new_actions)
    old_edges;
  new_edges
;;

let fsm (x : Fsm.t) : Fsm.t =
  Log.trace __FUNCTION__;
  { x with edges = edges x.alphabet x.states (Edges.copy x.edges) }
;;
