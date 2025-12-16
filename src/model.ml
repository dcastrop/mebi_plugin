open Logging
module Info = Model_info
module State = Model_state
module Label = Model_label
module Note = Model_note
module Transition = Model_transition
module Transition_opt = Model_transition_opt
module Action = Model_action
module Edge = Model_edge
module Enc = Mebi_setup.Enc
module Tree = Mebi_constr.Tree

(* the prefix *)
let trace_enabled : bool = true
let log_trace (x : string) : unit = if trace_enabled then Log.trace x else ()
let nest = Utils.Strfy.nest

type style_args = Utils.Strfy.style_args

let style_args = Utils.Strfy.style_args
let collection_style = Utils.Strfy.collection_style

(***********************************************************************)
(*** States ************************************************************)
(***********************************************************************)

module States = Set.Make (struct
    include State
  end)

let states_to_string
      ?(args : style_args = style_args ~name:"States" ())
      (x : States.t)
  : string
  =
  let args : style_args =
    Utils.Strfy.nest
      { args with style = Some (collection_style List); newline = true }
  in
  States.to_list x |> Utils.Strfy.list ~args State.to_string
;;

(** same as [states_to_string] except with name "Destinations" *)
let destinations_to_string ?(args : style_args = style_args ())
  : States.t -> string
  =
  states_to_string ~args:{ args with name = Some "Destinations" }
;;

let decode_state_opt (x : Enc.t) : States.t -> State.t option =
  Log.trace "Model.decode_state_opt";
  States.find_opt { enc = x; pp = None }
;;

exception Model_CannotDecodeState of (Enc.t * States.t)

let decode_state (x : Enc.t) (states : States.t) : State.t =
  Log.trace "Model.decode_state";
  match decode_state_opt x states with
  | None -> raise (Model_CannotDecodeState (x, states))
  | Some state -> state
;;

(***********************************************************************)
(*** Partition *********************************************************)
(***********************************************************************)

module Partition = Set.Make (States)

let partition_to_string ?(args : style_args = style_args ()) (x : Partition.t)
  : string
  =
  let args : style_args =
    Utils.Strfy.nest
      { args with
        name = Some "Partition"
      ; style = Some (collection_style List)
      ; newline = true
      }
  in
  Partition.to_list x |> Utils.Strfy.list ~args states_to_string
;;

exception Model_Bisim_State_NotFound of (State.t * Partition.t)

let get_bisim_states (x : State.t) (pi : Partition.t) : States.t =
  Log.trace "Model.get_bisim_states";
  try
    Partition.to_list pi
    |> List.find (fun (block : States.t) -> States.mem x block)
  with
  | Not_found -> raise (Model_Bisim_State_NotFound (x, pi))
;;

(***********************************************************************)
(*** Alphabet **********************************************************)
(***********************************************************************)

module Alphabet = Set.Make (struct
    include Label
  end)

let alphabet_to_string ?(args : style_args = style_args ()) (x : Alphabet.t)
  : string
  =
  let args : style_args =
    Utils.Strfy.nest
      { args with
        name = Some "Alphabet"
      ; style = Some (collection_style List)
      ; newline = true
      }
  in
  Alphabet.to_list x |> Utils.Strfy.list ~args Label.to_string
;;

exception Model_Alphabet_LabelOfEncNotFound of (Enc.t * Alphabet.t)

let find_label_of_enc (x : Enc.t) (alphabet : Alphabet.t) : Label.t =
  Log.trace "Model.find_label_of_enc";
  try
    Alphabet.to_list alphabet
    |> List.find (fun ({ enc = y; _ } : Label.t) -> Enc.equal x y)
  with
  | Not_found -> raise (Model_Alphabet_LabelOfEncNotFound (x, alphabet))
;;

let silent_label_opt (alphabet : Alphabet.t) : Label.t option =
  Log.trace "Model.silent_label_opt";
  Alphabet.to_list alphabet
  |> List.find_opt (fun (x : Label.t) ->
    Option.cata (fun (y : bool) -> y) false x.is_silent)
;;

exception Model_Alphabet_SilentLabelNotFound of Alphabet.t

let silent_label (xs : Alphabet.t) : Label.t =
  Log.trace "Model.silent_label";
  match silent_label_opt xs with
  | None -> raise (Model_Alphabet_SilentLabelNotFound xs)
  | Some x -> x
;;

(***********************************************************************)
(*** Transitions *******************************************************)
(***********************************************************************)

module Transitions = Set.Make (struct
    include Transition
  end)

let transitions_to_string
      ?(args : style_args = style_args ~name:"Transitions" ())
      (x : Transitions.t)
  : string
  =
  let args : style_args =
    { args with style = Some (collection_style List); newline = true }
  in
  Transitions.to_list x |> Utils.Strfy.list ~args Transition.to_string
;;

(***********************************************************************)
(*** Actions ***********************************************************)
(***********************************************************************)

module Actions = Hashtbl.Make (struct
    include Action
  end)

let actions_to_string
      ?(args : style_args = style_args ())
      (x : States.t Actions.t)
  : string
  =
  let args : style_args =
    { args with name = Some "Actions"; style = Some (collection_style List) }
  in
  let open Utils.Strfy in
  let xs =
    let args : style_args =
      nest { args with name = None; style = Some (collection_style Record) }
    in
    Actions.fold
      (fun (action : Action.t) (destinations : States.t) (acc : string list) ->
        record
          ~args
          [ "action", Action.to_string ~args action
          ; "destinations", destinations_to_string ~args destinations
          ]
        :: acc)
      x
      []
  in
  list ~args string xs
;;

let action_labels_to_string
      ?(args : style_args = style_args ())
      (x : States.t Actions.t)
  : string
  =
  let args : style_args =
    { args with name = Some "Actions"; style = Some (collection_style List) }
  in
  let open Utils.Strfy in
  let xs =
    let args : style_args =
      nest { args with name = None; style = Some (collection_style Record) }
    in
    Actions.fold
      (fun (action : Action.t) (destinations : States.t) (acc : string list) ->
        record
          ~args
          [ "action.label", Label.to_string ~args action.label
          ; "destinations", destinations_to_string ~args destinations
          ]
        :: acc)
      x
      []
  in
  list ~args string xs
;;

exception Model_Action_HasNoAnnotations of Action.t

(* let get_annotation (from : State.t) (x : Action.t) : Note.annotation =
   if List.is_empty x.annotation
   then raise (Model_Action_HasNoAnnotations x)
   else
   List.map
   (fun (y : Note.annotation) -> Note.add_note (Note.create from x.label) y)
   x.annotation
   ;; *)

(* let get_annotation_from (from : State.t) (x : Action.t)
  : Note.annotation
  =
  Log.trace "Model.get_shortest_annotation_from";
  try
    { this = Note.create from x.label; next = x.annotation }
    match x.annotation with
    | None -> { this = Note.create from x.label; next = None }
    | h :: tl ->
      List.fold_left
        (fun (the_min : Note.annotation) (y : Note.annotation) ->
          let f = Note.annotation_depth in
          match Int.compare (f y) (f the_min) with -1 -> y | _ -> the_min)
        h
        tl
  with
  | Model_Action_HasNoAnnotations x ->
    { this = Note.create from x.label; next = None }
;; *)

(* exception Model_Actions_IsEmpty of States.t Actions.t

   let get_action_with_shortest_annotation (actions : States.t Actions.t)
   : Action.t
   =
   match Actions.to_seq_keys actions |> List.of_seq with
   | [] -> raise (Model_Actions_IsEmpty actions)
   | h :: [] -> h
   | h :: tl ->
   List.fold_left
   (fun (acc : Action.t) (x : Action.t) ->
   let f (z : Action.t) : int =
   get_shortest_annotation z |> Note.annotation_depth
   in
   match Int.compare (f x) (f acc) with -1 -> x | _ -> acc)
   h
   tl
   ;; *)

(* exception Model_Action_HasNoConstructors of Action.t

   let get_shortest_constructor (x : Action.t) : Tree.node list =
   Log.trace "Model.get_shortest_constructor";
   match x.constructor_trees with
   | [] -> raise (Model_Action_HasNoConstructors x)
   | h :: tl ->
   List.map Tree.minimize tl
   |> List.fold_left
   (fun the_min y ->
   match Int.compare (List.length y) (List.length the_min) with
   | -1 -> y
   | _ -> the_min)
   (Tree.minimize h)
   ;; *)

exception Model_Action_HasSilentLabel_ButIsSaturated of Action.t

let is_action_annotated (x : Action.t) : bool =
  log_trace __FUNCTION__;
  Option.has_some x.annotation
;;

(** [is_action_silent x] is an alias for [Label.is_silent x.label].
    @return [true] if [x.is_silent] is [Some true], else [false].
    @raise Model_Action_HasSilentLabel_ButIsSaturated
      if [Label.is_silent x] is [true] but [x.annotation] is non-empty (i.e., it has been saturated and is no longer a silent action)
*)
let is_action_silent (x : Action.t) : bool =
  log_trace __FUNCTION__;
  if Label.is_silent x.label
  then
    if is_action_annotated x
    then raise (Model_Action_HasSilentLabel_ButIsSaturated x)
    else true
  else false
;;

exception Model_NoActionLabelled of (bool * Label.t * States.t Actions.t)

(** [get_action_labelled x actions] returns an action in [actions] that has a label matching [x]. (follows [List.find], so throws error if match is not found)
*)
let get_actions_labelled
      ?(annotated : bool = false)
      (x : Label.t)
      (actions : States.t Actions.t)
  : Action.t list
  =
  log_trace __FUNCTION__;
  try
    Actions.to_seq_keys actions
    |> List.of_seq
    |> List.filter (fun (y : Action.t) ->
      Label.equal y.label x
      ||
      if annotated
      then Option.cata (Note.exists_label x) false y.annotation
      else false)
  with
  | Not_found ->
    Log.debug
      (Printf.sprintf
         "Model.get_action_labelled, Model_NoActionLabelled:\n\
          - annotated: %b\n\
          - x: %s\n\
          - actions: %s"
         annotated
         (Label.to_string x)
         (actions_to_string actions));
    raise (Model_NoActionLabelled (annotated, x, actions))
;;

let get_action_destinations (actions : States.t Actions.t) : States.t =
  Actions.to_seq_values actions
  |> List.of_seq
  |> List.fold_left
       (fun acc destinations -> States.union acc destinations)
       States.empty
;;

let get_reachable_partition (pi : Partition.t) (actions : States.t Actions.t)
  : Partition.t
  =
  let destinations : States.t = get_action_destinations actions in
  pi
  |> Partition.filter (fun (block : States.t) ->
    Bool.not (States.is_empty (States.inter block destinations)))
;;

let get_reachable_partition_opt
      (pi : Partition.t)
      (actions : States.t Actions.t)
  : Partition.t option
  =
  let pi : Partition.t = get_reachable_partition pi actions in
  if Partition.is_empty pi then None else Some pi
;;

(** [update_destinations actions updated_action destinations] maps [updated_action] to [destinations] in [actions], replacing an existing mapping to some set of [old_destinations] with the union of [destinations] and [old_destinations].
*)
let update_destinations
      (actions : States.t Actions.t)
      (updated_action : Action.t)
      (destinations : States.t)
  : States.t
  =
  match Actions.find_opt actions updated_action with
  | None -> destinations
  | Some old_destinations -> States.union destinations old_destinations
;;

(** [update_action actions updated_action destinations] maps [updated_action] to [destinations] in [actions] only if [destinations] is non-empty, via [update_destinations].
*)
let update_action
      (actions : States.t Actions.t)
      (updated_action : Action.t)
      (destinations : States.t)
  : unit
  =
  if Bool.not (States.is_empty destinations)
  then
    update_destinations actions updated_action destinations
    |> Actions.replace actions updated_action
;;

let alphabet_of_actions (actions : States.t Actions.t) : Alphabet.t =
  Actions.to_seq_keys actions
  |> List.of_seq
  |> List.map (fun (x : Action.t) : Label.t -> x.label)
  |> Alphabet.of_list
;;

(***********************************************************************)
(*** Edges *************************************************************)
(***********************************************************************)

module Edges = Hashtbl.Make (struct
    include State
  end)

let update_edge
      (edges : States.t Actions.t Edges.t)
      (from : State.t)
      (action : Action.t)
      (destinations : States.t)
  : unit
  =
  match Edges.find_opt edges from with
  | None ->
    [ action, destinations ]
    |> List.to_seq
    |> Actions.of_seq
    |> Edges.add edges from
  | Some actions -> update_action actions action destinations
;;

let add_edge (edges : States.t Actions.t Edges.t) (edge : Edge.t) : unit =
  States.singleton edge.goto |> update_edge edges edge.from edge.action
;;

let add_edges (edges : States.t Actions.t Edges.t) (to_add : Edge.t list) : unit
  =
  List.iter
    (fun (edge : Edge.t) ->
      States.singleton edge.goto |> update_edge edges edge.from edge.action)
    to_add
;;

exception
  Model_NoActionLabelledFrom of
    (bool * State.t * Label.t * States.t Actions.t Edges.t)

let get_actions_labelled_from
      ?(annotated : bool = false)
      (from : State.t)
      (x : Label.t)
      (edges : States.t Actions.t Edges.t)
  : Action.t list
  =
  log_trace __FUNCTION__;
  let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
  Debug.thing (prefix "x") x (A Label.to_string);
  let actions : States.t Actions.t = Edges.find edges from in
  try get_actions_labelled ~annotated x actions with
  | Not_found -> raise (Model_NoActionLabelledFrom (annotated, from, x, edges))
  | Model_NoActionLabelled _ ->
    raise (Model_NoActionLabelledFrom (annotated, from, x, edges))
;;

(** [get_edges_labelled x edges] returns a copy of [edges] that has been [Edges.filter-map-inplace] so that only actions with a label matching [x] remain.
*)
let get_edges_labelled (x : Label.t) (edges : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  let edges' : States.t Actions.t Edges.t = Edges.copy edges in
  Edges.filter_map_inplace
    (fun (_ : State.t) (actions : States.t Actions.t) ->
      let actions' : States.t Actions.t = Actions.copy actions in
      Actions.filter_map_inplace
        (fun (action : Action.t) (destinations : States.t) ->
          if Label.equal x action.label then Some destinations else None)
        actions';
      if Actions.length actions' > 0 then Some actions' else None)
    edges';
  edges'
;;

let get_reachable_blocks_opt
      (pi : Partition.t)
      (edges : States.t Actions.t Edges.t)
      (x : State.t)
  =
  match Edges.find_opt edges x with
  | None -> None
  | Some blocks -> get_reachable_partition_opt pi blocks
;;

(***********************************************************************)
(*** Transition_opt -> Transition **************************************)
(***********************************************************************)

exception Model_TransitionOptGotoNone of Transition_opt.t

let transition_opt_to_transition : Transition_opt.t -> Transition.t = function
  | { from; label; goto = Some goto; annotation; constructor_trees } ->
    Transition.create from label goto ~annotation ~constructor_trees ()
  | x -> raise (Model_TransitionOptGotoNone x)
;;

(***********************************************************************)
(*** Transition -> Action **********************************************)
(***********************************************************************)

let transition_to_action : Transition.t -> Action.t = function
  | { from; label; goto; annotation; constructor_trees } ->
    Action.create label ~annotation ~constructor_trees ()
;;

let transition_opt_to_action : Transition_opt.t -> Action.t = function
  | { from; label; goto; annotation; constructor_trees } ->
    Action.create label ~annotation ~constructor_trees ()
;;

(***********************************************************************)
(*** Transition -> (From, Action, Goto) ********************************)
(***********************************************************************)

let transition_to_edge : Transition.t -> Edge.t = function
  | { from; label; goto; annotation; constructor_trees } ->
    Edge.create
      from
      (Action.create label ~annotation ~constructor_trees ())
      goto
;;

(***********************************************************************)
(*** (From, Action, Goto) -> Transition ********************************)
(***********************************************************************)

let edge_to_transition : Edge.t -> Transition.t = function
  | { from; action = { label; annotation; constructor_trees }; goto } ->
    Transition.create from label goto ~annotation ~constructor_trees ()
;;

(***********************************************************************)
(*** Edges/Actions -> Transitions **************************************)
(***********************************************************************)

let action_destinations_to_transitions
      (from : State.t)
      ({ label; annotation; constructor_trees } : Action.t)
  : States.t -> Transitions.t -> Transitions.t
  =
  States.fold (fun (goto : State.t) (acc : Transitions.t) ->
    Transitions.add
      (Transition.create from label goto ~annotation ~constructor_trees ())
      acc)
;;

let actions_to_transitions (from : State.t)
  : States.t Actions.t -> Transitions.t -> Transitions.t
  =
  Actions.fold
    (fun (action : Action.t) (destinations : States.t) (acc : Transitions.t) ->
    action_destinations_to_transitions from action destinations acc)
;;

let edges_to_transitions (edges : States.t Actions.t Edges.t) : Transitions.t =
  Edges.fold
    (fun (from : State.t)
      (actions : States.t Actions.t)
      (acc : Transitions.t) -> actions_to_transitions from actions acc)
    edges
    Transitions.empty
;;

(***********************************************************************)
(*** Transitions -> Edges/Actions **************************************)
(***********************************************************************)

let transitions_to_edges (transitions : Transitions.t)
  : States.t Actions.t Edges.t
  =
  let edges : States.t Actions.t Edges.t = Edges.create 0 in
  Transitions.iter
    (fun ({ from; label; goto; constructor_trees; annotation } : Transition.t) ->
      update_edge
        edges
        from
        (Action.create label ~annotation ~constructor_trees ())
        (States.singleton goto))
    transitions;
  edges
;;

(***********************************************************************)
(*** Edges (Continued.) ************************************************)
(***********************************************************************)

let edges_to_string
      ?(args : style_args = style_args ())
      (x : States.t Actions.t Edges.t)
  : string
  =
  let args : style_args =
    nest { args with name = Some "Edges"; style = Some (collection_style List) }
  in
  edges_to_transitions x |> transitions_to_string ~args
;;

(***********************************************************************)
(*** Merge *************************************************************)
(***********************************************************************)
module Merge = struct
  let info_field (x : 'a list option) (y : 'a list option) : 'a list option =
    match x, y with
    | None, None -> None
    | None, Some yy -> Some yy
    | Some xx, None -> Some xx
    | Some xx, Some yy -> Some (List.append xx yy)
  ;;

  let info (x : Info.t) (y : Info.t) : Info.t =
    { mebi_info = info_field x.mebi_info y.mebi_info
    ; rocq_info = info_field x.rocq_info y.rocq_info
    ; weak_info = info_field x.weak_info y.weak_info
    }
  ;;

  exception Model_Merge_action_DifferentLabels of (Action.t * Action.t)

  let constructor_trees (xs : Tree.t list) (ys : Tree.t list) : Tree.t list =
    List.fold_left
      (fun (acc : Tree.t list) (y : Tree.t) ->
        if List.mem y acc then acc else y :: acc)
      xs
      ys
  ;;

  (** [Merge.action] *)
  let action (x : Action.t) (y : Action.t) : Action.t =
    let a : bool = Label.equal x.label y.label in
    let constructor_trees : Tree.t list =
      constructor_trees x.constructor_trees y.constructor_trees
    in
    if a
    then Action.create x.label ~constructor_trees ()
    else raise (Model_Merge_action_DifferentLabels (x, y))
  ;;

  let actions (x : States.t Actions.t) (y : States.t Actions.t)
    : States.t Actions.t
    =
    let actions : States.t Actions.t = Actions.create 0 in
    let f z = Actions.to_seq_keys z |> List.of_seq in
    let xactions : Action.t list = f x in
    let yactions : Action.t list = f y in
    let g label : Action.t list -> Action.t option =
      List.find_opt (fun (action : Action.t) -> Label.equal label action.label)
    in
    List.merge Action.compare xactions yactions
    |> List.map (fun (x : Action.t) : Label.t -> x.label)
    |> Alphabet.of_list
    |> Alphabet.iter (fun (label : Label.t) ->
      match g label xactions, g label yactions with
      | None, None -> () (* NOTE: impossible *)
      | None, Some yaction ->
        Actions.add actions yaction (Actions.find y yaction)
      | Some xaction, None ->
        Actions.add actions xaction (Actions.find x xaction)
      | Some xaction, Some yaction ->
        let merged_action : Action.t = action xaction yaction in
        States.union (Actions.find x xaction) (Actions.find y yaction)
        |> Actions.add actions merged_action);
    actions
  ;;

  let edges (x : States.t Actions.t Edges.t) (y : States.t Actions.t Edges.t)
    : States.t Actions.t Edges.t
    =
    let edges : States.t Actions.t Edges.t = Edges.create 0 in
    let f z = Edges.to_seq_keys z |> States.of_seq in
    States.union (f x) (f y)
    |> States.iter (fun (from : State.t) ->
      match Edges.find_opt x from, Edges.find_opt y from with
      | None, None -> () (* NOTE: impossible *)
      | None, Some yactions -> Edges.add edges from yactions
      | Some xactions, None -> Edges.add edges from xactions
      | Some xactions, Some yactions ->
        actions xactions yactions |> Edges.add edges from);
    edges
  ;;
end

(***********************************************************************)
(*** Kind (Lts, Fsm) ***************************************************)
(***********************************************************************)

type kind =
  | LTS of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * Transitions.t
      * Info.t)
  | FSM of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * States.t Actions.t Edges.t
      * Info.t)

(***********************************************************************)
(*** Lts ***************************************************************)
(***********************************************************************)

module Lts = struct
  type t =
    { init : State.t option
    ; terminals : States.t
    ; alphabet : Alphabet.t
    ; states : States.t
    ; transitions : Transitions.t
    ; info : Info.t
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
      let transitions = edges_to_transitions edges in
      { init; terminals; alphabet; states; transitions; info }
  ;;

  open Utils.Strfy

  let to_string ?(args : style_args = style_args ()) : t -> string =
    let module Strfy = Utils.Strfy in
    function
    | { init; terminals; alphabet; states; transitions; info } ->
      let init : string = Strfy.option ~args:(nest args) State.to_string init in
      let terminals : string = states_to_string ~args:(nest args) terminals in
      let alphabet : string = alphabet_to_string ~args:(nest args) alphabet in
      let states : string = states_to_string ~args:(nest args) states in
      let transitions : string =
        transitions_to_string ~args:(nest args) transitions
      in
      let info : string = Info.to_string ~args:(nest args) info in
      Strfy.record
        ~args:
          { args with
            name = Some "Lts"
          ; style = Some (collection_style Record)
          }
        [ "init", init
        ; "info", info
        ; "alphabet", alphabet
        ; "terminals", terminals
        ; "states", states
        ; "transitions", transitions
        ]
  ;;
end

(***********************************************************************)
(*** Fsm ***************************************************************)
(***********************************************************************)

module Fsm = struct
  type t =
    { init : State.t option
    ; terminals : States.t
    ; alphabet : Alphabet.t
    ; states : States.t
    ; edges : States.t Actions.t Edges.t
    ; info : Info.t
    }

  and pair = t * t

  let to_model ({ init; terminals; alphabet; states; edges; info } : t) : kind =
    FSM (init, terminals, alphabet, states, edges, info)
  ;;

  let of_model : kind -> t = function
    | LTS (init, terminals, alphabet, states, transitions, info) ->
      let edges = transitions_to_edges transitions in
      { init; terminals; alphabet; states; edges; info }
    | FSM (init, terminals, alphabet, states, edges, info) ->
      { init; terminals; alphabet; states; edges; info }
  ;;

  let clone (x : t) : t = { x with edges = Edges.copy x.edges }

  let merge (a : t) (b : t) : t =
    let init : State.t option = None in
    let terminals : States.t = States.union a.terminals b.terminals in
    let alphabet : Alphabet.t = Alphabet.union a.alphabet b.alphabet in
    let states : States.t = States.union a.states b.states in
    let edges : States.t Actions.t Edges.t = Merge.edges a.edges b.edges in
    let info : Info.t = Merge.info a.info b.info in
    { init; terminals; alphabet; states; edges; info }
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

  open Utils.Strfy

  let to_string ?(args : style_args = style_args ()) : t -> string =
    let module Strfy = Utils.Strfy in
    function
    | { init; terminals; alphabet; states; edges; info } ->
      let init : string = Strfy.option ~args:(nest args) State.to_string init in
      let terminals : string = states_to_string ~args:(nest args) terminals in
      let alphabet : string = alphabet_to_string ~args:(nest args) alphabet in
      let states : string = states_to_string ~args:(nest args) states in
      let edges : string = edges_to_string ~args:(nest args) edges in
      let info : string = Info.to_string ~args:(nest args) info in
      Strfy.record
        ~args:
          { args with
            name = Some "Fsm"
          ; style = Some (collection_style Record)
          }
        [ "init", init
        ; "info", info
        ; "alphabet", alphabet
        ; "terminals", terminals
        ; "states", states
        ; "edges", edges
        ]
  ;;
end

(***********************************************************************)
(*** Saturate **********************************************************)
(***********************************************************************)

module Saturate = struct
  (** [merge_saturated_tuples a b] merges elements of [b] into [a], either by updating an element in [a] with additional annotation for a saturation tuple that describes the same action-destination, or in the case that the saturation tuple is not described within [a] by inserting it within [a].
  *)
  let rec merge_saturated_tuples (a : (Action.t * States.t) list)
    : (Action.t * States.t) list -> (Action.t * States.t) list
    =
    log_trace __FUNCTION__;
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
    log_trace __FUNCTION__;
    let f
      :  Note.annotation option * Note.annotation option
      -> Note.annotation option
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

  let log_wips (xs : wip list) : unit =
    Debug.thing
      "wips"
      (List.rev xs)
      (A
         (Utils.Strfy.list
            (fun
                ?(args : style_args = style_args ())
                 ({ from; via; tree } : wip)
               ->
            Printf.sprintf
              "<From (%s) Via (%s)>"
              (Mebi_setup.Enc.to_string from.enc)
              (Mebi_setup.Enc.to_string via.enc))))
  ;;

  let log_notes (d : data) : unit =
    Debug.thing
      "notes (wip list)"
      (List.rev d.notes)
      (A
         (Utils.Strfy.list
            (fun
                ?(args : style_args = style_args ())
                 ({ from; via; tree } : wip)
               ->
            Printf.sprintf
              "<From (%s) Via (%s)>"
              (Mebi_setup.Enc.to_string from.enc)
              (Mebi_setup.Enc.to_string via.enc))))
  ;;

  let log_data ({ named; notes; visited; old_edges } : data) : unit =
    let e (x : Label.t) : string = Enc.to_string x.enc in
    let f (x : Action.t) : string = e x.label in
    (* let g (x : State.t) : string = Enc.to_string x.enc in *)
    let named : string = Option.cata f "None" named in
    Log.debug (Printf.sprintf "data, named: %s" named);
    (* List.iter
       (fun (n : wip) ->
       let from : string = g n.from in
       let via : string = e n.via in
       let tree : string = Tree.list_to_string n.tree in
       Log.debug (Printf.sprintf "data, note: (%s, %s); %s" from via tree))
       notes; *)
    log_wips notes;
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
      log_trace __FUNCTION__;
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
      log_wips xs;
      raise (Model_Saturate_WIP_HadNoNamedActions xs)
    | _ :: [] -> ()
    | _ :: _ ->
      log_wips xs;
      raise (Model_Saturate_WIP_HadMultipleNamedActions xs)
  ;;

  (* let new_actionpair_opt : data -> actionpair option =
    function | {named={label;annotation;}} *)

  (** [stop] *)
  let stop (d : data) (goto : State.t) (acc : actionpairs) : actionpairs =
    Log.debug "\n.\n";
    log_trace __FUNCTION__;
    (* let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in *)
    match d.named with
    | None -> acc
    | Some named ->
      validate_wips d.notes;
      log_notes d;
      let annotation = Some (wip_to_annotation goto d.notes) in
      Debug.thing
        "stop.annotation"
        annotation
        (B (Option.cata Note.annotation_to_string "None"));
      let x : Action.t =
        Action.create named.label ~annotation ()
        (* let x =
        { named with
          annotation = Note.add_annotation annotation named.annotation
        ; constructor_trees = []
        } *)
      in
      (x, States.singleton goto) :: acc
  ;;

  (** [check_from] explores the outgoing actions of state [from], which is some destination of another action.
  *)
  let rec check_from (d : data) (from : State.t) (acc : actionpairs)
    : actionpairs
    =
    log_trace __FUNCTION__;
    let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
    Debug.thing (prefix "from") from.enc (B Enc.to_string);
    if already_visited from d
    then (
      Debug.thing (prefix "already visited") from.enc (B Enc.to_string);
      stop d from acc)
    else (
      let d : data = _update_visited' from d in
      log_data d;
      match get_old_actions from d with
      | None ->
        Debug.thing (prefix "no actions from") from.enc (B Enc.to_string);
        stop d from acc
      | Some old_actions ->
        Debug.thing (prefix "checking actions from") from.enc (B Enc.to_string);
        check_actions d from old_actions acc)

  and check_actions (d : data) (from : State.t) (xs : States.t Actions.t)
    : actionpairs -> actionpairs
    =
    log_trace __FUNCTION__;
    let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
    Debug.thing (prefix "from") from.enc (B Enc.to_string);
    Actions.iter
      (fun k _ -> Debug.thing (prefix "action") k.label.enc (B Enc.to_string))
      xs;
    Actions.fold
      (fun (x : Action.t) (ys : States.t) (acc : actionpairs) ->
        if skip_action x d
        then (
          Debug.thing (prefix "skipping") x.label.enc (B Enc.to_string);
          stop d from acc)
        else (
          Debug.thing (prefix "continuing") x.label.enc (B Enc.to_string);
          States.iter
            (fun x -> Debug.thing (prefix "dest") x.enc (B Enc.to_string))
            ys;
          (* TODO: should be able to go from 10 - 16 too @ line 50 proc/text1/terms.v *)
          (* let destinations : States.t = States.diff ys d.visited in *)
          let d : data (* NOTE: copy [d] *) = _update_notes' from x d in
          let d : data = _update_named' x d in
          check_destinations d from ys acc))
      xs

  and check_destinations (d : data) (from : State.t) (xs : States.t)
    : actionpairs -> actionpairs
    =
    log_trace __FUNCTION__;
    let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
    States.iter
      (fun x -> Debug.thing (prefix "dest") x.enc (B Enc.to_string))
      xs;
    States.fold
      (fun (x : State.t) (acc : actionpairs) ->
        Log.debug "\n-\n";
        Debug.thing (prefix "from") from.enc (B Enc.to_string);
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
    Log.debug "\n---\n";
    log_trace __FUNCTION__;
    let prefix : string -> string = Printf.sprintf "%s %s" __FUNCTION__ in
    Debug.thing (prefix "from") from (A State.to_string);
    log_data d;
    States.fold
      (fun (y : State.t) (acc : actionpairs) -> check_from d y [])
      ys
      []
  ;;

  (** [edge_actions] returns a list of saturated actions tupled with their respective destinations, obtained from [edge_action_destinations] which explores the reflexive-transitive closure
      edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
  let edge_actions
        (from : State.t)
        (old_actions : States.t Actions.t)
        (old_edges : States.t Actions.t Edges.t)
    : actionpairs
    =
    log_trace __FUNCTION__;
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
    log_trace __FUNCTION__;
    edge_actions from old_actions old_edges
    |> List.iter
         (fun ((saturated_action, destinations) : Action.t * States.t) ->
         update_action new_actions saturated_action destinations)
  ;;

  (** [] *)
  let edges
        (labels : Alphabet.t)
        (states : States.t)
        (old_edges : States.t Actions.t Edges.t)
    : States.t Actions.t Edges.t
    =
    log_trace __FUNCTION__;
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
    log_trace __FUNCTION__;
    { x with edges = edges x.alphabet x.states (Edges.copy x.edges) }
  ;;
end
