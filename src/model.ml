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

(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

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
  States.to_list x |> Utils.Strfy.list ~args (Args State.to_string)
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
  Partition.to_list x |> Utils.Strfy.list ~args (Args states_to_string)
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
  Alphabet.to_list x |> Utils.Strfy.list ~args (Args Label.to_string)
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

let decode_label_opt (x : Enc.t) (labels : Alphabet.t) : Label.t option =
  Log.trace __FUNCTION__;
  try Some (find_label_of_enc x labels) with
  | Model_Alphabet_LabelOfEncNotFound (x, alphabet) -> None
;;

exception Model_CannotDecodeLabel of (Enc.t * Alphabet.t)

let decode_label (x : Enc.t) (labels : Alphabet.t) : Label.t =
  Log.trace __FUNCTION__;
  match decode_label_opt x labels with
  | None -> raise (Model_CannotDecodeLabel (x, labels))
  | Some label -> label
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
  Transitions.to_list x |> Utils.Strfy.list ~args (Args Transition.to_string)
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
  list ~args (Args string) xs
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
  list ~args (Args string) xs
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

   let get_shortest_constructor (x : Action.t) : Tree.Node.t list =
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
  Log.trace __FUNCTION__;
  Option.has_some x.annotation
;;

(** [is_action_silent x] is an alias for [Label.is_silent x.label].
    @return [true] if [x.is_silent] is [Some true], else [false].
    @raise Model_Action_HasSilentLabel_ButIsSaturated
      if [Label.is_silent x] is [true] but [x.annotation] is non-empty (i.e., it has been saturated and is no longer a silent action)
*)
let is_action_silent (x : Action.t) : bool =
  Log.trace __FUNCTION__;
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
  Log.trace __FUNCTION__;
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
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "x" x (Args Label.to_string);
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

exception Model_CannotConvertTransitionOpt_To_Transition of Transition_opt.t

let transition_opt_to_transition : Transition_opt.t -> Transition.t = function
  | { from
    ; label = Some label
    ; goto = Some goto
    ; annotation
    ; constructor_trees
    } ->
    Transition.create from label goto ~annotation ~constructor_trees ()
  | x -> raise (Model_CannotConvertTransitionOpt_To_Transition x)
;;

(***********************************************************************)
(*** Transition -> Action **********************************************)
(***********************************************************************)

let transition_to_action : Transition.t -> Action.t = function
  | { from; label; goto; annotation; constructor_trees } ->
    Action.create label ~annotation ~constructor_trees ()
;;

exception Model_CannotConvertTransitionOpt_To_Action of Transition_opt.t

let transition_opt_to_action : Transition_opt.t -> Action.t = function
  | { from; label = Some label; goto; annotation; constructor_trees } ->
    Action.create label ~annotation ~constructor_trees ()
  | x -> raise (Model_CannotConvertTransitionOpt_To_Action x)
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
      let init : string =
        Strfy.option ~args:(nest args) (Args State.to_string) init
      in
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
      let init : string =
        Strfy.option ~args:(nest args) (Args State.to_string) init
      in
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
