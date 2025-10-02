open Logging
open Model

type t =
  { init : State.t option
  ; mutable terminals : States.t
  ; mutable alphabet : Alphabet.t
  ; mutable states : States.t
  ; mutable edges : States.t Actions.t Edges.t
  ; info : Info.t option
  }

let to_model (m : t) : Model.t =
  FSM (m.init, m.terminals, m.alphabet, m.states, m.edges, m.info)
;;

let create
      (init : State.t option)
      (terminals : States.t)
      (alphabet : Alphabet.t)
      (states : States.t)
      (edges : States.t Actions.t Edges.t)
      (info : Info.t option)
  : t
  =
  Model.check_info (FSM (init, terminals, alphabet, states, edges, info));
  { init; terminals; alphabet; states; edges; info }
;;

let create_from (m : Model.t) : t =
  match m with
  | LTS (init, terminals, alphabet, states, transitions, info) ->
    let edges = Model.transitions_to_edges transitions in
    create init terminals alphabet states edges info
  | FSM (init, terminals, alphabet, states, edges, info) ->
    create init terminals alphabet states edges info
;;

(* | NestedStrList args ->
   let (init, alphabet, states, edges) = Model.handle_nested_str_list_to_edges  args in
   create init alphabet states edges None *)

let clone (m : t) : t = { m with edges = Edges.copy m.edges }

let update_info (m : t) : t =
  match m.info with
  | None -> m
  | Some i ->
    { m with
      info =
        Some
          { i with
            num_labels = Alphabet.cardinal m.alphabet
          ; num_states = States.cardinal m.states
          ; num_edges = get_num_edges m.edges
          }
    }
;;

(*********************************************************************)
(****** Add **********************************************************)
(*********************************************************************)

let add_action (m : t) (a : Action.t) : t =
  match m with
  | { alphabet; _ } -> { m with alphabet = Alphabet.add a.label alphabet }
;;

let add_action_list (m : t) (aa : Action.t list) : t =
  match m with
  | { alphabet; _ } ->
    { m with
      alphabet =
        Alphabet.add_seq
          (List.to_seq (Model.action_list_to_label_list aa))
          alphabet
    }
;;

let add_state (m : t) (s : State.t) : t =
  match m with { states; _ } -> { m with states = States.add s states }
;;

let add_state_list (m : t) (ss : State.t list) : t =
  match m with
  | { states; _ } -> { m with states = States.add_seq (List.to_seq ss) states }
;;

let add_states (m : t) (ss : States.t) : t =
  match m with { states; _ } -> { m with states = States.union ss states }
;;

let add_edge (m : t) (from : State.t) (a : Action.t) (dest : State.t) : t =
  match m with
  | { states; alphabet; edges; _ } ->
    let m = add_action (add_state_list m [ from; dest ]) a in
    Model.add_edge m.edges from a dest;
    m
;;

let rec add_edge_list (m : t) (es : Edge.t list) : t =
  match es with
  | [] -> m
  | (from, a, dest) :: t -> add_edge_list (add_edge m from a dest) t
;;

let add_edge_from_label
      ?(meta : Action.MetaData.t option = None)
      (m : t)
      (from : State.t)
      (l : Action.Label.t)
      (dest : State.t)
  : t
  =
  let a = label_to_action l in
  add_edge m from a dest
;;

let rec add_edges_from_label_list
          (m : t)
          (aa :
            (State.t * Action.Label.t * State.t * Action.MetaData.t option) list)
  : t
  =
  match aa with
  | [] -> m
  | (from, a, dest, meta) :: t ->
    add_edges_from_label_list (add_edge_from_label ~meta m from a dest) t
;;

(*********************************************************************)
(****** Get **********************************************************)
(*********************************************************************)

let get_actions_from (m : t) (from : State.t) : States.t Actions.t =
  match Edges.find_opt m.edges from with
  | None -> Actions.of_seq (List.to_seq [])
  | Some aa -> aa
;;

let get_states_reachable_from (m : t) (from : State.t) : States.t =
  Actions.fold
    (fun (_ : Action.t) (dests : States.t) (acc : States.t) ->
      States.union dests acc)
    (get_actions_from m from)
    States.empty
;;

(*********************************************************************)
(****** Checks/Boolean Ops *******************************************)
(*********************************************************************)

let has_state (m : t) (s : State.t) : bool = States.mem s m.states

(*********************************************************************)
(****** Prune ********************************************************)
(*********************************************************************)

let _prune_unreachable_edges (m : t) : t =
  match m.init with
  | None -> m
  | Some init ->
    let rec prune_unreachable_edges
              (from : State.t)
              (acc : (State.t * States.t Actions.t) list)
      : (State.t * States.t Actions.t) list
      =
      let reachable_states = get_states_reachable_from m from in
      if
        States.for_all
          (fun (s : State.t) ->
            List.exists
              (fun ((t, _) : State.t * States.t Actions.t) -> State.eq s t)
              acc)
          reachable_states
      then acc
      else
        States.fold
          (fun (s : State.t) (acc1 : (State.t * States.t Actions.t) list) ->
            prune_unreachable_edges s acc1)
          reachable_states
          (if
             List.exists
               (fun ((t, _) : State.t * States.t Actions.t) -> State.eq from t)
               acc
           then acc
           else (from, get_actions_from m from) :: acc)
    in
    { m with
      edges = Edges.of_seq (List.to_seq (prune_unreachable_edges init []))
    }
;;

let _prune_unreachable_states (m : t) : t =
  let pruned_states : States.t =
    Edges.fold
      (fun (from : State.t) (actions : States.t Actions.t) (acc : States.t) ->
        States.union (get_states_reachable_from m from) (States.add from acc))
      m.edges
      States.empty
  in
  { m with states = pruned_states }
;;

(*********************************************************************)
(****** Saturate *****************************************************)
(*********************************************************************)

let max_visit_num : int = 2

let can_revisit (s : State.t) (visited_states : (State.t, int) Hashtbl.t) : bool
  =
  match Hashtbl.find_opt visited_states s with
  | None ->
    Hashtbl.add visited_states s 0;
    true
  | Some n -> n < max_visit_num
;;

let log_visit (s : State.t) (visited_states : (State.t, int) Hashtbl.t) : unit =
  match Hashtbl.find_opt visited_states s with
  | None -> Hashtbl.add visited_states s 1
  | Some n -> Hashtbl.replace visited_states s (n + 1)
;;

exception CannotSaturateActionsWithUnknownVisibility of Action.t

let resolve_saturated_action
      ?(named : Action.t option = None)
      (acc0 : ActionPairs.t)
      (dest : State.t)
      (anno : Action.annotation)
  : ActionPairs.t
  =
  let anno = List.rev anno in
  match named with
  | None -> acc0 (* do nothing *)
  | Some a ->
    (match
       ActionPairs.fold
         (fun ((annotated_action, dests) : ActionPair.t)
           ((acc1a, acc1b) : Action.t list * ActionPairs.t) ->
           if Action.eq ~annos:false ~meta:false a annotated_action
           then annotated_action :: acc1a, acc1b
           else acc1a, ActionPairs.add (annotated_action, dests) acc1b)
         acc0
         ([], ActionPairs.empty)
     with
     | [], acc0 ->
       (* Logging.Log.warning
          (Printf.sprintf
          "resolve_saturated_action, adding new (%s) "
          (Action.to_string a)); *)
       ActionPairs.add ({ a with annos = [ anno ] }, States.singleton dest) acc0
     | b :: [], acc0 ->
       (* Logging.Log.warning
          (Printf.sprintf
          "resolve_saturated_action, merging (%s) with (%s)"
          (Action.to_string a)
          (Action.to_string b)); *)
       ActionPairs.add
         ( { b with
             annos =
               (if List.mem anno b.annos then b.annos else anno :: b.annos)
           }
         , States.singleton dest )
         acc0
     | multi, acc0 ->
       Log.warning
         (Printf.sprintf
            "fsm.resolve_saturated_action, found (%i) action pairs matching \
             named action (%s).\n\
             TEMP: dropping dupes and adding as fresh"
            (List.length multi)
            (Action.to_string a));
       ActionPairs.add ({ a with annos = [ anno ] }, States.singleton dest) acc0)
;;

(** *)
let rec saturate_action_from
          ?(named : Action.t option = None)
          (acc1 : ActionPairs.t)
          (from : State.t)
          (anno : Action.annotation)
          (visited : (State.t, int) Hashtbl.t)
          (unsaturated_es : States.t Actions.t Edges.t)
  : ActionPairs.t
  =
  log_visit from visited;
  if can_revisit from visited
  then (
    Log.debug
      (Printf.sprintf "fsm.saturate_action_from (%s)" (State.to_string from));
    match Edges.find_opt unsaturated_es from with
    | None ->
      resolve_saturated_action ~named acc1 from anno (* terminal state *)
    | Some s_actions ->
      saturate_actions_from
        ~named
        acc1
        from
        s_actions
        anno
        visited
        unsaturated_es)
  else (
    match named with
    | None ->
      Log.debug
        (Printf.sprintf
           "fsm.saturate_action_from (%s) ! abort, already visited and no \
            named action found"
           (State.to_string from));
      acc1
    | Some _ ->
      Log.debug
        (Printf.sprintf
           "fsm.saturate_action_from (%s) ! stop, already visited and named \
            action found"
           (State.to_string from));
      resolve_saturated_action ~named acc1 from anno)

and saturate_dest_actions
      ?(named : Action.t option = None)
      (acc1 : ActionPairs.t)
      (dests : States.t)
      (anno : Action.annotation)
      (visited : (State.t, int) Hashtbl.t)
      (unsaturated_es : States.t Actions.t Edges.t)
  : ActionPairs.t
  =
  Log.debug
    (Printf.sprintf
       "fsm.saturate_dest_actions (%s)"
       (match named with
        | None -> "no named action"
        | Some a -> Action.to_string a));
  States.fold
    (fun (dest : State.t) (acc2 : ActionPairs.t) ->
      saturate_action_from ~named acc2 dest anno visited unsaturated_es)
    dests
    acc1

and saturate_actions_from
      ?(named : Action.t option = None)
      (acc1 : ActionPairs.t)
      (from : State.t)
      (aa : States.t Actions.t)
      (anno : Action.annotation)
      (visited : (State.t, int) Hashtbl.t)
      (unsaturated_es : States.t Actions.t Edges.t)
  : ActionPairs.t
  =
  Log.debug
    (Printf.sprintf "fsm.saturate_actions_from (%s)" (State.to_string from));
  snd
    (Actions.fold
       (fun (a : Action.t)
         (dests : States.t)
         ((added, acc2) : bool * ActionPairs.t) ->
         match Action.Label.is_silent a.label with
         | None -> raise (CannotSaturateActionsWithUnknownVisibility a)
         | Some true ->
           Log.debug
             (Printf.sprintf
                "fsm.saturate_actions_from (%s) silent action (%s)"
                (State.to_string from)
                (Action.to_string a));
           ( added
           , saturate_dest_actions
               ~named
               acc2
               dests
               ((from, a) :: anno)
               visited
               unsaturated_es )
         | Some false ->
           (match named with
            (* found named action, continue *)
            | None ->
              Log.debug
                (Printf.sprintf
                   "fsm.saturate_actions_from (%s) named action (%s)"
                   (State.to_string from)
                   (Action.to_string a));
              ( added
              , saturate_dest_actions
                  ~named:(Some a)
                  acc2
                  dests
                  ((from, a) :: anno)
                  visited
                  unsaturated_es )
            | Some _ ->
              Log.debug
                (Printf.sprintf
                   "fsm.saturate_actions_from (%s) named action (%s) ! abort"
                   (State.to_string from)
                   (Action.to_string a));
              (* some other named action, stop saturating and add *)
              if added
              then added, acc2
              else true, resolve_saturated_action ~named acc2 from anno))
       aa
       (false, acc1))
;;

(** @return
      [ActionPairs.t] containing pairs of saturated actions and destinations, outgoing from state [from].
*)
let saturate_edges_from
      ?(named : Action.t option = None)
      (unsaturated_es : States.t Actions.t Edges.t)
      (from : State.t)
      (aa : States.t Actions.t)
  : ActionPairs.t
  =
  Log.trace "fsm.saturate_edges_from";
  Actions.fold
    (fun (a : Action.t) (dests : States.t) (acc0 : ActionPairs.t) ->
      match Action.Label.is_silent a.label with
      | None -> raise (CannotSaturateActionsWithUnknownVisibility a)
      | Some is_silent ->
        let named = if is_silent then None else Some a in
        let anno : Action.annotation = [ from, a ] in
        States.fold
          (fun (dest : State.t) (acc1 : ActionPairs.t) ->
            let visited : (State.t, int) Hashtbl.t = Hashtbl.create 0 in
            log_visit from visited;
            Log.debug
              (Printf.sprintf
                 "fsm.saturate_edges_from (%s) to (%s)"
                 (State.to_string from)
                 (State.to_string dest));
            Model.merge_action_pairs
              acc1
              (saturate_action_from
                 ~named
                 ActionPairs.empty
                 dest
                 anno
                 visited
                 unsaturated_es))
          dests
          acc0)
    aa
    ActionPairs.empty
;;

(** @return
      [m] with saturated edges. Saturation removes silent actions, replacing them with (potentially multiple) explicit non-silent actions that may otherwise be taken immediately following some sequence of silent actions.
*)
let saturate_edges (m : t) : States.t Actions.t Edges.t =
  Log.trace "fsm.saturate_edges";
  let edges : States.t Actions.t Edges.t = Edges.copy m.edges in
  if Logging.is_details_enabled ()
  then
    Log.debug (Printf.sprintf "fsm.saturate_edges, copy: %s" (pstr_edges edges));
  Edges.iter
    (fun (from : State.t) (aa : States.t Actions.t) ->
      let actions : States.t Actions.t = Actions.create 0 in
      ActionPairs.iter
        (fun ((saturated_action, dests) : ActionPair.t) ->
          if States.cardinal dests > 0
          then Actions.add actions saturated_action dests)
        (saturate_edges_from edges from aa);
      if Actions.length actions > 0 then Edges.add edges from actions)
    m.edges;
  edges
;;

(** @return [m] with saturated edges. *)
let saturate (m : t) : t =
  Log.trace "fsm.saturate";
  let m = clone m in
  update_info { m with edges = saturate_edges m }
;;

(*********************************************************************)
(****** Fsm Pairs ****************************************************)
(*********************************************************************)

type pair = t * t

let saturate_pair (p : pair) : pair = saturate (fst p), saturate (snd p)

exception StateOriginNotFound of (pair * State.t)

(** similar to [compare], returns [-1] if [s] is only in [fst p], [1] if only in [snd p] and [0] if in both of [p]
*)
let state_origin_opt (p : pair) (s : State.t) : int option =
  match has_state (fst p) s, has_state (snd p) s with
  | true, true -> Some 0
  | true, false -> Some (-1)
  | false, true -> Some 1
  | false, false -> None
;;

let state_origin (p : pair) (s : State.t) : int =
  match state_origin_opt p s with
  | None -> raise (StateOriginNotFound (p, s))
  | Some i -> i
;;

(*********************************************************************)
(****** Merge ********************************************************)
(*********************************************************************)

let merge (p : pair) : t =
  let m1, m2 = p in
  let init =
    match m1.init, m2.init with
    | Some i1, Some i2 -> if State.eq i1 i2 then Some i1 else None
    | _, _ -> None
  in
  let terminals = States.union m1.terminals m2.terminals in
  let alphabet = Alphabet.union m1.alphabet m2.alphabet in
  let states = States.union m1.states m2.states in
  let edges = Model.merge_edges m1.edges m2.edges in
  let info : Model.Info.t option =
    match m1.info, m2.info with
    | Some i1, Some i2 ->
      Some
        (Model.Info.merge
           ~num_terminals:(States.cardinal terminals)
           ~num_labels:(Alphabet.cardinal alphabet)
           ~num_states:(States.cardinal states)
           ~num_edges:(Model.get_num_edges edges)
           i1
           i2)
    | _, _ -> None
  in
  { init; terminals; alphabet; states; edges; info }
;;

(*********************************************************************)
(****** Pretty-Strings ***********************************************)
(*********************************************************************)
let to_string
      ?(pstr : bool = false)
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (m : t)
  : string
  =
  let str_indent0 = Utils.str_tabs indents in
  let str_indent1 = Utils.str_tabs (indents + 1) in
  let outer, sep =
    if pstr
    then str_indent0, Printf.sprintf "\n%s%s; " str_indent0 str_indent1
    else "", ""
  in
  let num_alpha_str = Printf.sprintf "(%i) " (Alphabet.cardinal m.alphabet) in
  let num_terms_str = Printf.sprintf "(%i) " (States.cardinal m.terminals) in
  let num_states_str = Printf.sprintf "(%i) " (States.cardinal m.states) in
  let num_edges_str = Printf.sprintf "(%i) " (get_num_edges m.edges) in
  Printf.sprintf
    "\n\
     %s%s{ initial state: %s\n\
     %sinfo: %s\n\
     %sterminals: %s%s\n\
     %salphabet: %s%s\n\
     %sstates: %s%s\n\
     %sedges: %s%s\n\
     %s}"
    (if skip_leading_tab then "" else str_indent0)
    outer
    (pstr_state_opt ~indents:(indents + 1) m.init)
    sep
    (pstr_info_opt ~indents:(indents + 1) m.info)
    sep
    num_terms_str
    (pstr_states ~skip_leading_tab:true ~indents:(indents + 1) m.terminals)
    sep
    num_alpha_str
    (pstr_alphabet ~skip_leading_tab:true ~indents:(indents + 1) m.alphabet)
    sep
    num_states_str
    (pstr_states ~skip_leading_tab:true ~indents:(indents + 1) m.states)
    sep
    num_edges_str
    (pstr_edges ~skip_leading_tab:true ~indents:(indents + 1) m.edges)
    outer
;;

let pstr ?(skip_leading_tab : bool = false) ?(indents : int = 0) (m : t)
  : string
  =
  to_string ~pstr:true ~skip_leading_tab ~indents m
;;
