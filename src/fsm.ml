open Model

type t =
  { init : State.t option
  ; mutable alphabet : Alphabet.t
  ; mutable states : States.t
  ; mutable edges : States.t Actions.t Edges.t
  ; info : Info.t option
  }

let to_model (m : t) : Model.t =
  FSM (m.init, m.alphabet, m.states, m.edges, m.info)
;;

let create
      (init : State.t option)
      (alphabet : Alphabet.t)
      (states : States.t)
      (edges : States.t Actions.t Edges.t)
      (info : Info.t option)
  : t
  =
  Model.check_info (FSM (init, alphabet, states, edges, info));
  { init; alphabet; states; edges; info }
;;

let create_from (m : Model.t) : t =
  match m with
  | LTS (init, alphabet, states, transitions, info) ->
    let edges = Model.transitions_to_edges transitions in
    create init alphabet states edges info
  | FSM (init, alphabet, states, edges, info) ->
    create init alphabet states edges info
;;

(* | NestedStrList args ->
   let (init, alphabet, states, edges) = Model.handle_nested_str_list_to_edges  args in
   create init alphabet states edges None *)

let clone (m : t) : t =
  match m with
  | { init; alphabet; states; edges = to_copy; info } ->
    { init; alphabet; states; edges = Edges.copy to_copy; info }
;;

let update_info (m : t) : t =
  match m with
  | { init; alphabet; states; edges; info = to_update } ->
    (match to_update with
     | None -> m
     | Some i ->
       { m with
         info =
           Some
             { i with
               num_labels = Alphabet.cardinal alphabet
             ; num_states = States.cardinal states
             ; num_edges = get_num_edges edges
             }
       })
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

(*********************************************************************)
(****** Checks/Boolean Ops *******************************************)
(*********************************************************************)

let has_state (m : t) (s : State.t) : bool = States.mem s m.states

(*********************************************************************)
(****** Saturate *****************************************************)
(*********************************************************************)

let max_revisit_num : int = 2

let can_revisit (s : State.t) (visited_states : (State.t, int) Hashtbl.t) : bool
  =
  match Hashtbl.find_opt visited_states s with
  | None ->
    Hashtbl.add visited_states s 0;
    true
  | Some n -> n < max_revisit_num
;;

let log_visit (s : State.t) (visited_states : (State.t, int) Hashtbl.t) : unit =
  match Hashtbl.find_opt visited_states s with
  | None -> Hashtbl.add visited_states s 1
  | Some n -> Hashtbl.replace visited_states s (n + 1)
;;

let annotate_action
      (p : Action.annotation_pair)
      (action_to_annotate : Action.t)
      (anno : Action.annotation)
  : Action.t
  =
  Action.saturated ?anno:(Some (p :: anno)) action_to_annotate
;;

let opt_silent_action (o : Action.t option) (a : Action.t) : Action.t option =
  match o with None -> Some a | Some _ -> o
;;

exception CannotSaturateActionsWithUnknownVisibility of Action.t

let rec get_annotated_actions
          (m : t)
          (visited_states : (State.t, int) Hashtbl.t)
          (anno : Action.annotation)
          (to_visit : States.t)
          (named_action : Action.t option)
          (silent_action : Action.t option)
          (acc0 : action_pair list)
  : action_pair list
  =
  States.fold
    (fun (dest : State.t) (acc1 : action_pair list) ->
      match Edges.find_opt m.edges dest with
      | None -> acc1 (* terminal state *)
      | Some dest_actions ->
        if can_revisit dest visited_states
        then (
          log_visit dest visited_states;
          Actions.fold
            (fun (a : Action.t) (dests : States.t) (acc2 : action_pair list) ->
              match Action.Label.is_silent a.label, named_action with
              | None, _ -> raise (CannotSaturateActionsWithUnknownVisibility a)
              (* stop if [a] is not silent and already have [named_action] *)
              | Some false, Some _ -> acc2
              (* if [a] is silent and have not yet found [named_action] *)
              | Some true, None ->
                (* let silent_action = opt_silent_action silent_action a in *)
                get_annotated_actions
                  m
                  visited_states
                  ((dest, a) :: anno)
                  dests
                  named_action
                  (opt_silent_action silent_action a)
                  acc2
              (* if [a] is silent and have already found [named_action] *)
              | Some true, Some named_action' ->
                get_annotated_actions
                  m
                  visited_states
                  ((dest, a) :: anno)
                  dests
                  named_action
                  (opt_silent_action silent_action a)
                  ((annotate_action (dest, a) named_action' anno, dests) :: acc2)
              (* if [a] is not silent and have not yet found [named_action] *)
              | Some false, None ->
                get_annotated_actions
                  m
                  visited_states
                  ((dest, a) :: anno)
                  dests
                  (Some a)
                  silent_action
                  ((annotate_action (dest, a) a anno, dests) :: acc2))
            dest_actions
            acc1)
        else acc1)
    to_visit
    acc0
;;

let saturate_actions (m : t) (from : State.t) (aa : States.t Actions.t)
  : action_pair list
  =
  Actions.fold
    (fun (a : Action.t) (dests : States.t) (acc : action_pair list) ->
      match Action.Label.is_silent a.label with
      | None -> raise (CannotSaturateActionsWithUnknownVisibility a)
      | Some is_silent ->
        let anno = [ from, a ] in
        let named_action = if is_silent then None else Some a in
        let silent_action = if is_silent then Some a else None in
        let acc = if is_silent then acc else (a, dests) :: acc in
        let visited_states = Hashtbl.create (States.cardinal m.states) in
        log_visit from visited_states;
        get_annotated_actions
          m
          visited_states
          anno
          dests
          named_action
          silent_action
          acc)
    aa
    []
;;

let saturate_edges (m : t) : States.t Actions.t Edges.t =
  let edges : States.t Actions.t Edges.t = Edges.create 0 in
  Edges.iter
    (fun (from : State.t) (aa : States.t Actions.t) ->
      match saturate_actions m from aa with
      | [] -> ()
      | saturated_actions ->
        Edges.add edges from (Actions.of_seq (List.to_seq saturated_actions)))
    m.edges;
  edges
;;

let saturate (m : t) : t =
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
  let alphabet = Alphabet.union m1.alphabet m2.alphabet in
  let states = States.union m1.states m2.states in
  let edges = Model.merge_edges m1.edges m2.edges in
  let info : Model.Info.t option =
    match m1.info, m2.info with
    | Some i1, Some i2 ->
      Some
        (Model.Info.merge
           ~num_labels:(Alphabet.cardinal alphabet)
           ~num_states:(States.cardinal states)
           ~num_edges:(Model.get_num_edges edges)
           i1
           i2)
    | _, _ -> None
  in
  { init; alphabet; states; edges; info }
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
  let num_states_str = Printf.sprintf "(%i) " (States.cardinal m.states) in
  let num_edges_str = Printf.sprintf "(%i) " (get_num_edges m.edges) in
  Printf.sprintf
    "\n\
     %s%s{ initial state: %s\n\
     %sinfo: %s\n\
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
