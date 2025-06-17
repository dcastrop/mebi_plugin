open Model

type t =
  { init : Model_state.t option
  ; mutable alphabet : Alphabet.t
  ; mutable states : States.t
  ; mutable edges : States.t Actions.t Edges.t
  ; info : Utils.model_info option
  }

let clone (m : t) : t =
  match m with
  | { init; alphabet; states; edges; info } ->
    { init; alphabet; states; edges = Edges.copy edges; info }
;;

(*********************************************************************)
(****** Add **********************************************************)
(*********************************************************************)

let add_label (m : t) (l : Model_label.t) : t =
  match m with
  | { alphabet; _ } -> { m with alphabet = Alphabet.add l alphabet }
;;

let add_label_list (m : t) (ls : Model_label.t list) : t =
  match m with
  | { alphabet; _ } ->
    { m with alphabet = Alphabet.add_seq (List.to_seq ls) alphabet }
;;

let add_state (m : t) (s : Model_state.t) : t =
  match m with { states; _ } -> { m with states = States.add s states }
;;

let add_state_list (m : t) (ss : Model_state.t list) : t =
  match m with
  | { states; _ } -> { m with states = States.add_seq (List.to_seq ss) states }
;;

let add_states (m : t) (ss : States.t) : t =
  match m with { states; _ } -> { m with states = States.union ss states }
;;

let add_edge
      (m : t)
      (from : Model_state.t)
      (a : Model_action.t)
      (dest : Model_state.t)
  : t
  =
  match m with
  | { states; alphabet; edges; _ } ->
    let m = add_label (add_state_list m [ from; dest ]) a.label in
    Model.add_edge m.edges from a dest;
    m
;;

let rec add_edge_list (m : t) (es : Model_edge.t list) : t =
  match es with
  | [] -> m
  | (from, a, dest) :: t -> add_edge_list (add_edge m from a dest) t
;;

let add_action
      ?(meta : Model_label.meta option = None)
      (m : t)
      (from : Model_state.t)
      (l : Model_label.t)
      (dest : Model_state.t)
  : t
  =
  let a = label_to_action l in
  add_edge m from a dest
;;

let rec add_action_list
          (m : t)
          (aa :
            (Model_state.t
            * Model_label.t
            * Model_state.t
            * Model_label.meta option)
              list)
  : t
  =
  match aa with
  | [] -> m
  | (from, a, dest, meta) :: t ->
    add_action_list (add_action ~meta m from a dest) t
;;

(*********************************************************************)
(****** Get **********************************************************)
(*********************************************************************)

let get_actions_from (m : t) (from : Model_state.t) : States.t Actions.t =
  match Edges.find_opt m.edges from with
  | None -> Actions.of_seq (List.to_seq [])
  | Some aa -> aa
;;

(*********************************************************************)
(****** Checks/Boolean Ops *******************************************)
(*********************************************************************)

let has_state (m : t) (s : Model_state.t) : bool = States.mem s m.states

(*********************************************************************)
(****** Saturate *****************************************************)
(*********************************************************************)

let max_revisit_num : int = 2

let can_revisit
      (s : Model_state.t)
      (visited_states : (Model_state.t, int) Hashtbl.t)
  : bool
  =
  match Hashtbl.find_opt visited_states s with
  | None ->
    Hashtbl.add visited_states s 0;
    true
  | Some n -> n < max_revisit_num
;;

let log_visit
      (s : Model_state.t)
      (visited_states : (Model_state.t, int) Hashtbl.t)
  : unit
  =
  match Hashtbl.find_opt visited_states s with
  | None -> Hashtbl.add visited_states s 1
  | Some n -> Hashtbl.replace visited_states s (n + 1)
;;

exception CannotSaturateActionWithNoNamedAction of unit

let annotate_action
      (p : Model_action.annotation_pair)
      (named_action : Model_action.t option)
      (anno : Model_action.annotation)
  : Model_action.t
  =
  match named_action with
  | None ->
    (match
       List.find_opt
         (fun ((_s, b) : Model_action.annotation_pair) ->
           Model_action.is_silent b)
         anno
     with
     | None -> raise (CannotSaturateActionWithNoNamedAction ())
     | Some (_s, b) -> Model_action.saturated ?anno:(Some (p :: anno)) b)
  | Some b -> Model_action.saturated ?anno:(Some (p :: anno)) b
;;

exception CannotSaturateActionsWithUnknownVisibility of Model_action.t

let rec get_annotated_actions
          (m : t)
          (visited_states : (Model_state.t, int) Hashtbl.t)
          (anno : Model_action.annotation)
          (to_visit : States.t)
          (named_action : Model_action.t option)
          (acc0 : action_pair list)
  : action_pair list
  =
  States.fold
    (fun (dest : Model_state.t) (acc1 : action_pair list) ->
      match Edges.find_opt m.edges dest with
      | None -> acc1 (* terminal state *)
      | Some dest_actions ->
        if can_revisit dest visited_states then log_visit dest visited_states;
        Actions.fold
          (fun (a : Model_action.t)
            (dests : States.t)
            (acc2 : action_pair list) ->
            match a.is_silent, named_action with
            | None, _ -> raise (CannotSaturateActionsWithUnknownVisibility a)
            (* stop if [a] is not silent and already have [named_action] *)
            | Some false, Some _ -> acc2
            (* if [a] is silent and have not yet found [named_action] *)
            | Some true, None ->
              get_annotated_actions
                m
                visited_states
                ((dest, a) :: anno)
                dests
                named_action
                acc2
            (* if [a] is silent and have already found [named_action] *)
            | Some true, Some _ ->
              get_annotated_actions
                m
                visited_states
                ((dest, a) :: anno)
                dests
                named_action
                ((annotate_action (dest, a) named_action anno, dests) :: acc2)
            (* if [a] is not silent and have not yet found [named_action] *)
            | Some false, None ->
              get_annotated_actions
                m
                visited_states
                ((dest, a) :: anno)
                dests
                (Some a)
                ((annotate_action (dest, a) (Some a) anno, dests) :: acc2))
          dest_actions
          acc1)
    to_visit
    []
;;

let saturate_actions (m : t) (from : Model_state.t) (aa : States.t Actions.t)
  : States.t Actions.t
  =
  let actions : States.t Actions.t = Actions.create 0 in
  Actions.iter
    (fun (a : Model_action.t) (dests : States.t) ->
      match a.is_silent with
      | None -> raise (CannotSaturateActionsWithUnknownVisibility a)
      | Some is_silent ->
        let annotated_actions : (Model_action.t * States.t) list =
          get_annotated_actions
            m
            (Hashtbl.create (States.cardinal m.states))
            [ from, a ]
            dests
            None
            []
        in
        Actions.add_seq actions (List.to_seq annotated_actions))
    aa;
  actions
;;

let saturate_edges (m : t) : t =
  let edges : States.t Actions.t Edges.t = Edges.create 0 in
  Edges.iter
    (fun (from : Model_state.t) (aa : States.t Actions.t) ->
      Edges.add edges from (saturate_actions m from aa))
    m.edges;
  { m with edges }
;;

let saturate (m : t) : t =
  let m = clone m in
  saturate_edges m
;;

(*********************************************************************)
(****** Fsm Pairs ****************************************************)
(*********************************************************************)

type pair = t * t

let saturate_pair (p : pair) : pair = saturate (fst p), saturate (snd p)

exception StateOriginNotFound of (pair * Model_state.t)

(** similar to [compare], returns [-1] if [s] is only in [fst p], [1] if only in [snd p] and [0] if in both of [p]
*)
let state_origin_opt (p : pair) (s : Model_state.t) : int option =
  match has_state (fst p) s, has_state (snd p) s with
  | true, true -> Some 0
  | true, false -> Some (-1)
  | false, true -> Some 1
  | false, false -> None
;;

let state_origin (p : pair) (s : Model_state.t) : int =
  match state_origin_opt p s with
  | None -> raise (StateOriginNotFound (p, s))
  | Some i -> i
;;

(*********************************************************************)
(****** Merge ********************************************************)
(*********************************************************************)

let merge (p : pair) : t =
  let m1, m2 = p in
  let init = None in
  let alphabet = Alphabet.union m1.alphabet m2.alphabet in
  let states = States.union m1.states m2.states in
  let merged = add_edge_list m1 (edges_to_list m2.edges) in
  let edges = merged.edges in
  (* if Bool.not (Alphabet.equal alphabet merged.alphabet)
  then
    Utils.Logging.Log.warning
      (Printf.sprintf
         "Fsm.merge, yielded different alphabets.\nA: %s\nB:%s"
         (Model.pstr_alphabet alphabet)
         (Model.pstr_alphabet merged.alphabet)); *)
  (* if Bool.not (States.equal states merged.states)
  then
    Utils.Logging.Log.warning
      (Printf.sprintf
         "Fsm.merge, yielded different states.\nA: %s\nB:%s"
         (Model.pstr_states states)
         (Model.pstr_states merged.states)); *)
  let info = None in
  { init; alphabet; states; edges; info }
;;

(*********************************************************************)
(****** Pretty-Strings ***********************************************)
(*********************************************************************)
let pstr ?(skip_leading_tab : bool = false) ?(indents : int = 0) (m : t)
  : string
  =
  (* Printf.sprintf *)
  "TODO: Fsm.pstr"
;;
