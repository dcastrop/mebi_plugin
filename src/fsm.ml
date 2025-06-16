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
    (match Edges.find_opt edges from with
     | None ->
       Edges.add
         edges
         from
         (Actions.of_seq (List.to_seq [ a, States.singleton dest ]))
     | Some es ->
       (match Actions.find_opt es a with
        | None -> Actions.add es a (States.singleton dest)
        | Some dests -> Actions.replace es a (States.add dest dests)));
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
(****** Merge ********************************************************)
(*********************************************************************)

let merge (m1 : t) (m2 : t) : t =
  let init = None in
  let alphabet = Alphabet.union m1.alphabet m2.alphabet in
  let states = States.union m1.states m2.states in
  let merged = add_edge_list m1 (edges_to_list m2.edges) in
  let edges = merged.edges in
  assert (Alphabet.equal alphabet merged.alphabet);
  assert (States.equal states merged.states);
  let info = None in
  { init; alphabet; states; edges; info }
;;
