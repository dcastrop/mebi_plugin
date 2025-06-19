open Model

type t =
  { init : State.t option
  ; alphabet : Alphabet.t
  ; states : States.t
  ; transitions : Transitions.t
  ; info : Info.t option
  }

let create
      (init : State.t option)
      (alphabet : Alphabet.t)
      (states : States.t)
      (transitions : Transitions.t)
      (info : Info.t option)
  : t
  =
  { init; alphabet; states; transitions; info }
;;

let create_from (m : Model.t) : t =
  match m with
  | LTS (init, alphabet, states, transitions, info) ->
    create init alphabet states transitions info
  | FSM (init, alphabet, states, edges, info) ->
    let transitions = Model.edges_to_transitions edges in
    create init alphabet states transitions info
;;

(*********************************************************************)
(****** Add **********************************************************)
(*********************************************************************)

(* let add_label (g : t) (l : Action.Label.t) : t =
  match g with
  | { alphabet; _ } -> { g with alphabet = Alphabet.add l alphabet }
;;

let add_label_list (g : t) (ls : Action.Label.t list) : t =
  match g with
  | { alphabet; _ } ->
    { g with alphabet = Alphabet.add_seq (List.to_seq ls) alphabet }
;; *)

let add_action (m : t) (a : Action.t) : t =
  match m with
  | { alphabet; _ } -> { m with alphabet = Alphabet.add a alphabet }
;;

let add_action_list (m : t) (al : Action.t list) : t =
  match m with
  | { alphabet; _ } ->
    { m with alphabet = Alphabet.add_seq (List.to_seq al) alphabet }
;;

let add_state (g : t) (s : State.t) : t =
  match g with
  | { init; alphabet; states; transitions; info } ->
    { g with states = States.add s states }
;;

let add_state_list (g : t) (ss : State.t list) : t =
  match g with
  | { init; alphabet; states; transitions; info } ->
    { g with states = States.add_seq (List.to_seq ss) states }
;;

let add_states (g : t) (ss : States.t) : t =
  match g with
  | { init; alphabet; states; transitions; info } ->
    { g with states = States.union ss states }
;;

let add_transition
      (g : t)
      (from : State.t)
      (l : Action.Label.t)
      (dest : State.t)
      (meta : Action.MetaData.t option)
  : t
  =
  match g with
  | { init; alphabet; states; transitions; info } ->
    { g with transitions = Transitions.add (from, l, dest, meta) transitions }
;;

let add_transition_from_action
      (g : t)
      (from : State.t)
      (a : Action.t)
      (dest : State.t)
  : t
  =
  add_transition g from a.label dest (Some a.meta)
;;

(*********************************************************************)
(****** Pretty-Strings ***********************************************)
(*********************************************************************)

let to_string
      ?(pstr : bool = false)
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (g : t)
  : string
  =
  let str_indent0 = Utils.str_tabs indents in
  let str_indent1 = Utils.str_tabs (indents + 1) in
  let outer, sep =
    if pstr
    then str_indent0, Printf.sprintf "\n%s%s; " str_indent0 str_indent1
    else "", ""
  in
  let num_alpha_str = Printf.sprintf "(%i) " (Alphabet.cardinal g.alphabet) in
  let num_states_str = Printf.sprintf "(%i) " (States.cardinal g.states) in
  let num_edges_str =
    Printf.sprintf "(%i) " (Transitions.cardinal g.transitions)
  in
  Printf.sprintf
    "\n\
     %s%s{ initial state: %s\n\
     %sinfo: %s\n\
     %salphabet: %s%s\n\
     %sstates: %s%s\n\
     %stransitions: %s%s\n\
     %s}"
    (if skip_leading_tab then "" else str_indent0)
    outer
    (pstr_state_opt ~indents:(indents + 1) g.init)
    sep
    (pstr_info_opt ~indents:(indents + 1) g.info)
    sep
    num_alpha_str
    (pstr_alphabet ~skip_leading_tab:true ~indents:(indents + 1) g.alphabet)
    sep
    num_states_str
    (pstr_states ~skip_leading_tab:true ~indents:(indents + 1) g.states)
    sep
    num_edges_str
    (pstr_transitions
       ~skip_leading_tab:true
       ~indents:(indents + 1)
       g.transitions)
    outer
;;

let pstr ?(skip_leading_tab : bool = false) ?(indents : int = 0) (g : t)
  : string
  =
  to_string ~pstr:true ~skip_leading_tab ~indents g
;;
