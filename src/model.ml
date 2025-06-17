module States = Set.Make (struct
    type t = Model_state.t

    let compare a b = Model_state.compare a b
  end)

module Partition = Set.Make (States)

module Alphabet = Set.Make (struct
    type t = Model_label.t

    let compare a b = Model_label.compare a b
  end)

module Actions = Hashtbl.Make (struct
    type t = Model_action.t

    let equal t1 t2 = Model_action.eq t1 t2
    let hash t = Model_action.hash t
  end)

module Edges = Hashtbl.Make (struct
    type t = Model_state.t

    let equal t1 t2 = Model_state.eq t1 t2
    let hash t = Model_state.hash t
  end)

(** used by LTS *)
module Transitions = Set.Make (struct
    type t = Model_transition.t

    let compare a b = Model_transition.compare a b
  end)

(*********************************************************************)
(****** Pairs ********************************************************)
(*********************************************************************)

type action_pair = Model_action.t * States.t

(*********************************************************************)
(****** Labels <-> Actions *******************************************)
(*********************************************************************)

let label_to_action ?(meta : Model_label.meta option = None) (l : Model_label.t)
  : Model_action.t
  =
  match meta with
  | None -> { label = l; is_silent = None; info = None; annos = [] }
  | Some m -> { label = l; is_silent = m.is_silent; info = m.info; annos = [] }
;;

let action_to_label (a : Model_action.t) : Model_label.t = a.label

(*********************************************************************)
(****** Edges <-> Edge List ******************************************)
(*********************************************************************)

let edges_to_list (es : States.t Actions.t Edges.t) : Model_edge.t list =
  Edges.fold
    (fun (from : Model_state.t)
      (aa : States.t Actions.t)
      (acc0 : Model_edge.t list) ->
      Actions.fold
        (fun (a : Model_action.t)
          (dests : States.t)
          (acc1 : Model_edge.t list) ->
          States.fold
            (fun (dest : Model_state.t) (acc2 : Model_edge.t list) ->
              (from, a, dest) :: acc2)
            dests
            acc1)
        aa
        acc0)
    es
    []
;;

(*********************************************************************)
(****** Edges <-> Transitions ****************************************)
(*********************************************************************)

let edges_to_transitions (es : States.t Actions.t Edges.t) : Transitions.t =
  Edges.fold
    (fun (from : Model_state.t)
      (aa : States.t Actions.t)
      (acc0 : Transitions.t) ->
      Actions.fold
        (fun (a : Model_action.t) (dests : States.t) (acc1 : Transitions.t) ->
          States.fold
            (fun (dest : Model_state.t) (acc2 : Transitions.t) ->
              Transitions.add
                ( from
                , a.label
                , dest
                , Some { is_silent = a.is_silent; info = a.info } )
                acc2)
            dests
            acc1)
        aa
        acc0)
    es
    Transitions.empty
;;

let transitions_to_edges (ts : Transitions.t) : States.t Actions.t Edges.t =
  let es = Edges.create 0 in
  Transitions.iter
    (fun ((from, label, dest, meta) : Model_transition.t) ->
      let a = label_to_action ~meta label in
      match Edges.find_opt es from with
      | None ->
        Edges.add
          es
          from
          (Actions.of_seq (List.to_seq [ a, States.singleton dest ]))
      | Some aa ->
        (match Actions.find_opt aa a with
         | None -> Actions.add aa a (States.singleton dest)
         | Some dests -> Actions.replace aa a (States.add dest dests)))
    ts;
  es
;;

(*********************************************************************)
(****** Edges <-> Transition List ************************************)
(*********************************************************************)

let edges_to_transition_list (es : States.t Actions.t Edges.t)
  : Model_transition.t list
  =
  Edges.fold
    (fun (from : Model_state.t)
      (aa : States.t Actions.t)
      (acc0 : Model_transition.t list) ->
      Actions.fold
        (fun (a : Model_action.t)
          (dests : States.t)
          (acc1 : Model_transition.t list) ->
          States.fold
            (fun (dest : Model_state.t) (acc2 : Model_transition.t list) ->
              ( from
              , a.label
              , dest
              , Some { is_silent = a.is_silent; info = a.info } )
              :: acc2)
            dests
            acc1)
        aa
        acc0)
    es
    []
;;

let transition_list_to_edges (ts : Model_transition.t list)
  : States.t Actions.t Edges.t
  =
  let es = Edges.create 0 in
  List.iter
    (fun ((from, label, dest, meta) : Model_transition.t) ->
      let a = label_to_action ~meta label in
      match Edges.find_opt es from with
      | None ->
        Edges.add
          es
          from
          (Actions.of_seq (List.to_seq [ a, States.singleton dest ]))
      | Some aa ->
        (match Actions.find_opt aa a with
         | None -> Actions.add aa a (States.singleton dest)
         | Some dests -> Actions.replace aa a (States.add dest dests)))
    ts;
  es
;;

(*********************************************************************)
(****** Actions <-> Alphabet *****************************************)
(*********************************************************************)

let alphabet_from_actions
      ?(acc : Alphabet.t = Alphabet.empty)
      (aa : States.t Actions.t)
  : Alphabet.t
  =
  Actions.fold
    (fun (a : Model_action.t) (_ : States.t) (acc : Alphabet.t) ->
      Alphabet.add a.label acc)
    aa
    acc
;;

let alphabet_from_edges (es : States.t Actions.t Edges.t) : Alphabet.t =
  Edges.fold
    (fun (_ : Model_state.t) (aa : States.t Actions.t) (acc : Alphabet.t) ->
      alphabet_from_actions ~acc aa)
    es
    Alphabet.empty
;;

let alphabet_from_transitions (ts : Transitions.t) : Alphabet.t =
  Transitions.fold
    (fun ((_from, a, _dest, _meta) : Model_transition.t) (acc : Alphabet.t) ->
      Alphabet.add a acc)
    ts
    Alphabet.empty
;;

(*********************************************************************)
(****** Add **********************************************************)
(*********************************************************************)

let add_action
      (aa : States.t Actions.t)
      (a : Model_action.t)
      (dest : Model_state.t)
  : unit
  =
  match Actions.find_opt aa a with
  | None -> Actions.add aa a (States.singleton dest)
  | Some dests -> Actions.replace aa a (States.add dest dests)
;;

let add_edge
      (es : States.t Actions.t Edges.t)
      (from : Model_state.t)
      (a : Model_action.t)
      (dest : Model_state.t)
  : unit
  =
  match Edges.find_opt es from with
  | None ->
    Edges.add
      es
      from
      (Actions.of_seq (List.to_seq [ a, States.singleton dest ]))
  | Some aa -> add_action aa a dest
;;

(*********************************************************************)
(****** Get **********************************************************)
(*********************************************************************)

let get_actions (es : States.t Actions.t Edges.t) : States.t Actions.t =
  let aa : States.t Actions.t = Actions.create 0 in
  List.iter
    (fun ((_from, a, dests) : Model_edge.t) -> add_action aa a dests)
    (edges_to_list es);
  aa
;;

let get_actions_from (from : Model_state.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t
  =
  match Edges.find_opt es from with None -> Actions.create 0 | Some aa -> aa
;;

let get_actions_of (a : Model_action.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t
  =
  let result : States.t Actions.t = Actions.create 0 in
  (match Actions.find_opt (get_actions es) a with
   | None -> ()
   | Some dests -> Actions.add result a dests);
  result
;;

let get_actions_with_label (l : Model_label.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t
  =
  let result : States.t Actions.t = Actions.create 0 in
  List.iter
    (fun ((_from, a, dests) : Model_edge.t) ->
      if Model_action.has_label l a then add_action result a dests)
    (edges_to_list es);
  result
;;

let get_edges_with_label (l : Model_label.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  let result : States.t Actions.t Edges.t = Edges.create 0 in
  List.iter
    (fun ((from, a, dests) : Model_edge.t) -> add_edge result from a dests)
    (List.filter
       (fun ((_from, a, _dests) : Model_edge.t) -> Model_action.has_label l a)
       (edges_to_list es));
  result
;;

let get_edges_from (from : Model_state.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  let result : States.t Actions.t Edges.t = Edges.create 0 in
  (match Edges.find_opt es from with
   | None -> ()
   | Some aa -> Edges.add result from aa);
  result
;;

let get_destinations (aa : States.t Actions.t) : States.t =
  Actions.fold
    (fun (_ : Model_action.t) (dests : States.t) (acc : States.t) ->
      States.union acc dests)
    aa
    States.empty
;;

let get_reachable_blocks (aa : States.t Actions.t) (pi : Partition.t)
  : Partition.t
  =
  let dests = get_destinations aa in
  Partition.filter
    (fun (block : States.t) ->
      Bool.not (States.is_empty (States.inter block dests)))
    pi
;;

let get_reachable_blocks_opt (aa : States.t Actions.t) (pi : Partition.t)
  : Partition.t option
  =
  let blocks = get_reachable_blocks aa pi in
  if Partition.is_empty blocks then None else Some blocks
;;

(*********************************************************************)
(****** Pretty-Strings ***********************************************)
(*********************************************************************)

let pstr_alphabet
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (ls : Alphabet.t)
  : string
  =
  match Alphabet.is_empty ls with
  | true -> "[ ] (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s[%s%s]"
      (if skip_leading_tab then "" else str_indent)
      (Alphabet.fold
         (fun (l : Model_label.t) (acc : string) ->
           Printf.sprintf
             "%s%s\n"
             acc
             (Model_label.pstr ~indents:(indents + 1) l))
         ls
         "\n")
      str_indent
;;

let pstr_states
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (ss : States.t)
  : string
  =
  match States.is_empty ss with
  | true -> "[ ] (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s[%s%s]"
      (if skip_leading_tab then "" else str_indent)
      (States.fold
         (fun (s : Model_state.t) (acc : string) ->
           Printf.sprintf
             "%s%s\n"
             acc
             (Model_state.pstr ~indents:(indents + 1) s))
         ss
         "\n")
      str_indent
;;

let pstr_partition
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (ps : Partition.t)
  : string
  =
  match Partition.is_empty ps with
  | true -> "[ ] (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s[%s%s]"
      (if skip_leading_tab then "" else str_indent)
      (Partition.fold
         (fun (ss : States.t) (acc : string) ->
           Printf.sprintf "%s%s\n" acc (pstr_states ~indents:(indents + 1) ss))
         ps
         "\n")
      str_indent
;;
