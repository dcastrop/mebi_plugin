module States = Set.Make (struct
    type t = Model_state.t

    let compare a b = Mebi_wrapper.E.compare a b
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

    let equal t1 t2 = Mebi_wrapper.E.eq t1 t2
    let hash t = Mebi_wrapper.E.hash t
  end)

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
(****** Edges <-> Transitions ****************************************)
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
(****** Edges <-> Edge List ******************************************)
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
