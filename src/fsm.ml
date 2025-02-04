(*********************************************************************)
(****** States *******************************************************)
(*********************************************************************)

(** [state] is a 2-tuple with a unique [id] and (non-unique) name ([pp]).
    [id] is an integer for identifying the state.
    [pp] is a pretty-printed string of something corresponding to this state. *)
type state =
  { id : int (* ; hash : int *)
  ; pp : string
  }

(** [make_state ?pp id] is a wrapper constructor for [state].
    @param ?pp is a pretty-printed respresentation, which defaults to [s{id}].
    @param id is the unique identifier for the state. *)
let make_state ?(pp : string option) (id : int) =
  { id
  ; pp =
      (match pp with
       | None -> Printf.sprintf "s%d" id
       | Some pp' -> pp')
  }
;;

(** [States] is a set of [states]. *)
module States = Set.Make (struct
    type t = state

    let compare a b = compare a.id b.id
    (* let compare a b = compare a.pp b.pp *)
  end)

(*************************************************)
(****** Blocks & Partitions of States ************)
(****** (Used by Bisimilarity algorithms.) *******)
(****** (Featured here for more seemless pstr.) **)
(*************************************************)

(** [Block] is a set of states.
    This is necessary since [KS90] requires the actions of states to be sortable. *)
module Block = States

(** [Partition] is a set of [Blocks]. (Required by [KS90].) *)
module Partition = Set.Make (States)

(*********************************************************************)
(****** Action Labels & Alphabet *************************************)
(*********************************************************************)

(** [action] is a 2-tuple with a unique [id] and (non-unique) [label]).
    - [id] is an integer for identifying the action.
    - [label] is a (pretty-printed) string describing the action. *)
type action =
  { id : int
  ; label : string
  }

(** [make_action ?label id] is a wrapper constructor for [action].
    @param ?label is a pretty-printed representation, which defaults to [s{id}].
    @param id is the unique identifier for the state. *)
let make_action ?(label : string option) (id : int) : action =
  { id
  ; label =
      (match label with
       | None -> Printf.sprintf "l%d" id
       | Some label' -> label')
  }
;;

(** [Alphabet] is a set of [actions]. *)
module Alphabet = Set.Make (struct
    type t = action

    let compare a b = compare a.label b.label
  end)

(*********************************************************************)
(****** Actions & Edges ***********************************)
(*********************************************************************)

(** [Actions] map [actions] to sets of destination [states]. *)
module Actions = Hashtbl.Make (struct
    type t = action

    let equal (t1 : action) (t2 : action) = Int.equal t1.id t2.id
    let hash (t : action) = Hashtbl.hash t
  end)

(** [make_actions ?size] is a wrapper constructor for [Actions]. *)
let make_actions ?(size : int = 0) () : States.t Actions.t = Actions.create size

(** [Edges] map [states] to outgoing actions, mapped to sets of destination [states]. *)
module Edges = Hashtbl.Make (struct
    type t = state

    let equal (t1 : state) (t2 : state) = Int.equal t1.id t2.id
    let hash (t : state) = Hashtbl.hash t
  end)

(** [make_edges ?size] is a wrapper constructor for [Actions]. *)
let make_edges ?(size : int = 0) () : States.t Actions.t Edges.t =
  Edges.create size
;;

(**  *)
let add_new_outgoing_edge
  (edges : States.t Actions.t Edges.t)
  (from_state : state)
  (out_action : action)
  (dest_state : state)
  : unit
  =
  match Edges.find_opt edges from_state with
  (* add new set of actions from state *)
  | None ->
    Edges.add
      edges
      from_state
      (Actions.of_seq
         (List.to_seq [ out_action, States.of_list [ dest_state ] ]))
  (* actions already exist, update them *)
  | Some existing_actions ->
    (match Actions.find_opt existing_actions out_action with
     (* this specific action does not exist, add *)
     | None ->
       Actions.add existing_actions out_action (States.of_list [ dest_state ])
     (* update destination states of this action which already exists *)
     | Some existing_destinations ->
       Actions.replace
         existing_actions
         out_action
         (States.add dest_state existing_destinations))
;;

(*********************************************************************)
(****** FSM **********************************************************)
(*********************************************************************)

(** [fsm] is a type used to describe an FSM in OCaml.
    - [init] is the initial state.
    - [alphabet] is the set of labels of actions of edges.
    - [states] is a set of states.
    - [edges] is a hashtable mapping states to outgoing actions and their detination states. *)
type fsm =
  { init : state
  ; alphabet : Alphabet.t
  ; states : States.t
  ; edges : States.t Actions.t Edges.t
  }
(* TODO: Currently, there may be many copies of the same state in an [fsm] (i.e., in [init] and the [edges]). Maybe add list of states and change others to be an index referencing their position in the list. *)

(** [make_fsm init alphabet states edges] is a wrapper constructor for [fsm]. *)
let make_fsm
  (init : state)
  (alphabet : Alphabet.t)
  (states : States.t)
  (edges : States.t Actions.t Edges.t)
  : fsm
  =
  { init; alphabet; states; edges }
;;

(** [make_fsm_from_lts] is a more user-friendly helper function for defining FSMs.
    @param init_label is the string label of the initial state.
    @param transitions
      is the list of pairs of states and, their list pairs of actions, with their list of destination states. *)

let make_fsm_from_lts
  (init_label : string)
  (transitions : (string * (string * string list) list) list)
  : fsm
  =
  let init : state = make_state ~pp:init_label 0 in
  (* traverse transitions and collect states, labels and edges *)
  let states, alphabet, edges =
    List.fold_left
      (fun ((states, alphabet, edges) :
             States.t * Alphabet.t * States.t Actions.t Edges.t)
        ((lhs, labels_rhs) : string * (string * string list) list) ->
        (* unpack outgoing state *)
        let states', from_state =
          let new_state : state = make_state ~pp:lhs (States.cardinal states) in
          let existing_state : States.t =
            States.filter
              (fun (existing_state : state) ->
                existing_state.pp == new_state.pp)
              states
          in
          match States.is_empty existing_state with
          | true -> States.add new_state states, new_state
          | false ->
            assert (States.cardinal existing_state == 1);
            states, List.hd (States.elements existing_state)
        in
        (* go through each outgoing edge *)
        let states'', alphabet', edges' =
          List.fold_left
            (fun ((states'', alphabet', edges') :
                   States.t * Alphabet.t * States.t Actions.t Edges.t)
              ((label, rhs) : string * string list) ->
              (* unpack outgoing label *)
              let alphabet'', out_action =
                let new_action : action =
                  make_action ~label (Alphabet.cardinal alphabet')
                in
                match Alphabet.find_opt new_action alphabet' with
                | None -> Alphabet.add new_action alphabet', new_action
                | Some existing_action -> alphabet', existing_action
              in
              (* go through each destination state *)
              let states''', edges'' =
                List.fold_left
                  (fun ((states''', edges'') :
                         States.t * States.t Actions.t Edges.t)
                    (destination : string) ->
                    (* unpack destination state *)
                    let states'''', dest_state =
                      let dest_state : state =
                        make_state ~pp:destination (States.cardinal states''')
                      in
                      let existing_dest : States.t =
                        States.filter
                          (fun (existing_dest : state) ->
                            existing_dest.pp == dest_state.pp)
                          states'''
                      in
                      match States.is_empty existing_dest with
                      | true -> States.add dest_state states''', dest_state
                      | false ->
                        states''', List.hd (States.to_list existing_dest)
                    in
                    (* add to edges *)
                    add_new_outgoing_edge
                      edges''
                      from_state
                      out_action
                      dest_state;
                    states'''', edges'')
                  (* return. *)
                  (states'', edges')
                  rhs
              in
              (* return. *)
              states''', alphabet'', edges'')
            (states', alphabet, edges)
            labels_rhs
        in
        (* return. *)
        states'', alphabet', edges')
      ( States.of_list [ init ]
      , Alphabet.empty
      , Edges.create (List.length transitions) )
      transitions
  in
  (* make fsm. *)
  { init; alphabet; states; edges }
;;

(*********************************************************************)
(****** Pretty-Printing **********************************************)
(*********************************************************************)

(********************************************)
(****** States ******************************)
(********************************************)

module PStr = struct
  open Utils

  (* make new param that wraps around logging, stating the tab level and such *)
  type formatting_params =
    { tabs : int
    ; no_leading_tab : bool (* ; non_repetative : bool *)
    ; params : logging_params
    }

  let default_formatting_params
    ?(params : logging_params = default_logging_params ())
    ()
    : formatting_params
    =
    { tabs = 0; no_leading_tab = true; (*non_repetative = true;*) params }
  ;;

  let inc_tab ?(by : int = 1) (params : formatting_params) : formatting_params =
    { tabs = params.tabs + by
    ; no_leading_tab =
        params.no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  let dec_tab ?(by : int = 1) (params : formatting_params) : formatting_params =
    { tabs = (if params.tabs - by < 0 then 0 else params.tabs + by)
    ; no_leading_tab =
        params.no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  let no_tab (params : formatting_params) : formatting_params =
    { tabs = 0
    ; no_leading_tab =
        params.no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  let no_leading_tab (_no_leading_tab : bool) (params : formatting_params)
    : formatting_params
    =
    { tabs = params.tabs
    ; no_leading_tab =
        _no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  type pstr_params =
    | Logging of logging_params
    | Formatting of formatting_params

  let handle_formatting_params (params : pstr_params) : formatting_params =
    match params with
    | Formatting _formatting_params -> _formatting_params
    | Logging _logging_params ->
      default_formatting_params ~params:_logging_params ()
  ;;

  (********************************************)
  (****** States ******************************)
  (********************************************)

  let state
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    (s : state)
    : string
    =
    let _params : formatting_params = handle_formatting_params params in
    let tabs : string = str_tabs _params.tabs in
    Printf.sprintf
      "%s%s"
      (if _params.no_leading_tab then "" else tabs)
      (let normal_pstr : string = Printf.sprintf "(%s)" s.pp
       and detail_pstr : string = Printf.sprintf "(%s | id:%d)" s.pp s.id in
       match _params.params.kind with
       | Normal () -> normal_pstr
       | Details () -> detail_pstr
       | Debug () -> detail_pstr
       | Warning () -> detail_pstr)
  ;;

  let states
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    (states : States.t)
    : string
    =
    if States.is_empty states
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : formatting_params = handle_formatting_params params in
      let _params' : formatting_params = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (States.fold
           (fun (s : state) (acc : string) ->
             Printf.sprintf
               "%s%s\n"
               acc
               (state ~params:(Formatting (no_leading_tab false _params')) s))
           states
           "\n")
        tabs)
  ;;

  let partition
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    (partition : Partition.t)
    : string
    =
    if Partition.is_empty partition
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : formatting_params = handle_formatting_params params in
      let _params' : formatting_params = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (Partition.fold
           (fun (states' : States.t) (acc : string) ->
             Printf.sprintf
               "%s%s\n"
               acc
               (states
                  ~params:(Formatting (no_leading_tab false _params'))
                  states'))
           partition
           "\n")
        tabs)
  ;;

  (********************************************)
  (****** Alphabet, Actions & Edges ***********)
  (********************************************)

  let action
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    (a : action)
    : string
    =
    let _params : formatting_params = handle_formatting_params params in
    let tabs : string = str_tabs _params.tabs in
    Printf.sprintf
      "%s%s"
      (if _params.no_leading_tab then "" else tabs)
      (let normal_pstr : string = Printf.sprintf "(%s)" a.label
       and detail_pstr : string = Printf.sprintf "(%s | id:%d)" a.label a.id in
       match _params.params.kind with
       | Normal () -> normal_pstr
       | Details () -> detail_pstr
       | Debug () -> detail_pstr
       | Warning () -> detail_pstr)
  ;;

  let alphabet
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    (actions : Alphabet.t)
    : string
    =
    if Alphabet.is_empty actions
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : formatting_params = handle_formatting_params params in
      let _params' : formatting_params = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (Alphabet.fold
           (fun (a : action) (acc : string) ->
             Printf.sprintf
               "%s%s\n"
               acc
               (action ~params:(Formatting (no_leading_tab false _params')) a))
           actions
           "\n")
        tabs)
  ;;

  let edge
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    ((from, a, destination) : state * action * state)
    : string
    =
    let _params : formatting_params = handle_formatting_params params in
    let _params' : formatting_params = no_tab _params
    and tabs : string = str_tabs _params.tabs in
    Printf.sprintf
      "%s{ %s ---%s--> %s }"
      (if _params.no_leading_tab then "" else tabs)
      (state ~params:(Formatting _params') from)
      (action ~params:(Formatting _params') a)
      (state ~params:(Formatting _params') destination)
  ;;

  let actions
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    ?(from : state option)
    (actions' : States.t Actions.t)
    : string
    =
    if Actions.length actions' == 0
    then "{ } (empty)"
    else (
      (* increment tab of inner elements of table *)
      let _params : formatting_params = handle_formatting_params params in
      let tabs : string = str_tabs _params.tabs in
      match from with
      (* if no from state, then this is a non-repetative representation *)
      | None ->
        (* should not be leading next *)
        let _params' : formatting_params =
          no_leading_tab true (inc_tab _params)
        in
        Printf.sprintf
          "%s{[%s]}"
          (if _params.no_leading_tab then "" else tabs)
          (Actions.fold
             (fun (a : action) (destinations : States.t) (acc : string) ->
               Printf.sprintf
                 "%s{ --%s--> %s }\n"
                 acc
                 (action ~params:(Formatting _params') a)
                 (states ~params:(Formatting _params') destinations))
             actions'
             "\n")
      (* if some from state, then go and pstr individual edges *)
      | Some from_state ->
        Actions.fold
          (fun (a : action) (destinations : States.t) (acc : string) ->
            Printf.sprintf
              "%s%s"
              acc
              (States.fold
                 (fun (destination : state) (acc' : string) ->
                   Printf.sprintf
                     "%s%s\n"
                     acc'
                     (edge
                        ~params:(Formatting (no_leading_tab false _params))
                        (from_state, a, destination)))
                 destinations
                 ""))
          actions'
          "")
  ;;

  let edges
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    (edges' : States.t Actions.t Edges.t)
    : string
    =
    if Edges.length edges' == 0
    then "{ } (empty)"
    else (
      (* increment tab of inner elements of table *)
      let _params : formatting_params = handle_formatting_params params in
      let _params' : formatting_params = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s{[%s%s]}"
        (if _params.no_leading_tab then "" else tabs)
        (Edges.fold
           (fun (from_state : state)
             (actions' : States.t Actions.t)
             (acc : string) ->
             Printf.sprintf
               "%s%s"
               acc
               (actions
                  ~params:(Formatting (no_leading_tab false _params'))
                  ?from:(Some from_state)
                  actions'))
           edges'
           "\n")
        tabs)
  ;;

  let fsm
    ?(params : pstr_params = Formatting (default_formatting_params ()))
    (fsm' : fsm)
    : string
    =
    (* increment tab of inner elements of fsm *)
    let _params : formatting_params = handle_formatting_params params in
    let _params' : formatting_params = inc_tab ~by:2 _params
    and tabs : string = str_tabs _params.tabs
    and tabs' : string = str_tabs (_params.tabs + 1) in
    Printf.sprintf
      "{ %s; %s; %s; %s; \n%s}"
      (Printf.sprintf
         "\n%sinitial state: %s"
         tabs'
         (state ~params:(Formatting _params') fsm'.init))
      (Printf.sprintf
         "\n%salphabet: %s"
         tabs'
         (alphabet ~params:(Formatting _params') fsm'.alphabet))
      (Printf.sprintf
         "\n%sstates: %s"
         tabs'
         (states ~params:(Formatting _params') fsm'.states))
      (Printf.sprintf
         "\n%sedges: %s"
         tabs'
         (edges ~params:(Formatting _params') fsm'.edges))
      tabs
  ;;
end

(*********************************************************************)
(****** Getter Functions *********************************************)
(*********************************************************************)

(********************************************)
(****** States ******************************)
(********************************************)

exception StateNotFoundWithID of (int * States.t)
exception MultipleStatesFoundWithID of (int * States.t)

(** @see [get_action_by_id]. *)
let get_state_by_id (states : States.t) (id : int) : state =
  let filtered = States.filter (fun (s : state) -> s.id == id) states in
  if States.is_empty filtered then raise (StateNotFoundWithID (id, states));
  if States.cardinal filtered > 1
  then raise (MultipleStatesFoundWithID (id, states));
  List.nth (States.elements filtered) 0
;;

exception StateNotFoundWithName of (string * States.t)
exception MultipleStatesFoundWithName of (string * States.t)

(** @see [get_action_by_label]. *)
let _get_state_by_name (states : States.t) (name : string) : state =
  let filtered = States.filter (fun (s : state) -> s.pp == name) states in
  if States.is_empty filtered then raise (StateNotFoundWithName (name, states));
  if States.cardinal filtered > 1
  then raise (MultipleStatesFoundWithName (name, states));
  List.nth (States.elements filtered) 0
;;

(********************************************)
(****** Alphabet ****************************)
(********************************************)

let _get_action_alphabet_from_actions (actions : States.t Actions.t)
  : Alphabet.t
  =
  Alphabet.of_list (List.of_seq (Actions.to_seq_keys actions))
;;

let _get_action_alphabet_from_edges (es : States.t Actions.t Edges.t)
  : Alphabet.t
  =
  Alphabet.of_list
    (Edges.fold
       (fun (_from_state : state)
         (actions : States.t Actions.t)
         (acc : action list) ->
         List.append
           acc
           (Actions.fold
              (fun (action : action)
                (_destinations : States.t)
                (acc' : action list) -> List.append acc' [ action ])
              actions
              []))
       es
       [])
;;

(********************************************)
(****** Actions *****************************)
(********************************************)

exception ActionNotFoundWithID of (int * Alphabet.t)
exception MultipleActionsFoundWithID of (int * Alphabet.t)

(** [get_action_by_id alphabet id] returns the action within [alphabet] with the matching [id].
    @raise ActionNotFoundWithID if no action is found in [alphabet] matching [id].
    @raise MultipleActionsFoundWithID if multiple actions are found in [alphabet] matching [id].
    @see [get_action_by_label]. *)
let get_action_by_id (alphabet : Alphabet.t) (id : int) : action =
  let filtered = Alphabet.filter (fun (a : action) -> a.id == id) alphabet in
  if Alphabet.is_empty filtered then raise (ActionNotFoundWithID (id, alphabet));
  if Alphabet.cardinal filtered > 1
  then raise (MultipleActionsFoundWithID (id, alphabet));
  List.nth (Alphabet.elements filtered) 0
;;

exception ActionNotFoundWithLabel of (string * Alphabet.t)
exception MultipleActionsFoundWithLabel of (string * Alphabet.t)

(** [get_action_by_label alphabet label] returns the action within [alphabet] with the matching [label].
    @raise ActionNotFoundWithLabel if no action is found in [alphabet] matching [label].
    @raise MultipleActionsFoundWithLabel if multiple actions are found in [alphabet] matching [label].
    @see [get_action_by_id]. *)
let get_action_by_label (alphabet : Alphabet.t) (label : string) : action =
  let filtered =
    Alphabet.filter (fun (a : action) -> a.label == label) alphabet
  in
  if Alphabet.is_empty filtered
  then raise (ActionNotFoundWithLabel (label, alphabet));
  if Alphabet.cardinal filtered > 1
  then raise (MultipleActionsFoundWithLabel (label, alphabet));
  List.nth (Alphabet.elements filtered) 0
;;

(********************************************)
(****** Edges *******************************)
(********************************************)

(** [get_edges_of a edges] filters [edges] by action [a]. *)
let get_edges_of (a : action) (edges : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  let edges_of = Edges.create 0 in
  Edges.iter
    (fun (from_state : state) (outgoing_edges : States.t Actions.t) : unit ->
      match Actions.find_opt outgoing_edges a with
      (* skip edge without action [a]. *)
      | None -> ()
      (* edge has an [a] aciton. *)
      | Some destinations ->
        Edges.add
          edges_of
          from_state
          (Actions.of_seq (List.to_seq [ a, destinations ])))
    edges;
  edges_of
;;

(** [get_actions_from from edges] is a shorthand for [Edges.find_opt] and in the case of [None] returns an empty map of actions. *)
let get_actions_from (from : state) (edges : States.t Actions.t Edges.t)
  : States.t Actions.t
  =
  match Edges.find_opt edges from with
  | None -> make_actions ~size:0 ()
  | Some actions -> actions
;;

(** [has_destinations] is a type denoting either maps of edges or actions, both of which have destination states. *)
type has_destinations =
  | Actions of States.t Actions.t
  | Edges of States.t Actions.t Edges.t

(** [get_all_destinations edgse] is the set of destination states at the end of each [edges].
    @return the set of detination states of [edges].
    @param edges is either a map of actions or edges. *)
let rec get_all_destinations (edges : has_destinations) : States.t =
  match edges with
  | Actions es ->
    Actions.fold
      (fun (_a : action) (destinations : States.t) (acc : States.t) ->
        States.union acc destinations)
      es
      States.empty
  | Edges es ->
    Edges.fold
      (fun (_from_state : state) (action : States.t Actions.t) (acc : States.t) ->
        States.union acc (get_all_destinations (Actions action)))
      es
      States.empty
;;

(********************************************)
(****** Other Functions *********************)
(****** (e.g., translation map helpers) *****)
(********************************************)

exception ReverseStateHashtblLookupFailed of ((state, state) Hashtbl.t * state)

(** [get_reverse_map_state tbl v] gets the key corresponding to [v] of translation map [tbl].
    @return the key corresponding to value [v] in [tbl].
    @raise ReverseStateHashtblLookupFailed if [v] is not a value in [tbl]. *)
let get_reverse_map_state (tbl : (state, state) Hashtbl.t) (v : state) : state =
  match Utils.get_key_of_val tbl v with
  | None -> raise (ReverseStateHashtblLookupFailed (tbl, v))
  | Some key -> key
;;

(********************************************)
(****** Merging *****************************)
(********************************************)

exception StateNotFoundInMergedStates of (state * States.t)

(** [map_merge_states a b] merges sets of states in [a] and [b] by combining them and assigning them new [ids] where necessary.
    Note: we do not use the [union] of the sets since this would cause states of one to be lost in the merger.
    @return
      the merger of [a] and [b], along with a bidirectional mapping from original and merged states.
    @param a is a set of states to be merged.
    @param b is a set of states to be merged. *)
let map_merge_states (a : States.t) (b : States.t)
  : States.t * (state, state) Hashtbl.t * (state, state) Hashtbl.t
  =
  let num_states : int = States.cardinal a + States.cardinal b in
  let map_of_merged_states : (state, state) Hashtbl.t =
    num_states |> Hashtbl.create
  and map_of_original_states : (state, state) Hashtbl.t =
    num_states |> Hashtbl.create
  in
  (* let merged_states = States.union a b in *)
  let merge_states (states : States.t) (into : States.t) : States.t =
    States.fold
      (fun (s : state) (acc : States.t) ->
        let s' = make_state ~pp:s.pp (States.cardinal acc) in
        (* add to map *)
        Hashtbl.add map_of_merged_states s s';
        Hashtbl.add map_of_original_states s' s;
        (* add to merged states *)
        States.add s' acc)
      states
      into
  in
  let merged_states = merge_states b (merge_states a States.empty) in
  (* *)
  assert (States.cardinal merged_states == num_states);
  (* *)
  merged_states, map_of_merged_states, map_of_original_states
;;

exception ActionNotFoundInMergedAlphabet of (action * Alphabet.t)

(** [map_merge_alphabet a b] merges alphabets [a] and [b].
    @return
      the union of [a] and [b], along with a mapping from the original to the merged actions.
    @param a is an alphabet to merge.
    @param b is an alphabet to merge.
    @raise ActionNotFoundInMergedAlphabet
      if when creating the mapping between original and merged actions, a merged action cannot be found. *)
let map_merge_alphabet (a : Alphabet.t) (b : Alphabet.t)
  : Alphabet.t * (action, action) Hashtbl.t
  =
  let map_of_alphabet : (action, action) Hashtbl.t =
    Alphabet.cardinal a + Alphabet.cardinal b |> Hashtbl.create
  in
  let merged_alphabet = Alphabet.union a b in
  let add_to_alphabet_map (some_alphabet : Alphabet.t) : unit =
    Alphabet.iter
      (fun (act : action) ->
        Hashtbl.add
          map_of_alphabet
          act
          (match Alphabet.find_opt act merged_alphabet with
           | None ->
             raise (ActionNotFoundInMergedAlphabet (act, merged_alphabet))
           | Some act' -> act'))
      some_alphabet
  in
  add_to_alphabet_map a;
  add_to_alphabet_map b;
  (* *)
  merged_alphabet, map_of_alphabet
;;

exception StateNotFoundInMapOfStates of (state * (state, state) Hashtbl.t)
exception ActionNotFoundInMapOfAlphabet of (action * (action, action) Hashtbl.t)

(** [map_merge_edges a b map_of_alphabet map_of_states] merges [a] and [b] and updates the relevant states and actions accordingly.
    @return a merger of [a] and [b] with updated states and actions.
    @param a is a map of edges to merge.
    @param b is a map of edges to merge.
    @param map_of_alphabet is a map from original actions to merged actions.
    @param map_of_states is a map from original states to merged states.
    @raise StateNotFoundInMapOfStates
      if when updating a state in an edge, it cannot be found in [map_of_states].
    @raise ActionNotFoundInMapOfAlphabet
      if when updating a action in an edge, it cannot be found in [map_of_alphabet]. *)
let map_merge_edges
  (a : States.t Actions.t Edges.t)
  (b : States.t Actions.t Edges.t)
  (map_of_alphabet : (action, action) Hashtbl.t)
  (map_of_states : (state, state) Hashtbl.t)
  : States.t Actions.t Edges.t
  =
  let merged_edges = Edges.length a + Edges.length b |> Edges.create in
  let add_to_merged_edges (edges : States.t Actions.t Edges.t) : unit =
    Edges.iter
      (fun (from_state : state) (outgoing_edges : States.t Actions.t) ->
        (* need to update states in edge *)
        Edges.add
          merged_edges
          (match Hashtbl.find_opt map_of_states from_state with
           | None ->
             raise (StateNotFoundInMapOfStates (from_state, map_of_states))
           | Some from_state' -> from_state')
          (Actions.of_seq
             (List.to_seq
                (Actions.fold
                   (fun (action : action)
                     (destinations : States.t)
                     (acc : (action * States.t) list) ->
                     List.append
                       acc
                       [ (match Hashtbl.find_opt map_of_alphabet action with
                          | None ->
                            raise
                              (ActionNotFoundInMapOfAlphabet
                                 (action, map_of_alphabet))
                          | Some new_action ->
                            ( new_action
                            , States.map
                                (fun (old_state : state) ->
                                  match
                                    Hashtbl.find_opt map_of_states old_state
                                  with
                                  | None ->
                                    raise
                                      (StateNotFoundInMapOfStates
                                         (old_state, map_of_states))
                                  | Some new_state -> new_state)
                                destinations ))
                       ])
                   outgoing_edges
                   []))))
      edges
  in
  add_to_merged_edges a;
  add_to_merged_edges b;
  (* return *)
  merged_edges
;;

(** [merge_fsm s t] is the merger of two fsms [s] and [t].
    Note: the states of each are disjointly unioned (by creating new states with new IDs).
    @return
      the merged fsm and a hashtable mapping the new states to the original.
    @param s is an fsm to merge.
    @param t is an fsm to merge.
    @raise StateNotFoundInMapOfStates
      if when updating the edges, the new state cannot be found (likely due to the sets of states not merging correctly). *)
let merge_fsm (s : fsm) (t : fsm) : fsm * (state, state) Hashtbl.t =
  (* *)
  let merged_alphabet, map_of_alphabet =
    map_merge_alphabet s.alphabet t.alphabet
  in
  let merged_states, map_of_merged_states, map_of_original_states =
    map_merge_states s.states t.states
  in
  let merged_edges =
    map_merge_edges s.edges t.edges map_of_alphabet map_of_merged_states
  in
  (* return a new fsm *)
  ( make_fsm
      (make_state ~pp:"<merged>" (-1))
      merged_alphabet
      merged_states
      merged_edges
  , map_of_original_states )
;;
