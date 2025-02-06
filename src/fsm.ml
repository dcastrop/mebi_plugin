(*********************************************************************)
(****** States *******************************************************)
(*********************************************************************)

(** [state] is a 2-tuple with a unique [id] and (non-unique) [name].
    [id] is an integer for identifying the state.
    [name] is a pretty-printed string of something corresponding to this state. *)
type state =
  { id : int (* ; hash : int *)
  ; name : string
  }

(** [States] is a set of [states]. *)
module States = Set.Make (struct
    type t = state

    let compare a b = compare a.id b.id
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

(** [Edges] map [states] to outgoing actions, mapped to sets of destination [states]. *)
module Edges = Hashtbl.Make (struct
    type t = state

    let equal (t1 : state) (t2 : state) = Int.equal t1.id t2.id
    let hash (t : state) = Hashtbl.hash t

    (* let add (tbl) (k:t) (v:States.t Actions.t) :unit = match Hashtbl.find_opt tbl k with
    | None -> Hashtbl.add tbl k v
    | Some v' -> Hashtbl.replace tbl k (Actions.add v') *)
  end)

(*********************************************************************)
(****** FSM **********************************************************)
(*********************************************************************)

(** [fsm] is a type used to describe an FSM in OCaml.
    - [init] is the initial state.
    - [alphabet] is the set of labels of actions of edges.
    - [states] is a set of states.
    - [edges] is a hashtable mapping states to outgoing actions and their detination states. *)
type fsm =
  { init : state option
  ; mutable alphabet : Alphabet.t
  ; mutable states : States.t
  ; mutable edges : States.t Actions.t Edges.t
  }
(* TODO: Currently, there may be many copies of the same state in an [fsm] (i.e., in [init] and the [edges]). Maybe add list of states and change others to be an index referencing their position in the list. *)

(*********************************************************************)
(****** Pretty-Printing **********************************************)
(*********************************************************************)

module Make = struct
  (********************************************)
  (****** States ******************************)
  (********************************************)

  type state_params =
    | From of (state * int)
    | Of of (int * string)

  (** [state ?pp id] is a wrapper constructor for [state].
    @param ?pp is a pretty-printed respresentation, which defaults to [s{id}].
    @param id is the unique identifier for the state. *)
  let state (params : state_params) : state =
    let id, name =
      match params with
      | From (s, offset) -> s.id + offset, s.name
      | Of (id, name) -> id, name
    in
    { id; name }
  ;;

  type states_params =
    | From of (States.t * int)
    | ()

  let states (params : states_params) : States.t =
    match params with
    | From (states, offset) ->
      States.map (fun (s : state) -> state (From (s, offset))) states
    | () -> States.empty
  ;;

  (********************************************)
  (****** Alphabet, Actions & Edges ***********)
  (********************************************)

  let label (id : int) : string = Printf.sprintf "l%d" id

  type action_param =
    | Of of (int * string)
    | From of int

  (** [action ?label id] is a wrapper constructor for [action].
    @param ?label is a pretty-printed representation, which defaults to [s{id}].
    @param id is the unique identifier for the state. *)
  let action (params : action_param) : action =
    match params with
    | Of (id, label) -> { id; label }
    | From id -> { id; label = label id }
  ;;

  type actions_params =
    | New of (action * States.t)
    | Singleton of (action * state)
    | ()

  let actions (params : actions_params) : States.t Actions.t =
    match params with
    | New (a, destinations) -> Actions.of_seq (List.to_seq [ a, destinations ])
    | Singleton (a, destination) ->
      Actions.of_seq (List.to_seq [ a, States.singleton destination ])
    | () -> Actions.create 0
  ;;

  (** [edges ?size] is a wrapper constructor for [Actions]. *)
  let edges ?(size : int = 0) unit : States.t Actions.t Edges.t =
    Edges.create size
  ;;

  (********************************************)
  (****** FSM *********************************)
  (********************************************)

  (** [fsm init alphabet states edges] is a wrapper constructor for [fsm]. *)
  let fsm
        (init : state option)
        (alphabet : Alphabet.t)
        (states : States.t)
        (edges : States.t Actions.t Edges.t)
    : fsm
    =
    { init; alphabet; states; edges }
  ;;
end

module New = struct
  let state (name : string) (fsm : fsm) : state =
    let filtered : States.t =
      States.filter (fun (s : state) -> s.name == name) fsm.states
    in
    match States.cardinal filtered with
    | 0 ->
      (* create new and add *)
      let s : state = Make.state (Of (States.cardinal fsm.states, name)) in
      fsm.states <- States.add s fsm.states;
      s
    | _ ->
      (* return existing state *)
      assert (States.cardinal filtered == 1);
      List.nth (States.elements filtered) 0
  ;;

  let action (label : string) (fsm : fsm) : action =
    match Alphabet.find_opt { id = -1; label } fsm.alphabet with
    | None ->
      (* create new and add *)
      let a : action =
        Make.action (Of (Alphabet.cardinal fsm.alphabet, label))
      in
      fsm.alphabet <- Alphabet.add a fsm.alphabet;
      a
    | Some a -> a (* return existing action *)
  ;;
end

(*********************************************************************)
(****** Append FSM ***************************************************)
(*********************************************************************)

module Append = struct
  let alphabet (fsm : fsm) (a : action) : unit =
    fsm.alphabet <- Alphabet.add a fsm.alphabet
  ;;

  let state ?(skip_duplicate_names : bool = true) (fsm : fsm) (s : state) : unit
    =
    fsm.states
    <- (if skip_duplicate_names
        then
          if States.exists (fun (s' : state) -> s'.name == s.name) fsm.states
          then fsm.states
          else States.add s fsm.states
        else States.add s fsm.states)
  ;;

  let states
        ?(skip_duplicate_names : bool = true)
        (fsm : fsm)
        (states : States.t)
    : unit
    =
    States.iter (fun (s : state) -> state ~skip_duplicate_names fsm s) states
  ;;

  let action (actions : States.t Actions.t) ((a, destination) : action * state)
    : unit
    =
    match Actions.find_opt actions a with
    | None ->
      (* add as new *)
      Actions.add actions a (States.singleton destination)
    | Some destinations ->
      (* append to existing destinations *)
      Actions.replace actions a (States.add destination destinations)
  ;;

  let edge (fsm : fsm) ((from, a, destination) : state * action * state) : unit =
    match Edges.find_opt fsm.edges from with
    | None ->
      (* add as new *)
      let actions : States.t Actions.t =
        Make.actions (Singleton (a, destination))
      in
      Edges.add fsm.edges from actions
    | Some actions' ->
      (* append to existing *)
      action actions' (a, destination)
  ;;
end

(********************************************)
(****** Merging *****************************)
(********************************************)

module Merge = struct
  open Utils

  let edges
        ?(params : Params.log = Params.Default.log ~mode:(Coq ()) ())
        ((state_id_offset, merged_alphabet) : int * Alphabet.t)
        (base : States.t Actions.t Edges.t)
        (to_merge : States.t Actions.t Edges.t)
    : States.t Actions.t Edges.t
    =
    Edges.iter
      (fun (from : state) (actions : States.t Actions.t) ->
         let from' : state = Make.state (From (from, state_id_offset))
         and actions' : States.t Actions.t =
           Actions.length actions |> Actions.create
         in
         Actions.iter
           (fun (a : action) (destinations : States.t) ->
              Actions.add
                actions'
                (Alphabet.find a merged_alphabet)
                (Make.states (From (destinations, state_id_offset))))
           actions;
         Edges.add base from' actions')
      to_merge;
    base
  ;;

  let fsms
        ?(params : Params.log = Params.Default.log ~mode:(Coq ()) ())
        (base : fsm)
        (to_merge : fsm)
    : fsm * (state, state) Hashtbl.t
    =
    (* merge into [base] the fsm [to_merge] *)
    match base with
    | { init; alphabet; states; edges = edges'; _ } ->
      (* needed to keep track of [to_merge.states] new IDs *)
      let state_id_offset : int = States.cardinal states
      and to_merge_state_map : (state, state) Hashtbl.t =
        States.cardinal to_merge.states |> Hashtbl.create
      in
      (* *)
      let merged_alphabet : Alphabet.t =
        Alphabet.union alphabet to_merge.alphabet
      and merged_states : States.t =
        States.fold
          (fun (s : state) (acc : States.t) ->
             let s' : state = Make.state (From (s, state_id_offset)) in
             Hashtbl.add to_merge_state_map s' s;
             States.add s' acc)
          to_merge.states
          states
      in
      let merged_edges : States.t Actions.t Edges.t =
        edges ~params (state_id_offset, merged_alphabet) edges' to_merge.edges
      in
      ( Make.fsm None merged_alphabet merged_states merged_edges
      , to_merge_state_map )
  ;;
end

(*********************************************************************)
(****** Pretty-Printing **********************************************)
(*********************************************************************)

module PStr = struct
  open Utils.Logging
  open Utils.Formatting
  open Utils

  let inc_tab ?(by : int = 1) (params : Params.fmt) : Params.fmt =
    { tabs = params.tabs + by
    ; no_leading_tab =
        params.no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  let dec_tab ?(by : int = 1) (params : Params.fmt) : Params.fmt =
    { tabs = (if params.tabs - by < 0 then 0 else params.tabs + by)
    ; no_leading_tab =
        params.no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  let no_tab (params : Params.fmt) : Params.fmt =
    { tabs = 0
    ; no_leading_tab =
        params.no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  let no_leading_tab (_no_leading_tab : bool) (params : Params.fmt) : Params.fmt
    =
    { tabs = params.tabs
    ; no_leading_tab =
        _no_leading_tab (* ; non_repetative = params.non_repetative *)
    ; params = params.params
    }
  ;;

  (********************************************)
  (****** States ******************************)
  (********************************************)

  let state ?(params : pstr_params = Fmt (Params.Default.fmt ())) (s : state)
    : string
    =
    let _params : Params.fmt = Params.handle params in
    let tabs : string = str_tabs _params.tabs in
    Printf.sprintf
      "%s%s"
      (if _params.no_leading_tab then "" else tabs)
      (let normal_pstr : string = Printf.sprintf "(%s)" s.name
       and detail_pstr : string = Printf.sprintf "(%s | id:%d)" s.name s.id in
       match _params.params.kind with
       | Normal () -> normal_pstr
       | Details () -> detail_pstr
       | Debug () -> detail_pstr
       | Warning () -> detail_pstr)
  ;;

  let states
        ?(params : pstr_params = Fmt (Params.Default.fmt ()))
        (states : States.t)
    : string
    =
    if States.is_empty states
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : Params.fmt = Params.handle params in
      let _params' : Params.fmt = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (States.fold
           (fun (s : state) (acc : string) ->
              Printf.sprintf
                "%s%s\n"
                acc
                (state ~params:(Fmt (no_leading_tab false _params')) s))
           states
           "\n")
        tabs)
  ;;

  let partition
        ?(params : pstr_params = Fmt (Params.Default.fmt ()))
        (partition : Partition.t)
    : string
    =
    if Partition.is_empty partition
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : Params.fmt = Params.handle params in
      let _params' : Params.fmt = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (Partition.fold
           (fun (states' : States.t) (acc : string) ->
              Printf.sprintf
                "%s%s\n"
                acc
                (states ~params:(Fmt (no_leading_tab false _params')) states'))
           partition
           "\n")
        tabs)
  ;;

  (********************************************)
  (****** Alphabet, Actions & Edges ***********)
  (********************************************)

  let action ?(params : pstr_params = Fmt (Params.Default.fmt ())) (a : action)
    : string
    =
    let _params : Params.fmt = Params.handle params in
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
        ?(params : pstr_params = Fmt (Params.Default.fmt ()))
        (actions : Alphabet.t)
    : string
    =
    if Alphabet.is_empty actions
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : Params.fmt = Params.handle params in
      let _params' : Params.fmt = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (Alphabet.fold
           (fun (a : action) (acc : string) ->
              Printf.sprintf
                "%s%s\n"
                acc
                (action ~params:(Fmt (no_leading_tab false _params')) a))
           actions
           "\n")
        tabs)
  ;;

  let edge
        ?(params : pstr_params = Fmt (Params.Default.fmt ()))
        ((from, a, destination) : state * action * state)
    : string
    =
    let _params : Params.fmt = Params.handle params in
    let _params' : Params.fmt = no_tab _params
    and tabs : string = str_tabs _params.tabs in
    Printf.sprintf
      "%s{ %s ---%s--> %s }"
      (if _params.no_leading_tab then "" else tabs)
      (state ~params:(Fmt _params') from)
      (action ~params:(Fmt _params') a)
      (state ~params:(Fmt _params') destination)
  ;;

  let actions
        ?(params : pstr_params = Fmt (Params.Default.fmt ()))
        ?(from : state option)
        (actions' : States.t Actions.t)
    : string
    =
    if Actions.length actions' == 0
    then "{ } (empty)"
    else (
      (* increment tab of inner elements of table *)
      let _params : Params.fmt = Params.handle params in
      let tabs : string = str_tabs _params.tabs in
      match from with
      (* if no from state, then this is a non-repetative representation *)
      | None ->
        (* should not be leading next *)
        let _params' : Params.fmt = no_leading_tab true (inc_tab _params) in
        Printf.sprintf
          "%s{[%s]}"
          (if _params.no_leading_tab then "" else tabs)
          (Actions.fold
             (fun (a : action) (destinations : States.t) (acc : string) ->
                Printf.sprintf
                  "%s{ --%s--> %s }\n"
                  acc
                  (action ~params:(Fmt _params') a)
                  (states ~params:(Fmt _params') destinations))
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
                          ~params:(Fmt (no_leading_tab false _params))
                          (from_state, a, destination)))
                  destinations
                  ""))
          actions'
          "")
  ;;

  let edges
        ?(params : pstr_params = Fmt (Params.Default.fmt ()))
        (edges' : States.t Actions.t Edges.t)
    : string
    =
    if Edges.length edges' == 0
    then "{ } (empty)"
    else (
      (* increment tab of inner elements of table *)
      let _params : Params.fmt = Params.handle params in
      let _params' : Params.fmt = inc_tab _params
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
                   ~params:(Fmt (no_leading_tab false _params'))
                   ?from:(Some from_state)
                   actions'))
           edges'
           "\n")
        tabs)
  ;;

  (********************************************)
  (****** FSM *********************************)
  (********************************************)

  let fsm ?(params : pstr_params = Fmt (Params.Default.fmt ())) (fsm' : fsm)
    : string
    =
    (* increment tab of inner elements of fsm *)
    let _params : Params.fmt = Params.handle params in
    let _params' : Params.fmt = inc_tab ~by:2 _params
    and tabs : string = str_tabs _params.tabs
    and tabs' : string = str_tabs (_params.tabs + 1) in
    Printf.sprintf
      "{ %s; %s; %s; %s; \n%s}"
      (Printf.sprintf
         "\n%sinitial state: %s"
         tabs'
         (match fsm'.init with
          | None -> "None"
          | Some init' -> state ~params:(Fmt _params') init'))
      (Printf.sprintf
         "\n%salphabet: %s"
         tabs'
         (alphabet ~params:(Fmt _params') fsm'.alphabet))
      (Printf.sprintf
         "\n%sstates: %s"
         tabs'
         (states ~params:(Fmt _params') fsm'.states))
      (Printf.sprintf
         "\n%sedges: %s"
         tabs'
         (edges ~params:(Fmt _params') fsm'.edges))
      tabs
  ;;
end

(*********************************************************************)
(****** Getter Functions *********************************************)
(*********************************************************************)

(********************************************)
(****** States ******************************)
(********************************************)

(********************************************)
(****** Alphabet ****************************)
(********************************************)

(********************************************)
(****** Actions *****************************)
(********************************************)

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
  | None -> Make.actions ()
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
      (fun (_from_state : state)
        (action : States.t Actions.t)
        (acc : States.t) ->
         States.union acc (get_all_destinations (Actions action)))
      es
      States.empty
;;
