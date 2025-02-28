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
  ; is_tau : bool
  ; mutable annotation : (state * action) list
  }

let tau : action = { id = 0; label = "~"; is_tau = true; annotation = [] }

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

module PStr = struct
  open Utils.Logging
  open Utils.Formatting
  open Utils

  (********************************************)
  (****** States ******************************)
  (********************************************)

  let state ?(params : Params.pstr = Fmt (Params.Default.fmt ())) (s : state)
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
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
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
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
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

  let action ?(params : Params.pstr = Fmt (Params.Default.fmt ())) (a : action)
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
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
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
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
    ((from, a, destination) : state * action * state)
    : string
    =
    let _params : Params.fmt = Params.handle params in
    let _params' : Params.fmt = no_tab _params
    and tabs : string = str_tabs _params.tabs in
    Printf.sprintf
      "%s{ %s ---%s--> %s } %s"
      (if _params.no_leading_tab then "" else tabs)
      (state ~params:(Fmt _params') from)
      (action ~params:(Fmt _params') a)
      (state ~params:(Fmt _params') destination)
      (if a.is_tau
       then (
         match a.annotation with
         | [] -> "anno: [] (empty)"
         | (h_s, h_a) :: annotation ->
           Printf.sprintf
             "anno: [%s]"
             (List.fold_left
                (fun (acc : string) ((t_s, t_a) : state * action) ->
                  Printf.sprintf
                    "%s; (%s, %s)"
                    acc
                    (state ~params:(Fmt _params') t_s)
                    (action ~params:(Fmt _params') t_a))
                (Printf.sprintf
                   "%s, %s"
                   (state ~params:(Fmt _params') h_s)
                   (action ~params:(Fmt _params') h_a))
                annotation))
       else "")
  ;;

  let actions
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
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
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
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

  let fsm ?(params : Params.pstr = Fmt (Params.Default.fmt ())) (fsm' : fsm)
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
(****** Create *******************************************************)
(*********************************************************************)

module Create = struct
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
      @param ?label
        is a pretty-printed representation, which defaults to [s{id}].
      @param id is the unique identifier for the state. *)
  let action
    ?(is_tau : bool option)
    ?(annotation : (state * action) list option)
    (params : action_param)
    : action
    =
    let is_tau : bool =
      match is_tau with
      | None -> false
      | Some t -> t
    in
    let annotation : (state * action) list =
      match annotation with
      | None -> []
      | Some anno -> anno
    in
    match params with
    | Of (id, label) -> { id; label; is_tau; annotation }
    | From id -> { id; label = label id; is_tau; annotation }
  ;;

  type alphabet_param =
    | ()
    | From of action list

  (**  *)
  let alphabet (params : alphabet_param) : Alphabet.t =
    match params with
    | () -> Alphabet.empty
    | From actions -> Alphabet.of_list actions
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

(*********************************************************************)
(****** Clone ********************************************************)
(*********************************************************************)
module Clone = struct
  let action (a : action) : action =
    match a with
    | { id; label; is_tau; annotation } ->
      Create.action ~is_tau ~annotation (Of (id, label))
  ;;

  let fsm (m : fsm) : fsm =
    match m with
    | { init; alphabet; states; edges } -> Create.fsm init alphabet states edges
  ;;
end

(*********************************************************************)
(****** New (state/action) in FSM ************************************)
(*********************************************************************)

module New = struct
  let state (name : string) (fsm : fsm) : state =
    let filtered : States.t =
      States.filter (fun (s : state) -> s.name == name) fsm.states
    in
    match States.cardinal filtered with
    | 0 ->
      (* create new and add *)
      let s : state = Create.state (Of (States.cardinal fsm.states, name)) in
      fsm.states <- States.add s fsm.states;
      s
    | _ ->
      (* return existing state *)
      assert (States.cardinal filtered == 1);
      List.nth (States.elements filtered) 0
  ;;

  let action
    ?(is_tau : bool option)
    ?(annotation : (state * action) list option)
    (label : string)
    (fsm : fsm)
    : action
    =
    let is_tau : bool =
      match is_tau with
      | None -> false
      | Some t -> t
    in
    let annotation : (state * action) list =
      match annotation with
      | None -> []
      | Some anno -> anno
    in
    match
      Alphabet.find_opt { id = -1; label; is_tau; annotation } fsm.alphabet
    with
    | None ->
      (* create new and add *)
      let a : action =
        Create.action ~is_tau (Of (Alphabet.cardinal fsm.alphabet, label))
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
        Create.actions (Singleton (a, destination))
      in
      Edges.add fsm.edges from actions
    | Some actions' ->
      (* append to existing *)
      action actions' (a, destination)
  ;;
end

(*********************************************************************)
(****** Filter *******************************************************)
(*********************************************************************)

module Filter = struct
  type kind_action_filter =
    | Matches of action
    | Label of string
    | To of state
    | IsSilent

  type kind_state_filter =
    | State of state
    | Action of kind_action_filter

  type kind_edge_filter =
    | From of state
    | FromAny of States.t
    | Action of kind_action_filter

  type has_states =
    | FSM of fsm
    | States of States.t

  (*  type has_actions =
      | FSM of fsm
      | Actions of States.t Actions.t
      | Edges of States.t Actions.t Edges.t *)

  type has_edges =
    | FSM of fsm
    | Edges of States.t Actions.t Edges.t

  exception
    CannotFilterStatesUsingActionWithoutFSM of (States.t * kind_action_filter)

  let filter_states (states : States.t) (kind : kind_state_filter) : States.t =
    match kind with
    | Action a -> raise (CannotFilterStatesUsingActionWithoutFSM (states, a))
    | State to_match ->
      States.filter
        (fun (s : state) ->
          (* will always use the same compare as [States.t] *)
          States.equal (States.singleton s) (States.singleton to_match))
        states
  ;;

  let filter_actions (actions : States.t Actions.t) (kind : kind_action_filter)
    : States.t Actions.t
    =
    let res : States.t Actions.t = Create.actions () in
    Actions.iter
      (fun (a : action) (destinations : States.t) ->
        match kind with
        | Label l -> if l == a.label then Actions.add res a destinations
        | Matches b -> if b == a then Actions.add res a destinations
        | IsSilent -> if a.is_tau then Actions.add res a destinations
        | To s ->
          let res' : States.t = filter_states destinations (State s) in
          if States.is_empty res' == false then Actions.add res a res')
      actions;
    res
  ;;

  let filter_edges
    (edges : States.t Actions.t Edges.t)
    (kind : kind_edge_filter)
    : States.t Actions.t Edges.t
    =
    let res : States.t Actions.t Edges.t = Create.edges 0 in
    Edges.iter
      (fun (from : state) (a's : States.t Actions.t) ->
        match kind with
        | From s -> if from == s then Edges.add res s a's
        | FromAny states ->
          States.iter
            (fun (s : state) -> if from == s then Edges.add res s a's)
            states
        | Action kind' ->
          let res_actions : States.t Actions.t = filter_actions a's kind' in
          if Actions.length res_actions > 0 then Edges.add res from res_actions)
      edges;
    res
  ;;

  let actions
    (actions_to_filter : States.t Actions.t)
    (kind : kind_action_filter)
    : States.t Actions.t
    =
    filter_actions actions_to_filter kind
  ;;

  let edges (edges_to_filter : has_edges) (kind : kind_edge_filter)
    : States.t Actions.t Edges.t
    =
    match edges_to_filter with
    | FSM m -> filter_edges m.edges kind
    | Edges e -> filter_edges e kind
  ;;

  let states (states_to_filter : has_states) (kind : kind_state_filter)
    : States.t
    =
    match states_to_filter with
    | FSM m ->
      (match kind with
       | Action a ->
         States.of_seq (Edges.to_seq_keys (edges (FSM m) (Action a)))
       | _ -> filter_states m.states kind)
    | States states -> filter_states states kind
  ;;
end

(*********************************************************************)
(****** Get **********************************************************)
(*********************************************************************)

module Get = struct
  (********************************************)
  (****** States ******************************)
  (********************************************)

  (********************************************)
  (****** Alphabet ****************************)
  (********************************************)

  (********************************************)
  (****** Actions *****************************)
  (********************************************)

  (** [actions_from from edges] is a shorthand for [Edges.find_opt] and in the case of [None] returns an empty map of actions. *)
  let actions_from (from : state) (edges : States.t Actions.t Edges.t)
    : States.t Actions.t
    =
    match Edges.find_opt edges from with
    | None -> Create.actions ()
    | Some actions -> actions
  ;;

  (**  *)
  let silent_actions (actions : States.t Actions.t) : States.t Actions.t =
    Filter.actions actions IsSilent
  ;;

  (********************************************)
  (****** Edges *******************************)
  (********************************************)

  (** *)
  let silent_edges (edges : States.t Actions.t Edges.t)
    : States.t Actions.t Edges.t
    =
    Filter.edges (Edges edges) (Action IsSilent)
  ;;

  (** [edges_of a edges] filters [edges] by action [a]. *)
  let edges_of (a : action) (edges : States.t Actions.t Edges.t)
    : States.t Actions.t Edges.t
    =
    Filter.edges (Edges edges) (Action (Matches a))
  ;;

  let from_states (edges : States.t Actions.t Edges.t) : States.t =
    States.of_seq (Edges.to_seq_keys edges)
  ;;

  type has_silent_states =
    | Edges of States.t Actions.t Edges.t
    | FSM of fsm

  let silent_states (silent_states : has_silent_states) : States.t =
    match silent_states with
    | FSM m -> Filter.states (FSM m) (Action IsSilent)
    | Edges e -> from_states (Filter.edges (Edges e) (Action IsSilent))
  ;;

  (** [has_destinations] is a type denoting either maps of edges or actions, both of which have destination states. *)
  type has_destinations =
    | Actions of States.t Actions.t
    | Edges of States.t Actions.t Edges.t

  (** [destinations edges] is the set of destination states at the end of each [edges].
      @return the set of detination states of [edges].
      @param edges is either a map of actions or edges. *)
  let rec destinations (edges : has_destinations) : States.t =
    match edges with
    | Actions es ->
      Actions.fold
        (fun (_a : action) (dests : States.t) (acc : States.t) ->
          States.union acc dests)
        es
        States.empty
    | Edges es ->
      Edges.fold
        (fun (_from_state : state)
          (action : States.t Actions.t)
          (acc : States.t) -> States.union acc (destinations (Actions action)))
        es
        States.empty
  ;;
end

(*********************************************************************)
(****** Merge ********************************************************)
(*********************************************************************)

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
        let from' : state = Create.state (From (from, state_id_offset))
        and actions' : States.t Actions.t =
          Actions.length actions |> Actions.create
        in
        Actions.iter
          (fun (a : action) (destinations : States.t) ->
            Actions.add
              actions'
              (Alphabet.find a merged_alphabet)
              (Create.states (From (destinations, state_id_offset))))
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
            let s' : state = Create.state (From (s, state_id_offset)) in
            Hashtbl.add to_merge_state_map s' s;
            States.add s' acc)
          to_merge.states
          states
      in
      let merged_edges : States.t Actions.t Edges.t =
        edges ~params (state_id_offset, merged_alphabet) edges' to_merge.edges
      in
      ( Create.fsm None merged_alphabet merged_states merged_edges
      , to_merge_state_map )
  ;;
end

(*********************************************************************)
(****** Saturate *****************************************************)
(*********************************************************************)

module Saturate = struct
  open Utils

  (** [collected_annotated_actions ?params visited destinations m] ...
      @param visited
        is the trace of states reached via silent actions used for annotation. *)
  let rec collect_annotated_actions
    ?(params : Params.log = Params.Default.log ~mode:(Coq ()) ())
    (visited : States.t)
    (annotation : (state * action) list)
    (to_visit : States.t)
    (m : fsm)
    : States.t Actions.t
    =
    let collection : States.t Actions.t = Create.actions () in
    States.iter
      (fun (destination : state) ->
        match Edges.find_opt m.edges destination with
        | None -> ()
        | Some destination_actions ->
          if States.mem destination visited == false
          then
            Actions.iter
              (fun (a : action) (destinations : States.t) ->
                if a.is_tau
                then (
                  let annotated_actions : States.t Actions.t =
                    collect_annotated_actions
                      ~params
                      (States.add destination visited)
                      (List.append annotation [ destination, a ])
                      (States.union to_visit destinations)
                      m
                  in
                  Actions.add_seq collection (Actions.to_seq annotated_actions))
                else (
                  let a' : action =
                    Create.action
                      ?is_tau:(Some true)
                      ?annotation:
                        (Some (List.append annotation [ destination, a ]))
                      (Of (Alphabet.cardinal m.alphabet, tau.label))
                  in
                  m.alphabet <- Alphabet.add a' m.alphabet;
                  Actions.add collection a' destinations))
              destination_actions)
      to_visit;
    collection
  ;;

  let fsm
    ?(params : Params.log = Params.Default.log ~mode:(Coq ()) ())
    (to_saturate : fsm)
    : fsm
    =
    let m : fsm = Clone.fsm to_saturate in
    Logging.log
      ~params
      (Printf.sprintf
         "Fsm.Saturate.fsm, silent states: %s."
         (PStr.states
            ~params:(Log params)
            (Get.from_states (Filter.edges (Edges m.edges) (Action IsSilent)))));
    (* for each outgoing edge from a state *)
    Edges.iter
      (fun (from : state) (a's : States.t Actions.t) ->
        Actions.iter
          (fun (a : action) (destinations : States.t) ->
            (* if it is silent,
               - then collect all of the destination states transitions,
               - and if those transitions are also silent, continue recursively *)
            if a.is_tau
            then (
              let annotated_actions : States.t Actions.t =
                collect_annotated_actions
                  ~params
                  (States.singleton from)
                  [ from, a ]
                  destinations
                  m
              in
              Actions.add_seq a's (Actions.to_seq annotated_actions)))
          a's)
      m.edges;
    m
  ;;
end
