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
    [?pp] is a pretty-printed respresentation, which defaults to [s{id}].
    [id] is the unique identifier for the state. *)
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
  end)

(*********************************************************************)
(****** Action Labels & Alphabet *************************************)
(*********************************************************************)

(** [action] is a 2-tuple with a unique [id] and (non-unique) [label]).
    [id] is an integer for identifying the action.
    [label] is a (pretty-printed) string describing the action. *)
type action =
  { id : int
  ; label : string
  }

(** [make_action ?label id] is a wrapper constructor for [action].
    [?label] is a pretty-printed representation, which defaults to [s{id}].
    [id] is the unique identifier for the state. *)
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

    let compare a b = compare a.id b.id
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

(*********************************************************************)
(****** FSM **********************************************************)
(*********************************************************************)

(** [fsm] is a type used to describe an FSM in OCaml.
    [init] is the initial state.
    [alphabet] is the set of labels of actions of edges.
    [states] is a set of states.
    [edges] is a hashtable mapping states to outgoing actions and their detination states. *)
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

(*********************************************************************)
(****** Pretty-Printing **********************************************)
(*********************************************************************)

(********************************************)
(****** States ******************************)
(********************************************)

(** [pp_axiom] denotes the finest granularity that [pp_str] applies to. *)
type pp_axiom =
  | State of state
  | Action of action
  | Edge of (state * action * state)
  | OutgoingEdge of (action * state)

type pp_list =
  | States of States.t
  | Alphabet of Alphabet.t

type pp_map =
  | Actions of States.t Actions.t
  | Edges of States.t Actions.t Edges.t

type pp_collection =
  | List of pp_list
  | Map of pp_map

type pp_utils_fsm =
  | Init of fsm
  | Alphabet of fsm
  | States of fsm
  | Edges of fsm

type pp_utils = Fsm of pp_utils_fsm

type pp_supported =
  | Axiom of pp_axiom
  | Collection of pp_collection
  | Utils of pp_utils
  | Fsm of fsm

type pp_wrappable =
  | State of state
  | Action of action
  | Edge of (state * action * state)
  | OutgoingEdge of (action * state)
  | States of States.t
  | Alphabet of Alphabet.t
  | Actions of States.t Actions.t
  | Edges of States.t Actions.t Edges.t
  | Fsm of fsm

(** [pp_options] denotes the types that specifically support the [pp] flag. *)
type pp_options =
  | None of unit
  | Debug of unit

(** [pp_collection_is_empty c] returns true if [c] is empty. *)
let pp_collection_is_empty (c : pp_collection) : bool =
  match c with
  | List l ->
    (match l with
     | States s -> States.is_empty s
     | Alphabet a -> Alphabet.is_empty a)
  | Map m ->
    (match m with
     | Actions a -> Actions.length a == 0
     | Edges e -> Edges.length e == 0)
;;

(** [] helper wrapper function for [pp_supported]. *)
let pp_wrap_as_supported (to_wrap : pp_wrappable) : pp_supported =
  match to_wrap with
  | State to_wrap -> Axiom (State to_wrap)
  | Action to_wrap -> Axiom (Action to_wrap)
  | Edge to_wrap -> Axiom (Edge to_wrap)
  | OutgoingEdge to_wrap -> Axiom (OutgoingEdge to_wrap)
  | States to_wrap -> Collection (List (States to_wrap))
  | Alphabet to_wrap -> Collection (List (Alphabet to_wrap))
  | Actions to_wrap -> Collection (Map (Actions to_wrap))
  | Edges to_wrap -> Collection (Map (Edges to_wrap))
  | Fsm to_wrap -> Fsm to_wrap
;;

(** [pstr] *)
let rec pstr
  ?(tabs : int = 0)
  ?(options : pp_options = None ())
  (to_str : pp_supported)
  : string
  =
  let basedent = Mebi_utils.str_tabs (tabs - 1) in
  let indent = Mebi_utils.str_tabs tabs in
  (* build string *)
  match to_str with
  (*  *)
  | Axiom to_str ->
    (match to_str with
     (* [state] *)
     | State to_str ->
       Printf.sprintf
         "(%s)"
         (match options with
          | None () -> Printf.sprintf "%s" to_str.pp
          | Debug () -> Printf.sprintf "%s <id: %d>" to_str.pp to_str.id)
     (* [action] *)
     | Action to_str ->
       Printf.sprintf
         "{| %s |}"
         (match options with
          | None () -> Printf.sprintf "%s" to_str.label
          | Debug () -> Printf.sprintf "%s <id: %d>" to_str.label to_str.id)
     (* edge of [state * action * state] *)
     | OutgoingEdge to_str ->
       (match to_str with
        | a, destination ->
          Printf.sprintf
            "---%s--> %s"
            (pstr ~options (pp_wrap_as_supported (Action a)))
            (pstr ~options (pp_wrap_as_supported (State destination))))
     (* edge of [state * action * state] *)
     | Edge to_str ->
       (match to_str with
        | from, a, destination ->
          Printf.sprintf
            "{ %s %s }"
            (pstr ~options (pp_wrap_as_supported (State from)))
            (pstr
               ~options
               (pp_wrap_as_supported (OutgoingEdge (a, destination))))))
  (*  *)
  | Collection to_str ->
    (match to_str with
     (*  *)
     | List to_str ->
       if pp_collection_is_empty (List to_str)
       then "[ ] (empty)"
       else
         Printf.sprintf
           "[%s%s]"
           (match to_str with
            (* [States] *)
            | States to_str ->
              States.fold
                (fun (s : state) (acc : string) ->
                  Printf.sprintf
                    "%s%s\n"
                    acc
                    (pstr ~tabs:(tabs + 1) (pp_wrap_as_supported (State s))))
                to_str
                "\n"
            (* [Alphabet] *)
            | Alphabet to_str ->
              Alphabet.fold
                (fun (a : action) (acc : string) ->
                  Printf.sprintf
                    "%s%s\n"
                    acc
                    (pstr ~tabs:(tabs + 1) (pp_wrap_as_supported (Action a))))
                to_str
                "\n")
           basedent
     (*  *)
     | Map to_str ->
       if pp_collection_is_empty (Map to_str)
       then "{ } (empty)"
       else
         Printf.sprintf
           "{%s%s}"
           (match to_str with
            (* [Actions] *)
            | Actions to_str ->
              Actions.fold
                (fun (a : action) (destinations : States.t) (acc : string) ->
                  (* for each destination *)
                  Printf.sprintf
                    "%s%s"
                    acc
                    (States.fold
                       (fun (destination : state) (acc' : string) ->
                         Printf.sprintf
                           "%s%s%s"
                           acc'
                           indent
                           (pstr
                              ~options
                              (pp_wrap_as_supported
                                 (OutgoingEdge (a, destination)))))
                       destinations
                       ""))
                to_str
                "\n"
            (* [Edges] *)
            | Edges to_str ->
              Edges.fold
                (fun (from : state)
                  (actions : States.t Actions.t)
                  (acc : string) ->
                  Printf.sprintf
                    "%s%s"
                    acc
                    (Actions.fold
                       (fun (a : action)
                         (destinations : States.t)
                         (acc : string) ->
                         (* for each destination *)
                         Printf.sprintf
                           "%s%s"
                           acc
                           (States.fold
                              (fun (destination : state) (acc' : string) ->
                                Printf.sprintf
                                  "%s%s%s"
                                  acc'
                                  indent
                                  (pstr
                                     ~options
                                     (pp_wrap_as_supported
                                        (Edge (from, a, destination)))))
                              destinations
                              ""))
                       actions
                       "\n"))
                to_str
                "\n")
           basedent)
  (*  *)
  | Utils to_str -> ""
  (*  *)
  | Fsm to_str ->
    Printf.sprintf
      "{init state %s\nalphabet: %s\nstates: %s\nedges: %s}"
      (pstr ~options (pp_wrap_as_supported (State to_str.init)))
      (pstr ~options (pp_wrap_as_supported (Alphabet to_str.alphabet)))
      (pstr ~options (pp_wrap_as_supported (States to_str.states)))
      (pstr ~options (pp_wrap_as_supported (Edges to_str.edges)))
;;

(** [pstr_state ?long state] is a string of [state].
    [?ids] determines if [state.id] is shown.
    [?pp] determins if [state.pp] is shown.
    [?long] overrides the other options, showing everything.
    By default only [state.id] is shown.
    [state] is the state to be printed. *)
let pstr_state
  ?(ids : unit option)
  ?(pp : unit option)
  ?(long : unit option)
  (state : state)
  : string
  =
  match long with
  | None ->
    Printf.sprintf
      "(%s)"
      (match pp with
       (* show [state.id] only *)
       | None -> Printf.sprintf "id: %d" state.id
       (* show [state.pp] *)
       | Some () ->
         Printf.sprintf
           "%s%s"
           state.pp
           (* check if [show.id] too *)
           (match ids with
            | None -> ""
            | Some () -> Printf.sprintf " <id: %d>" state.id))
  (* show everything *)
  | Some () -> Printf.sprintf "(id: %d; pp: %s)" state.id state.pp
;;

(* Printf.sprintf "(id: %d; hash: %d; pp: %s)" state.id state.hash state.pp *)

(** [handle_state_pstr ids pp long s] is a wrapper for [pstr_state] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_state_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (s : state)
  : string
  =
  match long with
  | None ->
    (match pp with
     | None -> pstr_state s
     | Some () ->
       (match ids with
        | None -> pstr_state ~pp:() s
        | Some () -> pstr_state ~ids:() ~pp:() s))
  | Some () -> pstr_state ~long:() s
;;

(** [pstr_states ?long states] is a string of [states].
    [?ids], [?pp] and [?long] are used for individual [states].
    [?indent] determines the level of indentation.
    [states] is the set of states to be printed. *)
let pstr_states
  ?(ids : unit option)
  ?(pp : unit option)
  ?(long : unit option)
  ?(indent : int = 1)
  (states : States.t)
  : string
  =
  if States.is_empty states
  then "[ ] (empty)"
  else
    Printf.sprintf
      "[%s%s]"
      (States.fold
         (fun (s : state) (acc : string) ->
           Printf.sprintf
             "%s%s%s\n"
             acc
             (Mebi_utils.str_tabs indent)
             (* figure out which units to pass on (bit messy) *)
             (handle_state_pstr ids pp long s))
         states
         "\n")
      (Mebi_utils.str_tabs (indent - 1))
;;

(** [pstr_action ?ids ?long action] is a string of [action].
    [?ids] determines if [action.id] is shown.
    [?long] determines if all fields are shown.
    [action] is the action to show. *)
let pstr_action ?(ids : unit option) ?(long : unit option) (action : action)
  : string
  =
  match long with
  | None ->
    Printf.sprintf
      "%s"
      (Printf.sprintf
         "%s%s"
         action.label
         (match ids with
          | None -> ""
          | Some () -> Printf.sprintf " <id: %d>" action.id))
  (* show everything *)
  | Some () -> Printf.sprintf "id: %d; label: %s" action.id action.label
;;

let handle_action_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (a : action)
  : string
  =
  match long with
  | None ->
    (match ids with
     | None -> pstr_action a
     | Some () -> pstr_action ~ids:() a)
  | Some () -> pstr_action ~long:() a
;;

(** [pstr_action_alphabet ?ids ?long action] is a string of [action].
    [?ids] determines if [action.id] is shown.
    [?long] determines if all fields are shown.
    [action] is the action to show. *)
let pstr_action_alphabet
  ?(ids : unit option)
  ?(long : unit option)
  ?(indent : int = 1)
  (actions : Alphabet.t)
  : string
  =
  if Alphabet.is_empty actions
  then "[ ] (empty)"
  else
    Printf.sprintf
      "[%s%s]"
      (Alphabet.fold
         (fun (a : action) (acc : string) ->
           Printf.sprintf
             "%s%s%s\n"
             acc
             (Mebi_utils.str_tabs indent)
             (* figure out which units to pass on (bit messy) *)
             (handle_action_pstr ids None long a))
         actions
         "\n")
      (Mebi_utils.str_tabs (indent - 1))
;;

(** [pstr_edge ?long (from_state, outgoing_edge)] is a string of [edge].
    [?ids], [?pp] and [?long] are used for corresponding states.
    [from_state] is the starting state.
    [outgoing_edge] denotes the edge label [outgoing_edge.label]
    and the destination [outgoing_edge.to_state]. *)
let pstr_edge
  ?(ids : unit option)
  ?(pp : unit option)
  ?(long : unit option)
  ((from_state : state), (action : action), (destination : state))
  : string
  =
  Printf.sprintf
    "%s ---{ %s }--> %s"
    (handle_state_pstr ids pp long from_state)
    (handle_action_pstr ids pp long action)
    (handle_state_pstr ids pp long destination)
;;

(* TODO: merge below with [handle_state_pstr] ? *)
(** [handle_edge_pstr ids pp long e] is a wrapper for [pstr_edge] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_edge_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (e : state * action * state)
  : string
  =
  match long with
  | None ->
    (match pp with
     | None -> pstr_edge e
     | Some () ->
       (match ids with
        | None -> pstr_edge ~pp:() e
        | Some () -> pstr_edge ~ids:() ~pp:() e))
  | Some () -> pstr_edge ~long:() e
;;

let pstr_actions
  ?(ids : unit option)
  ?(pp : unit option)
  ?(long : unit option)
  ?(indent : int = 1)
  ?(from_state : state = { id = -1; pp = ".." })
  (actions : States.t Actions.t)
  : string
  =
  if List.is_empty (List.of_seq (Actions.to_seq_keys actions))
  then "[ ] (empty)"
  else
    Printf.sprintf
      "[%s%s]"
      (Actions.fold
         (fun (action : action) (destinations : States.t) (acc : string) ->
           Printf.sprintf
             "%s%s"
             acc
             (States.fold
                (fun (destination : state) (acc' : string) ->
                  Printf.sprintf
                    "%s%s{ %s }\n"
                    acc'
                    (Mebi_utils.str_tabs indent)
                    (handle_edge_pstr
                       ids
                       pp
                       long
                       (from_state, action, destination)))
                destinations
                ""))
         actions
         "\n")
      (Mebi_utils.str_tabs (indent - 1))
;;

let handle_actions_pstr
  ?(indent : int = 1)
  ?(from_state : state = { id = -1; pp = ".." })
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (a : States.t Actions.t)
  : string
  =
  match long with
  | None ->
    (match ids with
     | None ->
       (match pp with
        | None -> pstr_actions ~indent ~from_state a
        | Some () -> pstr_actions ~pp:() ~indent ~from_state a)
     | Some () ->
       (match pp with
        | None -> pstr_actions ~ids:() ~indent ~from_state a
        | Some () -> pstr_actions ~ids:() ~pp:() ~indent ~from_state a))
  | Some () -> pstr_actions ~long:() ~indent ~from_state a
;;

(* TODO: trying to combine all of the [handle_state_pstr], [handle_edge_pstr], etc. *)
(** [handle_units] denotes the types that [handle_units_pstr] supports. *)
(* type handle_units =
   | State of state
   | States of States.t
   | Edge of fsm_transition
   | Edges of edges *)

(* let handle_units_pstr
   (ids : unit option)
   (pp : unit option)
   (long : unit option)
   (to_handle : handle_units)
   (with_function) : string =

   match long with
   | None ->
   (match pp with
   | None -> with_function to_handle
   | Some () ->
   (match ids with
   | None -> with_function ~pp:() to_handle
   | Some () -> with_function ~ids:() ~pp:() to_handle))
   | Some () -> with_function ~long:() to_handle
   ;; *)

(** [pstr_edges ?long edges] is a string of [edges].
    [?ids], [?pp] and [?long] are used by individual edges.
    [edges] is the list of edges to be printed. *)
let pstr_edges
  ?(ids : unit option)
  ?(pp : unit option)
  ?(long : unit option)
  ?(indent : int = 1)
  (edges : States.t Actions.t Edges.t)
  : string
  =
  if Edges.to_seq_keys edges |> Seq.is_empty
  then "[ ] (empty)"
  else
    Printf.sprintf
      "[%s%s]"
      (Edges.fold
         (fun (from_state : state)
           (outgoing_edges : States.t Actions.t)
           (acc : string) ->
           Printf.sprintf
             "%s%s%s\n"
             acc
             (Mebi_utils.str_tabs indent)
             (handle_actions_pstr
                ~indent:(indent + 1)
                ~from_state
                ids
                pp
                long
                outgoing_edges))
         edges
         "\n")
      (Mebi_utils.str_tabs (indent - 1))
;;

(** [handle_states_pstr ids pp long e] is a wrapper for [pstr_states] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_states_pstr
  ?(indent : int = 1)
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (s : States.t)
  : string
  =
  match long with
  | None ->
    (match pp with
     | None -> pstr_states ~indent s
     | Some () ->
       (match ids with
        | None -> pstr_states ~pp:() ~indent s
        | Some () -> pstr_states ~ids:() ~pp:() ~indent s))
  | Some () -> pstr_states ~long:() ~indent s
;;

(** [handle_pstr_alphabet ids pp long a] is a wrapper for [pstr_states] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_action_alphabet_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (a : Alphabet.t)
  : string
  =
  match long with
  | None ->
    (match ids with
     | None -> pstr_action_alphabet a
     | Some () -> pstr_action_alphabet ~ids:() a)
  | Some () -> pstr_action_alphabet ~long:() a
;;

(** [handle_edges_pstr ids pp long e] is a wrapper for [pstr_edges] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_edges_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (e : States.t Actions.t Edges.t)
  : string
  =
  match long with
  | None ->
    (match pp with
     | None -> pstr_edges e
     | Some () ->
       (match ids with
        | None -> pstr_edges ~pp:() e
        | Some () -> pstr_edges ~ids:() ~pp:() e))
  | Some () -> pstr_edges ~long:() e
;;

(** [pstr_fsm ?long the_fsm] is a string of [the_fsm].
    [?long] is used when printing individual states and edges.
    [the_fsm] is the fsm to be printed. *)
let pstr_fsm
  ?(ids : unit option)
  ?(pp : unit option)
  ?(long : unit option)
  (the_fsm : fsm)
  : string
  =
  Printf.sprintf
    "%s\n%s\n%s\n%s"
    (Printf.sprintf
       "init state: %s"
       (handle_state_pstr ids pp long the_fsm.init))
    (Printf.sprintf
       "states: %s"
       (handle_states_pstr ids pp long the_fsm.states))
    (Printf.sprintf
       "alphabet: %s"
       (handle_action_alphabet_pstr ids pp long the_fsm.alphabet))
    (Printf.sprintf "edges: %s" (handle_edges_pstr ids pp long the_fsm.edges))
;;

(*********************************************************************)
(****** Getter Functions *********************************************)
(*********************************************************************)

(********************************************)
(****** States ******************************)
(********************************************)

exception StateNotFoundWithID of (int * States.t)
exception MultipleStatesFoundWithID of (int * States.t)

(** [] *)
let get_state_by_id (states : States.t) (id : int) : state =
  let filtered = States.filter (fun (s : state) -> s.id == id) states in
  if States.is_empty filtered then raise (StateNotFoundWithID (id, states));
  if States.cardinal filtered > 1
  then raise (MultipleStatesFoundWithID (id, states));
  List.nth (States.elements filtered) 0
;;

exception StateNotFoundWithName of (string * States.t)
exception MultipleStatesFoundWithName of (string * States.t)

(** [] *)
let get_state_by_name (states : States.t) (name : string) : state =
  let filtered = States.filter (fun (s : state) -> s.pp == name) states in
  if States.is_empty filtered then raise (StateNotFoundWithName (name, states));
  if States.cardinal filtered > 1
  then raise (MultipleStatesFoundWithName (name, states));
  List.nth (States.elements filtered) 0
;;

(********************************************)
(****** Alphabet ****************************)
(********************************************)

let get_action_alphabet_from_actions (actions : States.t Actions.t) : Alphabet.t
  =
  Alphabet.of_list (List.of_seq (Actions.to_seq_keys actions))
;;

let get_action_alphabet_from_edges (es : States.t Actions.t Edges.t)
  : Alphabet.t
  =
  Alphabet.of_list
    (Edges.fold
       (fun (from_state : state)
         (actions : States.t Actions.t)
         (acc : action list) ->
         List.append
           acc
           (Actions.fold
              (fun (action : action)
                (destinations : States.t)
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

(** [] *)
let get_action_by_id (alphabet : Alphabet.t) (id : int) : action =
  let filtered = Alphabet.filter (fun (a : action) -> a.id == id) alphabet in
  if Alphabet.is_empty filtered then raise (ActionNotFoundWithID (id, alphabet));
  if Alphabet.cardinal filtered > 1
  then raise (MultipleActionsFoundWithID (id, alphabet));
  List.nth (Alphabet.elements filtered) 0
;;

exception ActionNotFoundWithLabel of (string * Alphabet.t)
exception MultipleActionsFoundWithLabel of (string * Alphabet.t)

(** [] *)
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

(** [] *)
let get_outgoing_actions
  (edges : States.t Actions.t Edges.t)
  (from : state)
  (a : action)
  : States.t Actions.t
  =
  match Edges.find_opt edges from with
  | None -> make_actions ~size:0 ()
  | Some actions -> actions
;;

(** [] *)
let get_outgoing_actions_by_id
  (edges : States.t Actions.t Edges.t)
  (from : state)
  (id : int)
  : States.t Actions.t
  =
  get_outgoing_actions
    edges
    from
    (get_action_by_id (get_action_alphabet_from_edges edges) id)
;;

(** [] *)
let get_outgoing_actions_by_label
  (edges : States.t Actions.t Edges.t)
  (from : state)
  (label : string)
  : States.t Actions.t
  =
  get_outgoing_actions
    edges
    from
    (get_action_by_label (get_action_alphabet_from_edges edges) label)
;;

(********************************************)
(****** Other Functions *********************)
(****** (e.g., translation map helpers) *****)
(********************************************)

exception ReverseStateHashtblLookupFailed of ((state, state) Hashtbl.t * state)

(** [get_reverse_map_state tbl v] gets the key corresponding to [v] of translation map [tbl]. *)
let get_reverse_map_state (tbl : (state, state) Hashtbl.t) (v : state) : state =
  match Mebi_utils.get_key_of_val tbl v with
  | None -> raise (ReverseStateHashtblLookupFailed (tbl, v))
  | Some key -> key
;;
