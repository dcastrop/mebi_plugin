(** [state] is a 2-tuple with a unique [id] and (non-unique) name (or [label]).
    [id] is an integer for identifying the state.
    [pp] is a pretty-printed string of something corresponding to this state. *)
type state =
  { id : int (* ; hash : int *)
  ; pp : string
  }

(*
   !
   !!!
   ! continue changing edges to be States.t Actions.t Edges.t
   !
*)

(** [state ?pp ?hash id] is a wrapper constructor for [state].
    [?pp] is a pretty-printed respresentation, which defaults to [s{id}].
    [?hash] is for storing the hash of Coq-based terms; defaults to -1.
    [id] is the unique identifier for the state. *)
let state ?(pp : string option) (* ?(hash : int = -1) *)
                                  (id : int) =
  { id (* ; hash *)
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

exception StateNotFoundWithID of (int * States.t)
exception MultipleStatesFoundWithID of (int * States.t)
exception StateNotFoundWithName of (string * States.t)
exception MultipleStatesFoundWithName of (string * States.t)

let get_state_by_id (states : States.t) (id : int) : state =
  let filtered = States.filter (fun (s : state) -> s.id == id) states in
  if States.is_empty filtered then raise (StateNotFoundWithID (id, states));
  if States.cardinal filtered > 1
  then raise (MultipleStatesFoundWithID (id, states));
  List.nth (States.elements filtered) 0
;;

let get_state_by_name (states : States.t) (name : string) : state =
  let filtered = States.filter (fun (s : state) -> s.pp == name) states in
  if States.is_empty filtered then raise (StateNotFoundWithName (name, states));
  if States.cardinal filtered > 1
  then raise (MultipleStatesFoundWithName (name, states));
  List.nth (States.elements filtered) 0
;;

(** [label] is an alias for [int], corresponding to the index of a Coq-based constructor. *)
(* type label = int *)

type action =
  { id : int
  ; label : string
  }

(** [action] ... *)
let action ?(label : string option) (id : int) : action =
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

    (* let add_new (label:string) (actions:t) : t = add {id = actio; label }  *)
  end)

(** [('a, 'b) transition] is a 2-tuple with a [label] and [destination].
    [label] is of type ['a].
    [go_to] is of type ['b]. *)
type ('a, 'b) transition =
  { action : 'a
  ; destination : 'b
  }

(** [fsm_transition] is a type describing outgoing edges of an OCaml FSM.
    [label] is a label (corresponding to the Coq-based LTS constructor number).
    [state] is thr destination state. *)
(* type fsm_transition = (action, state) transition *)

(** [outgoing_edges] is a hashtable mapping the actions of outgoing edges of states to their destination state. *)
(* type outgoing_edges = (action, state) Hashtbl.t *)

(** [edges] is a hashtable mapping states to their outgoing-transitions. *)
(* type edges = (state, outgoing_edges) Hashtbl.t *)

(* module Edges = Set.Make (struct type t = (state, outgoing_edges) Hashtbl.t let compare a b = compare a.) *)

module Actions = Hashtbl.Make (struct
    type t = action

    let equal (t1 : action) (t2 : action) = Int.equal t1.id t2.id
    let hash (t : action) = Hashtbl.hash t
  end)

module Edges = Hashtbl.Make (struct
    type t = state

    let equal (t1 : state) (t2 : state) = Int.equal t1.id t2.id
    let hash (t : state) = Hashtbl.hash t
  end)

let get_edges_with_action
  (es : States.t Actions.t Edges.t)
  (s : state)
  (a : action)
  : States.t Actions.t
  =
  Actions.of_seq
    (List.to_seq
       (Actions.fold
          (fun (action : action)
            (destinations : States.t)
            (acc : (action * States.t) list) ->
            if action == a
            then List.append acc [ action, destinations ]
            else acc)
          (match Edges.find_opt es s with
           | None -> Actions.of_seq (List.to_seq [])
           | Some es' -> es')
          []))
;;

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

(** [fsm] is a type used to describe an FSM in OCaml.
    [init] is the initial state.
    [edges] is a hashtable mapping states to outgoing edges. *)
type fsm =
  { init : state
  ; states : States.t
  ; edges : States.t Actions.t Edges.t
  }
(* TODO: Currently, there may be many copies of the same state in an [fsm] (i.e., in [init] and the [edges]). Maybe add list of states and change others to be an index referencing their position in the list. *)

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
       (handle_action_alphabet_pstr
          ids
          pp
          long
          (get_action_alphabet_from_edges the_fsm.edges)))
    (Printf.sprintf "edges: %s" (handle_edges_pstr ids pp long the_fsm.edges))
;;
