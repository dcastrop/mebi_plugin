(** [state] is a 2-tuple with a unique [id] and (non-unique) name (or [label]).
    [id] is an integer for identifying the state.
    [pp] is a pretty-printed string of something corresponding to this state. *)
type state =
  { id : int
  ; hash : int
  ; pp : string
  }

(** [state ?pp ?hash id] is a wrapper constructor for [state].
    [?pp] is a pretty-printed respresentation, which defaults to [s{id}].
    [?hash] is for storing the hash of Coq-based terms; defaults to -1.
    [id] is the unique identifier for the state. *)
let state ?(pp : string option) ?(hash : int = -1) (id : int) =
  { id
  ; hash
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

(** [label] is an alias for [int], corresponding to the index of a Coq-based constructor. *)
(* type label = int *)

type action =
  { id : int
  ; name : string
  }

(** [Actions] is a set of [actions]. *)
module Actions = Set.Make (struct
    type t = action

    let compare a b = compare a.id b.id
  end)

(** [('a, 'b) transition] is a 2-tuple with a [label] and [to_state].
    [label] is of type ['a].
    [to_state] is of type ['b]. *)
type ('a, 'b) transition =
  { label : 'a
  ; to_state : 'b
  }

(** [fsm_transition] is a type describing outgoing edges of an OCaml FSM.
    [label] is a label (corresponding to the Coq-based LTS constructor number).
    [state] is thr destination state. *)
type fsm_transition = (action, state) transition

(** [edges] is a hashtable mapping states to their outgoing-transitions. *)
type edges = (state, fsm_transition) Hashtbl.t

(** [fsm] is a type used to describe an FSM in OCaml.
    [init] is the initial state.
    [edges] is a hashtable mapping states to outgoing edges. *)
type fsm =
  { init : state
  ; states : States.t
  ; edges : (state, fsm_transition) Hashtbl.t (* ; actions : Actions.t *)
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
  | Some () ->
    Printf.sprintf "(id: %d; hash: %d; pp: %s)" state.id state.hash state.pp
;;

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
      "[%s]"
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
  ((from_state : state), (outgoing_edge : fsm_transition))
  : string
  =
  (* get state pstr *)
  let from_state_pstr, dest_state_pstr =
    ( handle_state_pstr ids pp long from_state
    , handle_state_pstr ids pp long outgoing_edge.to_state )
  in
  (* TODO: when overhauling labels/actions, update this *)
  let edge_label_pstr =
    match long with
    | None -> Printf.sprintf "--{ %d }->" outgoing_edge.label.id
    | Some () -> Printf.sprintf "--{ %s }->" outgoing_edge.label.name
  in
  Printf.sprintf "%s %s %s" from_state_pstr edge_label_pstr dest_state_pstr
;;

(* TODO: merge below with [handle_state_pstr] ? *)
(** [handle_edge_pstr ids pp long e] is a wrapper for [pstr_edge] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_edge_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (e : state * fsm_transition)
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
  (edges : edges)
  : string
  =
  if Hashtbl.to_seq_keys edges |> Seq.is_empty
  then "[ ] (empty)"
  else
    Printf.sprintf
      "[%s]"
      (Hashtbl.fold
         (fun (from_state : state)
           (outgoing_edge : fsm_transition)
           (acc : string) ->
           Printf.sprintf
             "%s%s{ %s }\n"
             acc
             (Mebi_utils.str_tabs indent)
             (handle_edge_pstr ids pp long (from_state, outgoing_edge)))
         edges
         "\n")
;;

(** [handle_states_pstr ids pp long e] is a wrapper for [pstr_states] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_states_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (s : States.t)
  : string
  =
  match long with
  | None ->
    (match pp with
     | None -> pstr_states s
     | Some () ->
       (match ids with
        | None -> pstr_states ~pp:() s
        | Some () -> pstr_states ~ids:() ~pp:() s))
  | Some () -> pstr_states ~long:() s
;;

(** [handle_edges_pstr ids pp long e] is a wrapper for [pstr_edges] which
    makes it easier to pass the options from higher-level [pstr_] functions. *)
let handle_edges_pstr
  (ids : unit option)
  (pp : unit option)
  (long : unit option)
  (e : edges)
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
    "%s\n%s\n%s"
    (Printf.sprintf
       "init state: %s"
       (handle_state_pstr ids pp long the_fsm.init))
    (Printf.sprintf
       "states: %s"
       (handle_states_pstr ids pp long the_fsm.states))
    (Printf.sprintf "edges: %s" (handle_edges_pstr ids pp long the_fsm.edges))
;;
