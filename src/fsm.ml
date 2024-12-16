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

(*************************************************)
(****** Blocks & Partitions of States ************)
(****** (Used by Bisimilarity algorithms.) *******)
(****** (Featured here for more seemless pstr.) **)
(*************************************************)

(** [Block] is a set of states.
    This is necessary since [KS90] requires the actions of states to be sortable. *)
module Block = States

module Partition = Set.Make (States)

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
  | Block of Block.t
  | Partition of Partition.t

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
  | Block of Block.t
  | Alphabet of Alphabet.t
  | Actions of States.t Actions.t
  | Edges of States.t Actions.t Edges.t
  | Partition of Partition.t
  | Fsm of fsm

(** [pp_options] denotes the types that specifically support the [pp] flag. *)
type pp_options =
  | Default of unit
  | Debug of unit

(** [pp_collection_is_empty c] returns true if [c] is empty. *)
let pp_collection_is_empty (c : pp_collection) : bool =
  match c with
  | List l ->
    (match l with
     | States s -> States.is_empty s
     | Alphabet a -> Alphabet.is_empty a
     | Block b -> Block.is_empty b
     | Partition p ->
       (* check also that any blocks are empty too *)
       if Partition.is_empty p
       then true
       else Partition.for_all (fun (b : Block.t) -> Block.is_empty b) p)
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
  | Block to_wrap -> Collection (List (Block to_wrap))
  | Alphabet to_wrap -> Collection (List (Alphabet to_wrap))
  | Actions to_wrap -> Collection (Map (Actions to_wrap))
  | Edges to_wrap -> Collection (Map (Edges to_wrap))
  | Partition to_wrap -> Collection (List (Partition to_wrap))
  | Fsm to_wrap -> Fsm to_wrap
;;

(** [pstr] *)
let rec pstr
  ?(tabs : int = 0)
  ?(options : pp_options = Default ())
  (to_str : pp_supported)
  : string
  =
  let basedent = Utils.str_tabs (tabs - 1) in
  let indent = Utils.str_tabs tabs in
  (* build string *)
  match to_str with
  (*  *)
  | Axiom to_str ->
    Printf.sprintf
      "%s%s"
      indent
      (match to_str with
       (* [state] *)
       | State to_str ->
         Printf.sprintf
           "(%s%s)"
           to_str.pp
           (match options with
            | Default () -> ""
            | Debug () -> Printf.sprintf " <id: %d>" to_str.id)
       (* [action] *)
       | Action to_str ->
         Printf.sprintf
           "{| %s%s |}"
           to_str.label
           (match options with
            | Default () -> ""
            | Debug () -> Printf.sprintf " <id: %d>" to_str.id)
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
                    (pstr
                       ~tabs:(tabs + 1)
                       ~options
                       (pp_wrap_as_supported (State s))))
                to_str
                "\n"
            (* [Block] *)
            | Block to_str ->
              pstr ~tabs ~options (pp_wrap_as_supported (States to_str))
            (* [Alphabet] *)
            | Alphabet to_str ->
              Alphabet.fold
                (fun (a : action) (acc : string) ->
                  Printf.sprintf
                    "%s%s\n"
                    acc
                    (pstr
                       ~tabs:(tabs + 1)
                       ~options
                       (pp_wrap_as_supported (Action a))))
                to_str
                "\n"
            (* [Partition] *)
            | Partition to_str ->
              Partition.fold
                (fun (b : Block.t) (acc : string) ->
                  Printf.sprintf
                    "%s%s\n"
                    acc
                    (pstr
                       ~tabs:(tabs + 1)
                       ~options
                       (pp_wrap_as_supported (Block b))))
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
              (* order by starting state *)
              States.fold
                (fun (from : state) (acc : string) ->
                  let outgoing_actions = Edges.find to_str from in
                  Printf.sprintf
                    "%s%s"
                    acc
                    (* order by action ids *)
                    (Alphabet.fold
                       (fun (a : action) (acc' : string) ->
                         Printf.sprintf
                           "%s%s"
                           acc'
                           (* order by destination state *)
                           (States.fold
                              (fun (destination : state) (acc'' : string) ->
                                Printf.sprintf
                                  "%s%s%s\n"
                                  acc''
                                  indent
                                  (pstr
                                     ~options
                                     (pp_wrap_as_supported
                                        (Edge (from, a, destination)))))
                              (Actions.find outgoing_actions a)
                              ""))
                       (* get alphabet from actions *)
                       (Alphabet.of_seq (Actions.to_seq_keys outgoing_actions))
                       ""))
                (* get states from edges *)
                (States.of_seq (Edges.to_seq_keys to_str))
                "\n")
           basedent)
  (*  *)
  | Utils _to_str -> "<<<<unimplemented>>>>"
  (*  *)
  | Fsm to_str ->
    let indent = Utils.str_tabs (tabs + 1) in
    Printf.sprintf
      "{ %s; %s; %s; %s; \n%s}"
      (Printf.sprintf
         "\n%sinitial state: %s"
         indent
         (pstr ~options (pp_wrap_as_supported (State to_str.init))))
      (Printf.sprintf
         "\n%salphabet: %s"
         indent
         (pstr
            ~tabs:(tabs + 2)
            ~options
            (pp_wrap_as_supported (Alphabet to_str.alphabet))))
      (Printf.sprintf
         "\n%sstates: %s"
         indent
         (pstr
            ~tabs:(tabs + 2)
            ~options
            (pp_wrap_as_supported (States to_str.states))))
      (Printf.sprintf
         "\n%sedges: %s"
         indent
         (pstr
            ~tabs:(tabs + 2)
            ~options
            (pp_wrap_as_supported (Edges to_str.edges))))
      basedent
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
  (_a : action)
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
