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

(** [pstr_options debug] is the [pp_options] corresponding to [debug]. *)
let pstr_options (debug : bool) : pp_options =
  if debug then Debug () else Default ()
;;

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

(** [pp_wrap_as_supported] is a helper wrapper function for [pp_supported]. *)
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

(** [pstr] is a central function that handles the pretty-printing of all FSM types.
    @return a pretty-printed string representation of an FSM type.
    @param ?tabs is the amount of lhs tabs to include.
    @param ?options denote the level of detail to be included in printed types.
    @param to_str is the type to be pretty-printed. *)
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
  (* *)
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
  (* *)
  | Collection to_str ->
    (match to_str with
     (* *)
     | List to_str ->
       if pp_collection_is_empty (List to_str)
       then "[ ] (empty)"
       else
         Printf.sprintf
           "%s[%s%s]"
           basedent
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
              Block.fold
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
     (* *)
     | Map to_str ->
       if pp_collection_is_empty (Map to_str)
       then "{ } (empty)"
       else
         Printf.sprintf
           "%s{%s%s}"
           basedent
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
  (* *)
  | Utils _to_str -> "<<<<unimplemented>>>>"
  (* *)
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
      (fun (_from_state : state)
        (action : States.t Actions.t)
        (acc : States.t) ->
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
    @return the merger of [a] and [b], along with a bidirectional mapping from original and merged states.
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
    @return the union of [a] and [b], along with a mapping from the original to the merged actions.
    @param a is an alphabet to merge.
    @param b is an alphabet to merge.
    @raise ActionNotFoundInMergedAlphabet if when creating the mapping between original and merged actions, a merged action cannot be found. *)
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
    @raise StateNotFoundInMapOfStates if when updating a state in an edge, it cannot be found in [map_of_states].
    @raise ActionNotFoundInMapOfAlphabet if when updating a action in an edge, it cannot be found in [map_of_alphabet]. *)
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
    @return the merged fsm and a hashtable mapping the new states to the original.
    @param s is an fsm to merge.
    @param t is an fsm to merge.
    @raise StateNotFoundInMapOfStates if when updating the edges, the new state cannot be found (likely due to the sets of states not merging correctly). *)
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
