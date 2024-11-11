(** [id] is an [int] used to uniquely index states. *)
type id = int

(** [label] is a [string] used in [fsm]s for [state] names and transition (or [edge]) [label]s. *)
type label = string

(** [labels] is a list of [label]s. *)
type _labels = label list

(** [state] is a 2-tuple with a unique [id] and (non-unique) name (or [label]). *)
type state =
  { id : id
  ; name : label
  }

(** [default_name id] is [s[id]]. *)
let default_name (id : id) : string = Printf.sprintf "s%d" id

(* let state (id : id) ?(name : label = default_name id) : state = { id; name } *)

(** [state] is a [state] type with a default [name] of [default_name id]. *)
let state ?name (id : id) : state =
  match name with
  | None -> { id; name = default_name id }
  | Some name' -> { id; name = name' }
;;

(** [states] is a list of [state]s. *)
type states = state list

(** [seq_from_states states] is the sequence of [states]. *)
let seq_from_states (states : states) : state Seq.t = List.to_seq states

(** [edge] is a transition from [lhs] to [rhs] via [label]. *)
type edge =
  { id : id
  ; lhs : id
  ; rhs : id
  ; label : label
  }

(** [has_edge] denotes the types that can have an [edge] (i.e., a [state] or an [id] (of a [state])). *)
type has_edge =
  | ID of id
  | State of state

(** [edge] is an [edge] type with a default [label] of [[lhs_id] -> [rhs_id]]. *)
let edge ?label (id : id) (lhs : has_edge) (rhs : has_edge) : edge =
  let lhs_id : id =
    match lhs with
    | ID id -> id
    | State state ->
      (match state with
       | { id; _ } -> id)
  in
  let rhs_id : id =
    match rhs with
    | ID id -> id
    | State state ->
      (match state with
       | { id; _ } -> id)
  in
  { id
  ; lhs = lhs_id
  ; rhs = rhs_id
  ; label =
      (match label with
       | None -> Printf.sprintf "%d -> %d" lhs_id rhs_id
       | Some label' -> label')
  }
;;

(** [edges] is a list of [edge]s. *)
type edges = edge list

(* ! removing traces for now
   (** [Trace] is a set of [edge]s. *)
module Trace = Set.Make (struct
    type t = edge

    (** [compare a b] is [0] if [a] and [b] are the same, and otherwise compares [id]s of each (returning either [1] for [a] first and [-1] for [b] first).
        TODO: make this more useful. *)
    let compare a b =
      match a, b with
      | ( { lhs = a_l_id; rhs = a_r_id; label = a_label; _ }
        , { lhs = b_l_id; rhs = b_r_id; label = b_label; _ } ) ->
        if a_l_id == b_l_id
        then
          if a_r_id == b_r_id
          then if a_label == b_label then 0 else 0
          else if a_r_id > b_r_id
          then 1
          else -1
        else if a_l_id > b_l_id
        then 1
        else -1
    ;;
  end)

(** [trace edges] is a [Trace] type ([Set]) from list [edges]. *)
let trace (edges : edges) : Trace.t = edges |> Trace.of_list
*)

(** [fsm] is a 3-tuple of [init] states, and list of [states] and [edges]. *)
type fsm =
  { init : id (*** [init] is the initial (starting) state of the [fsm]. *)
  ; states : states (*** [states] is the list of [state]s of the [fsm]. *)
  ; edges : edges (*** [edges] is the list of [edge]s of the [fsm]. *)
  }

(** [fsm] is an [fsm] type with default [init] [id] of [0]. *)
let fsm ?(init : id = 0) (states : states) (edges : edges) : fsm =
  { init; states; edges }
;;

(** [has_state_id] denotes the types that can contain [id]s of [state]s. *)
type has_state_id =
  | States of states
  | State of state
  | Edges of edges
  | Edge of edge
  (* | Trace of Trace.t *)
  | Fsm of fsm

(** [has_state] is [true] if [m] contains either: a [state] with matching [id], or a matching [id]. *)
let rec has_state (id_to_find : id) (m : has_state_id) : bool =
  match m with
  (* [State state] *)
  | State state ->
    (match state with
     | { id; _ } -> id_to_find == id)
  (* [States states] *)
  | States states ->
    let rec has_state' (id_to_find : id) (states : states) : bool =
      match states with
      | [] -> false
      | { id; _ } :: t ->
        if id_to_find == id then true else has_state' id_to_find states
    in
    has_state' id_to_find states
  (* [Edge edge] *)
  | Edge edge ->
    (match edge with
     | { lhs; rhs; _ } -> id_to_find == lhs || id_to_find == rhs)
  (* [Edges edges] *)
  | Edges edges ->
    let rec has_state' (id_to_find : id) (edges : edges) : bool =
      match edges with
      | [] -> false
      | { lhs; rhs; _ } :: t ->
        if id_to_find == lhs || id_to_find == rhs
        then true
        else has_state' id_to_find t
    in
    has_state' id_to_find edges
  (* [Trace trace] *)
  (* | Trace trace ->
    Trace.exists
      (fun e ->
        match e with
        | { lhs; rhs; _ } -> id_to_find == lhs || id_to_find == rhs)
      trace *)
  (* [Fsm fsm] *)
  | Fsm fsm ->
    (match fsm with
     | { states; _ } -> has_state id_to_find (States states))
;;

(** [has_state] denotes the types that can contain [state]s. *)
type has_state =
  | State of state
  | States of states
  | Fsm of fsm

(** [find_state id m] is the [state] in [m] with matching [id_to_find].*)
let rec find_state (id_to_find : id) (m : has_state) : state option =
  match m with
  (* [State state] *)
  | State state ->
    if has_state id_to_find (State state) then Some state else None
  (* [States states] *)
  | States states ->
    let rec find_state' (id_to_find : id) (states : states) : state option =
      match states with
      | [] -> None
      | h :: t ->
        if has_state id_to_find (State h)
        then Some h
        else find_state' id_to_find t
    in
    find_state' id_to_find states
  (* [Fsm fsm] *)
  | Fsm fsm ->
    (match fsm with
     | { states; _ } -> find_state id_to_find (States states))
;;

(** [has_lts] denotes the types the contain both [states] and [edges]. *)
type has_lts = Fsm of fsm

(** [get_edges id m] is the list of [edges] in [m] that are outgoing from some [state] of [id] in [m]. *)
let get_edges (has_edge : has_edge) (m : has_lts) : edges option =
  (* check what [has_edge] is *)
  let id =
    match has_edge with
    | ID id -> id
    | State state ->
      (match state with
       | { id; _ } -> id)
  in
  (* check what [m] is *)
  match m with
  (* [Fsm fsm] *)
  | Fsm fsm ->
    (match fsm with
     | { states; edges; _ } ->
       (*** [get_edges' id edges] is the list of [edges] with [[id]==[lhs]]. *)
       let rec get_edges' (id : id) (edges : edges) : edges =
         match edges with
         | [] -> []
         | h :: t ->
           (match h with
            | { lhs; _ } ->
              if id == lhs then h :: get_edges' id t else get_edges' id t)
       in
       (match get_edges' id edges with
        | [] -> None
        | es -> Some es))
;;

(** [stringable] denotes the types supported by the [to_string] function. *)
type stringable =
  | ID of id
  | Label of label
  | State of state
  | States of states
  | Edge of edge
  | Edges of edges
  | Fsm of fsm
(* | Trace of Trace.t *)

(** [default_indent_val] is the default number of spaces to use perindent in [to_string]. *)
let default_indent_val = 2

(** [tabs ?size n] is [n] number of [?size]d spaces. *)
let rec tabs ?(size : int = default_indent_val) (n : int) : string =
  (*** [tab num] is [n'] number of spaces. *)
  let rec tab (n' : int) : string =
    if n' > 0 then Printf.sprintf " %s" (tab (n' - 1)) else ""
  in
  if n > 0 then Printf.sprintf "%s%s" (tab size) (tabs ~size (n - 1)) else ""
;;

(** [to_string ?indents ?prefix m] is the pretty-printed string of [m], with [?prefix] and [?indents]. If [?indents] is [-1] then everything will be pretty-printed inline. *)
let rec to_string ?(indents : int = 0) ?(prefix : string = "") (m : stringable)
  : string
  =
  Printf.sprintf
    "%s%s%s"
    (tabs indents)
    prefix
    (match m with
     (* [ID id] *)
     | ID id -> Printf.sprintf "{|%d|}" id
     (* [Label label] *)
     | Label label -> Printf.sprintf "[|%s|]" label
     (* [State state] *)
     | State state ->
       Printf.sprintf
         "(%s)"
         (match state with
          | { name; _ } -> name)
     (* [States states] *)
     | States states ->
       (*** [to_string ?indents' states] is the pretty-printed string of each [state] in [states]. *)
       let rec to_string' ?(indents' : int = indents + 1) (states : states)
         : string
         =
         match states with
         | [] -> ""
         | h :: t ->
           Printf.sprintf
             "%s%s%s"
             (to_string ~indents:indents' ~prefix (State h))
             (if List.is_empty t
              then ""
              else Printf.sprintf ",%s" (if indents == -1 then "" else "\n"))
             (to_string' ~indents' t)
       in
       Printf.sprintf
         "[\n%s\n%s]"
         (to_string' ~indents':(indents + 1) states)
         (tabs indents)
     (* [Edge edge] *)
     | Edge edge ->
       (match edge with
        | { lhs; rhs; label; _ } ->
          Printf.sprintf
            "%s to %s via %s"
            (to_string ~indents:(-1) (ID lhs))
            (to_string ~indents:(-1) (ID rhs))
            (to_string ~indents:(-1) (Label label)))
     (* [Edges edges] *)
     | Edges edges ->
       (*** [to_string ?indents' edges] is the pretty-printed string of each [edge] in [edges]. *)
       let rec to_string' ?(indents' : int = indents + 1) (edges : edges)
         : string
         =
         match edges with
         | [] -> ""
         | h :: t ->
           Printf.sprintf
             "%s%s%s"
             (to_string ~indents:indents' (Edge h))
             (if List.is_empty t
              then ""
              else Printf.sprintf ",%s" (if indents == -1 then "" else "\n"))
             (to_string' ~indents' t)
       in
       Printf.sprintf
         "[\n%s\n%s]"
         (to_string' ~indents':(indents + 1) edges)
         (tabs indents)
     (* [Fsm fsm] *)
     | Fsm fsm ->
       (match fsm with
        | { init; states; edges; _ } ->
          Printf.sprintf
            "{\n%s;\n%s;\n%s;\n}"
            (to_string
               ~indents:(indents + 1)
               ~prefix:"init: "
               (State (Option.get (find_state init (States states)))))
            (to_string
               ~indents:(indents + 1)
               ~prefix:"states: "
               (States states))
            (to_string ~indents:(indents + 1) ~prefix:"edges: " (Edges edges)))
       (* [Trace trace] *)
       (* | Trace trace -> to_string ~indents (Edges (Trace.elements trace)) *))
;;

(** [pp str] is a shorthand for [Printf.printf "%s\n" str]. Useful for pretty-printed strings. *)
let pp (str : string) : unit = Printf.printf "%s\n" str

(** [pp_tests]. *)
let pp_tests =
  Printf.printf "\nFsm, begin.\n\n";
  (*  *)
  let s0 = state 0 ~name:"s0" in
  pp (to_string ~prefix:"state 0: " (State s0));
  let s1 = state 1 in
  pp (to_string ~prefix:"state 1: " (State s1));
  let s2 = state 2 ~name:"S2" in
  pp (to_string ~prefix:"state 2: " (State s2));
  let s3 = state 3 in
  pp (to_string ~prefix:"state 3: " (State s3));
  (*  *)
  let states1 = [ s0; s1; s2; s3 ] in
  pp (to_string ~prefix:"states 0-3: " (States states1));
  (*  *)
  let e1 = edge 1 (State s0) (State s1) in
  pp (to_string ~prefix:"e1: " (Edge e1));
  let e2 = edge 2 (State s1) (State s2) in
  pp (to_string ~prefix:"e2: " (Edge e2));
  let e3 = edge 3 (State s2) (State s3) in
  pp (to_string ~prefix:"e3: " (Edge e2));
  (*  *)
  let edges1 : edges = [ e1; e2; e3 ] in
  pp (to_string ~prefix:"edges 1-3: " (Edges edges1));
  (*  *)
  let fsm1 = fsm states1 edges1 in
  pp (to_string ~prefix:"fsm1: " (Fsm fsm1));
  (*  *)
  let edges_from_s2 = Option.get (get_edges (ID 2) (Fsm fsm1)) in
  pp (to_string ~prefix:"edges from s2: " (Edges edges_from_s2));
  (*  *)
  let edges_from_s2' = Option.get (get_edges (State s2) (Fsm fsm1)) in
  pp (to_string ~prefix:"edges from s2': " (Edges edges_from_s2'));
  (*  *)
  Printf.printf "\nFsm, end.\n"
;;

(** [fsm.ml] entry point. *)
let () = pp_tests
