(** [state] is a 2-tuple with a unique [id] and (non-unique) name (or [label]).
    [id] is an integer for identifying the state.
    [pp] is a pretty-printed string of something corresponding to this state. *)
type state =
  { id : int
  (* ; hash : int *)
  ; pp : string
  }

(** [label] is similar to [state]. *)
type label = int

(** [('a, 'b) transition] is a 2-tuple with a [label] and [to_state].
    [label] is of type ['a].
    [to_state] is of type ['b]. *)
type ('a, 'b) transition =
  { label : 'a
  ; to_state : 'b
  }

(*  *)
(* module Inttbl = Hashtbl.Make() *)
type fsm_aux =
  {
    (* init : int *)
init:state
  (* ; states : *)
  ; edges : (state, (label, state) transition) Hashtbl.t
  }

(** [state] is a [state] type of [id] and [?name].
    [?name] is an optional argument, which when omitted is replaced by ["s[id]"].
    [id] is an integer. *)
(* let state ?name (id : int) : state =
  match name with
  | None -> { id; name = Printf.sprintf "s%d" id }
  | Some name' -> { id; name = name' }
;; *)

(** [seq_from_states states] is the sequence of [states]. *)
(* let seq_from_states (states : state list) : state Seq.t = List.to_seq states *)

(** [edge] is a transition from [lhs] to [rhs] via [label]. *)
type edge =
  { id : int
  ; lhs : int
  ; rhs : int
  ; label : string
  }

(** [has_edge] denotes the types that can have an [edge].
    [ID] corresponds to the [id] of a [state].
    [State] corresponds to a [state]. *)
type has_edge =
  | ID of int
  | State of state

(** [edge] is an [edge] type with a default [?label] of [[lhs_id] -> [rhs_id]].
    [?label]
    [id]
    [lhs]
    [rhs] *)
let edge ?label (id : int) (lhs : has_edge) (rhs : has_edge) : edge =
  let lhs_id : int =
    match lhs with
    | ID id -> id
    | State state ->
      (match state with
       | { id; _ } -> id)
  in
  let rhs_id : int =
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

(** [fsm] is a 3-tuple of [init] states, and list of [states] and [edges].
    [init] is the initial (starting) state of the [fsm].
    [states] is the list of [state]s of the [fsm].
    [edges] is the list of [edge]s of the [fsm].*)
type fsm =
  { init : int
  ; states : state list
  ; edges : edge list
  }

(** [fsm] is an [fsm] type with default [init] [id] of [0]. *)
let fsm ?(init : int = 0) (states : state list) (edges : edge list) : fsm =
  { init; states; edges }
;;

(** [has_state_id] denotes the types that can contain [id]s of [states]. *)
type has_state_id =
  | States of state list
  | State of state
  | Edges of edge list
  | Edge of edge
  | Fsm of fsm

(** [has_state] is [true] if [m] contains either: a [state] with matching [id], or a matching [id]. *)
let rec has_state (id_to_find : int) (m : has_state_id) : bool =
  match m with
  (* [State state] *)
  | State state ->
    (match state with
     | { id; _ } -> id_to_find == id)
  (* [States states] *)
  | States states ->
    let rec has_state' (id_to_find : int) (states : state list) : bool =
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
    let rec has_state' (id_to_find : int) (edges : edge list) : bool =
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

(** [has_state] denotes the types that can contain [states]. *)
type has_state =
  | State of state
  | States of state list
  | Fsm of fsm

(** [find_state id m] is the [state] in [m] with matching [id_to_find].*)
let rec find_state (id_to_find : int) (m : has_state) : state option =
  match m with
  (* [State state] *)
  | State state ->
    if has_state id_to_find (State state) then Some state else None
  (* [States states] *)
  | States states ->
    let rec find_state' (id_to_find : int) (states : state list) : state option =
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
let get_edges (has_edge : has_edge) (m : has_lts) : edge list option =
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
       let rec get_edges' (id : int) (edges : edge list) : edge list =
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

(* end *)

(** [Stringify] ... *)
module Stringify = struct
  (* open Fsm *)

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

  (** [stringable] denotes the types supported by the [to_string] function. *)
  type stringable =
    | ID of int
    | Label of string
    | State of state
    | States of state list
    | Edge of edge
    | Edges of edge list
    | Fsm of fsm

  (** [stringable_context] denotes the types that may be provided to enable more detailed pretty-printed strings via the [to_string] function. E.g., providing [States] for [Edges] will allow the [State.name] to be used rather than the [id]s found in an [Edge]. *)
  type stringable_context =
    | None
    | ShowIDs
    | States of state list
    | List of stringable_context list

  (** [add_to_stringable_context c1 c2] is the a [stringable_context.List] containing the concat of both [c1] and [c2]. *)
  let add_to_stringable_context
    (c1 : stringable_context)
    (c2 : stringable_context)
    : stringable_context
    =
    match c1, c2 with
    | List l1, List l2 -> List (List.concat [ l1; l2 ])
    | List l1, _ -> List (List.concat [ l1; [ c2 ] ])
    | _, List l2 -> List (List.concat [ [ c1 ]; l2 ])
    | _, _ -> List [ c1; c2 ]
  ;;

  (** [to_string ?indents ?prefix m] is the pretty-printed string of [m], with [?prefix] and [?indents]. If [?indents] is [-1] then everything will be pretty-printed inline. *)
  let rec to_string
    ?(context : stringable_context = None)
    ?(indents : int = 0)
    ?(prefix : string = "")
    (m : stringable)
    : string
    =
    Printf.sprintf
      "%s%s%s"
      (tabs indents)
      prefix
      (match m with
       (* [ID int] *)
       | ID id -> Printf.sprintf "{%d}" id
       (* [Label string] *)
       | Label label -> Printf.sprintf "[%s]" label
       (* [State state] *)
       | State state ->
         Printf.sprintf
           "(%s%s)"
           state.pp
           (* check if id should be printed too *)
           (if let rec check_context (ctx : stringable_context) : bool =
                 match ctx with
                 | ShowIDs -> true
                 | List l' ->
                   (match l' with
                    | [] -> false
                    | h :: t ->
                      (match h with
                       | ShowIDs -> true
                       | _ -> check_context (List t)))
                 | _ -> false
               in
               check_context context
            then Printf.sprintf " #%d" state.id
            else "")
       (* [States states] *)
       | States states ->
         (*** [to_string ?indents' states] is the pretty-printed string of each [state] in [states]. *)
         let rec to_string'
           ?(context' : stringable_context = None)
           ?(indents' : int = indents + 1)
           (states : state list)
           : string
           =
           match states with
           | [] -> ""
           | h :: t ->
             Printf.sprintf
               "%s%s%s"
               (to_string ~context:context' ~indents:indents' (State h))
               (if List.is_empty t
                then ""
                else Printf.sprintf ",%s" (if indents == -1 then "" else "\n"))
               (to_string' ~context' ~indents' t)
         in
         Printf.sprintf
           "[\n%s\n%s]"
           (to_string' ~context':context ~indents':(indents + 1) states)
           (tabs indents)
       (* [Edge edge] *)
       | Edge edge ->
         (match edge with
          | { id; lhs; rhs; label; _ } ->
            (* check if states are provided by context. *)
            let lhs', rhs' =
              match context with
              | States states ->
                ( to_string
                    ~context
                    ~indents:(-1)
                    (match find_state lhs (States states) with
                     | Some s -> State s
                     | None -> ID lhs)
                , to_string
                    ~context
                    ~indents:(-1)
                    (match find_state rhs (States states) with
                     | Some s -> State s
                     | None -> ID rhs) )
              | List l ->
                let rec check_context (ctx : stringable_context)
                  : string * string
                  =
                  match ctx with
                  | List l' ->
                    (match l' with
                     | [] ->
                       ( to_string ~context ~indents:(-1) (ID lhs)
                       , to_string ~context ~indents:(-1) (ID rhs) )
                     | h :: t ->
                       (match h with
                        | States states ->
                          ( to_string
                              ~context
                              ~indents:(-1)
                              (match find_state lhs (States states) with
                               | Some s -> State s
                               | None -> ID lhs)
                          , to_string
                              ~context
                              ~indents:(-1)
                              (match find_state rhs (States states) with
                               | Some s -> State s
                               | None -> ID rhs) )
                        | _ -> check_context (List t)))
                  | _ ->
                    ( to_string ~context ~indents:(-1) (ID lhs)
                    , to_string ~context ~indents:(-1) (ID rhs) )
                in
                check_context (List l)
              | _ ->
                ( to_string ~context ~indents:(-1) (ID lhs)
                , to_string ~context ~indents:(-1) (ID rhs) )
            in
            Printf.sprintf
              "from %s to %s via label: %s"
              lhs'
              rhs'
              (to_string ~context ~indents:(-1) (Label label)))
       (* [Edges edges] *)
       | Edges edges ->
         (*** [to_string ?indents' edges] is the pretty-printed string of each [edge] in [edges]. *)
         let rec to_string'
           ?(context' : stringable_context = None)
           ?(indents' : int = indents + 1)
           (edges : edge list)
           : string
           =
           match edges with
           | [] -> ""
           | h :: t ->
             Printf.sprintf
               "%s%s%s"
               (to_string ~context:context' ~indents:indents' (Edge h))
               (if List.is_empty t
                then ""
                else Printf.sprintf ",%s" (if indents == -1 then "" else "\n"))
               (to_string' ~context' ~indents' t)
         in
         Printf.sprintf
           "[\n%s\n%s]"
           (to_string' ~context':context ~indents':(indents + 1) edges)
           (tabs indents)
       (* [Fsm fsm] *)
       | Fsm fsm ->
         (match fsm with
          | { init; states; edges; _ } ->
            Printf.sprintf
              "{\n%s;\n%s;\n%s;\n}"
              (to_string
                 ~context
                 ~indents:(indents + 1)
                 ~prefix:"init: "
                 (State (Option.get (find_state init (States states)))))
              (to_string
                 ~context
                 ~indents:(indents + 1)
                 ~prefix:"states: "
                 (States states))
              (to_string
                 ~context:(add_to_stringable_context context (States states))
                 ~indents:(indents + 1)
                 ~prefix:"edges: "
                 (Edges edges))))
  ;;

  (** [pp str] is a shorthand for [Printf.printf "%s\n" str]. Useful for pretty-printed strings. *)
  let pp (str : string) : unit = Printf.printf "%s\n" str
end

(** [pp_tests]. *)
(* let pp_tests =
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
   ;; *)

(* () *)

(** [fsm.ml] entry point. *)
(* let () = pp_tests *)
