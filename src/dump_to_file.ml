open Fsm
open Lts

let perm : int = 0o777
let default_output_dir : string = "./_dumps/"

type output_dir_kind =
  | Default of unit
  | Exact of string

let get_local_timestamp : string =
  match Unix.localtime (Unix.time ()) with
  | { tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ } ->
    Printf.sprintf
      "%d %s%d %s%d - %s%d:%s%d:%s%d"
      (tm_year + 1900)
      (if tm_mon < 10 then "0" else "")
      tm_mon
      (if tm_mday < 10 then "0" else "")
      tm_mday
      (if tm_hour < 10 then "0" else "")
      tm_hour
      (if tm_min < 10 then "0" else "")
      tm_min
      (if tm_sec < 10 then "0" else "")
      tm_sec
;;

type filename_kind =
  | Auto of unit
  | Just of string
  | LTS of string
  | FSM of string

let get_name (f : filename_kind) : string =
  match f with Auto () -> "unknown" | Just s -> s | LTS s -> s | FSM s -> s
;;

let get_filename (f : filename_kind) (is_complete : bool option) : string =
  match f with
  | Auto () -> get_local_timestamp
  | Just s -> s
  | LTS _ ->
    Printf.sprintf
      "%s | LTS | %s%s"
      get_local_timestamp
      (get_name f)
      (match is_complete with
       | None -> " | unknown if complete"
       | Some b -> if b then " | complete" else " | incomplete")
  | FSM _ ->
    Printf.sprintf
      "%s | FSM | %s%s"
      get_local_timestamp
      (get_name f)
      (match is_complete with
       | None -> " | unknown if complete"
       | Some b -> if b then " | complete" else " | incomplete")
;;

type filetype_kind = JSON of unit

let build_filename
  (filename : filename_kind)
  (filetype : filetype_kind)
  (is_complete : bool option)
  : string
  =
  match filetype with
  | JSON () -> Printf.sprintf "%s.json" (get_filename filename is_complete)
;;

let build_filepath
  (output_dir : output_dir_kind)
  (filename : filename_kind)
  (filetype : filetype_kind)
  (is_complete : bool option)
  : string
  =
  match output_dir, filetype with
  | Default (), JSON () ->
    Filename.concat
      (Printf.sprintf "%sjson/" default_output_dir)
      (build_filename filename filetype is_complete)
  (* | Default (), _ -> Filename.concat default_output_dir (build_filename
     filename filetype) *)
  | Exact s, _ ->
    Filename.concat s (build_filename filename filetype is_complete)
;;

(** https://discuss.ocaml.org/t/how-to-create-a-new-file-while-automatically-creating-any-intermediate-directories/14837/5 *)
let rec create_parent_dir (fn : string) =
  let parent_dir = Filename.dirname fn in
  if not (Sys.file_exists parent_dir)
  then (
    create_parent_dir parent_dir;
    Sys.mkdir parent_dir perm)
;;

module JSON = struct
  (* let remove_double_spaces (s : string) : string = Str.global_replace
     (Str.regexp {| + |\t+\t|}) " " s ;; *)

  (* let remove_linebreaks (s : string) : string = Str.global_replace
     (Str.regexp {|\(\r\n|\n|\r\)|}) " " s ;; *)

  (** removes all newlines, excess spaces from string, and makes " " safe *)
  let clean (s : string) : string =
    let writing_space : bool ref = ref true in
    String.fold_left
      (fun (acc : string) (c : char) ->
        Printf.sprintf
          "%s%s"
          acc
          (if String.contains "\n\r\t" c
           then
             if !writing_space
             then ""
             else (
               writing_space := true;
               " ")
           else (
             let c_str : string = String.make 1 c in
             if String.contains "\"" c
             then "\\\""
             else (
               match String.equal " " c_str, !writing_space with
               | true, true -> ""
               | true, false ->
                 writing_space := true;
                 c_str
               | false, true ->
                 writing_space := false;
                 c_str
               | false, false -> c_str))))
      ""
      s
  ;;

  let quoted (s : string) : string = Printf.sprintf "\"%s\"" s

  type col_kind =
    | List of string list
    | Dict of string list

  let handle_list_sep sep : string =
    match sep with None -> " " | Some sep -> sep
  ;;

  let handle_dict_sep sep : string =
    match sep with None -> "\n" | Some sep -> sep
  ;;

  (** assumes elements of [ss] are already stringified. *)
  let col
    ?(prefix : string = "")
    ?(sep : string option)
    ?(tlindent : string = "")
    (ss : col_kind)
    : string
    =
    let ss, lhs, rhs, sep =
      match ss with
      | List ss -> ss, "[", "]", handle_list_sep sep
      | Dict ss -> ss, "{", "}", handle_dict_sep sep
    in
    match ss with
    | [] -> Printf.sprintf "%s %s" lhs rhs
    | h :: t ->
      Printf.sprintf
        "%s%s\n%s\n%s%s"
        prefix
        lhs
        (List.fold_left
           (fun (acc : string) (s : string) ->
             Printf.sprintf "%s,%s%s" acc sep s)
           h
           t)
        tlindent
        rhs
  ;;

  let key_val ?(prefix : string = "") (k : string) (v : string) : string =
    Printf.sprintf "%s%s: %s" prefix (quoted k) v
  ;;

  let model_info (m : Utils.model_info option) : string =
    match m with
    | None -> "None"
    | Some i ->
      col
        ~tlindent:"\t"
        (Dict
           [ key_val
               ~prefix:"\t\t"
               "is_complete"
               (quoted (Printf.sprintf "%b" i.is_complete))
           ; key_val
               ~prefix:"\t\t"
               "bound"
               (quoted (Printf.sprintf "%i" i.bound))
           ; key_val
               ~prefix:"\t\t"
               "num_states"
               (quoted (Printf.sprintf "%i" i.num_states))
           ; key_val
               ~prefix:"\t\t"
               "num_edges"
               (quoted (Printf.sprintf "%i" i.num_edges))
           ])
  ;;

  module LTS = struct
    let initial (init : string option) : string =
      match init with
      | None -> key_val "initial" (quoted "None")
      | Some s -> key_val "initial" (quoted (clean s))
    ;;

    let states (ss : Lts.States.t) : string =
      key_val
        "states"
        (col
           ~sep:"\n"
           ~tlindent:"\t"
           (List
              (Lts.States.fold
                 (fun (s : Lts.state) (acc : string list) ->
                   (match s.info with
                    | None -> Printf.sprintf "\t\t%s" (quoted (clean s.name))
                    | Some i ->
                      col
                        ~prefix:"\t\t"
                        ~tlindent:"\t\t"
                        (Dict
                           [ key_val
                               ~prefix:"\t\t\t"
                               "name"
                               (quoted (clean s.name))
                           ; key_val ~prefix:"\t\t\t" "info" (quoted (clean i))
                           ]))
                   :: acc)
                 ss
                 [])))
    ;;

    let transition (t : Lts.transition) : string =
      col
        ~prefix:"\t\t"
        ~tlindent:"\t\t"
        (Dict
           [ (*key_val ~prefix:"\t\t\t" "id" (quoted (clean (Printf.sprintf "%i"
               t.id))) ; *)
             key_val ~prefix:"\t\t\t" "from" (quoted (clean t.from))
           ; key_val ~prefix:"\t\t\t" "label" (quoted (clean t.label))
           ; key_val ~prefix:"\t\t\t" "dest" (quoted (clean t.destination))
           ; key_val
               ~prefix:"\t\t\t"
               "info"
               (quoted
                  (clean
                     (match t.info with
                      | None -> "None"
                      | Some index_tree -> index_tree)))
           ])
    ;;

    let transitions (ts : Lts.Transitions.t) : string =
      key_val
        "transitions"
        (col
           ~sep:"\n"
           ~tlindent:"\t"
           (List
              (Lts.Transitions.fold
                 (fun (t : Lts.transition) (acc : string list) ->
                   transition t :: acc)
                 ts
                 [])))
    ;;

    let lts (name : string) (m : lts) : string =
      Printf.sprintf
        "{\n\t%s,\n\t%s,\n\t%s,\n\t%s,\n\t%s,\n\t%s\n}"
        (key_val "name" (quoted name))
        (key_val "kind" (quoted "lts"))
        (key_val "info" (model_info m.info))
        (initial m.init)
        (transitions m.transitions)
        (states m.states)
    ;;
  end

  module FSM = struct
    let state (s : Fsm.state) : string =
      col
        (Dict
           [ key_val "id" (quoted (clean (Printf.sprintf "%d" s.id)))
           ; key_val "name" (quoted (clean s.name))
           ])
    ;;

    let annotation (anno : annotation) : string =
      col
        (List
           (List.fold_left
              (fun (acc : string list) ((s, a) : Fsm.state * action) ->
                col
                  (Dict [ key_val "state" (state s); key_val "action" a.label ])
                :: acc)
              []
              anno))
    ;;

    let annotations (annos : annotations) : string =
      key_val
        "annotations"
        (col
           (List
              (List.fold_left
                 (fun (acc : string list) (anno : annotation) ->
                   annotation anno :: acc)
                 []
                 annos)))
    ;;

    let action (a : action) : string =
      col
        (Dict
           [ key_val "id" (quoted (clean (Printf.sprintf "%i" a.id)))
           ; key_val "label" (quoted (clean a.label))
           ; key_val "is_tau" (quoted (clean (Printf.sprintf "%b" a.is_tau)))
           ; annotations a.annotation
           ])
    ;;

    let alphabet (alphas : Alphabet.t) : string =
      key_val
        "alphabet"
        (col
           (List
              (Fsm.Alphabet.fold
                 (fun (a : action) (acc : string list) -> action a :: acc)
                 alphas
                 [])))
    ;;

    let states ?(key : string = "states") (states : Fsm.States.t) : string =
      key_val
        key
        (col
           (List
              (Fsm.States.fold
                 (fun (s : Fsm.state) (acc : string list) -> state s :: acc)
                 states
                 [])))
    ;;

    let edge (from : Fsm.state) (a : action) (destinations : Fsm.States.t)
      : string
      =
      col
        (Dict
           [ key_val "from" (state from)
           ; key_val "label" (action a)
           ; states ~key:"destinations" destinations
           ])
    ;;

    let edges (edges : Fsm.States.t Actions.t Edges.t) : string =
      key_val
        "edges"
        (col
           (List
              (let from_states : Fsm.States.t =
                 Fsm.States.of_seq (Edges.to_seq_keys edges)
               in
               Fsm.States.fold
                 (fun (from_state : Fsm.state) (from_acc : string list) ->
                   let outgoing_actions : Fsm.States.t Actions.t =
                     Edges.find edges from_state
                   in
                   let actions : Alphabet.t =
                     Alphabet.of_seq (Actions.to_seq_keys outgoing_actions)
                   in
                   let action_acc : string list =
                     Alphabet.fold
                       (fun (a : action) (action_acc : string list) ->
                         let destination_states : Fsm.States.t =
                           Actions.find outgoing_actions a
                         in
                         edge from_state a destination_states :: action_acc)
                       actions
                       []
                   in
                   List.append from_acc action_acc)
                 from_states
                 [])))
    ;;

    let initial (s : Fsm.state option) : string =
      match s with
      | None -> key_val "initial" (quoted "None")
      | Some s -> key_val "initial" (state s)
    ;;

    let fsm (name : string) (m : fsm) : string =
      Printf.sprintf
        "{\n\t%s,\n\t%s,\n\t%s,\n\t%s,\n\t%s,\n\t%s,\n\t%s\n}"
        (key_val "name" (quoted name))
        (key_val "kind" (quoted "fsm"))
        (key_val "info" (model_info m.info))
        (initial m.init)
        (alphabet m.alphabet)
        (states m.states)
        (edges m.edges)
    ;;
  end
end

type dumpable_kind =
  | LTS of Lts.lts
  | FSM of Fsm.fsm

let handle_filecontents
  (filename : string)
  (filetype : filetype_kind)
  (to_dump : dumpable_kind)
  : string * bool option
  =
  match to_dump, filetype with
  | LTS s, JSON () -> JSON.LTS.lts filename s, Utils.is_complete s.info
  | FSM s, JSON () -> JSON.FSM.fsm filename s, Utils.is_complete s.info
;;

let write_to_file
  (output_dir : output_dir_kind)
  (filename : filename_kind)
  (filetype : filetype_kind)
  (to_dump : dumpable_kind)
  : string
  =
  (* get content to output *)
  let (content, is_complete) : string * bool option =
    handle_filecontents (get_name filename) filetype to_dump
  in
  (* build filepath *)
  let filepath : string =
    build_filepath output_dir filename filetype is_complete
  in
  (* check parent directory exists *)
  create_parent_dir filepath;
  (* write to file *)
  (* Out_channel.with_open_gen [ Out_channel.Open_creat; Out_channel.Open_excl ]
     perm filepath *)
  (* (fun oc -> Out_channel.output_string oc content); *)
  let oc = open_out filepath in
  Printf.fprintf oc "%s" content;
  close_out oc;
  (* return filepath *)
  filepath
;;
