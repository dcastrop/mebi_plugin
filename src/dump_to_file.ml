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
      "%d %d %d - %d:%d:%d"
      tm_year
      tm_mon
      tm_mday
      tm_hour
      tm_min
      tm_sec
;;

type filename_kind =
  | Auto of unit
  | Just of string
  | LTS of string
  | FSM of string

let get_filename (f : filename_kind) : string =
  match f with
  | Auto () -> get_local_timestamp
  | Just s -> s
  | LTS s -> Printf.sprintf "LTS | %s | %s" s get_local_timestamp
  | FSM s -> Printf.sprintf "FSM | %s | %s" s get_local_timestamp
;;

type filetype_kind = JSON of unit

let build_filename (filename : filename_kind) (filetype : filetype_kind)
  : string
  =
  match filetype with
  | JSON () -> Printf.sprintf "%s.json" (get_filename filename)
;;

let build_filepath
  (output_dir : output_dir_kind)
  (filename : filename_kind)
  (filetype : filetype_kind)
  : string
  =
  match output_dir, filetype with
  | Default (), JSON () ->
    Filename.concat
      (Printf.sprintf "%sjson/" default_output_dir)
      (build_filename filename filetype)
  (* | Default (), _ ->  Filename.concat default_output_dir (build_filename filename filetype) *)
  | Exact s, _ -> Filename.concat s (build_filename filename filetype)
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
  (* let remove_double_spaces (s : string) : string =
    Str.global_replace (Str.regexp {| + |\t+\t|}) " " s
  ;; *)

  (* let remove_linebreaks (s : string) : string =
    Str.global_replace (Str.regexp {|\(\r\n|\n|\r\)|}) " " s
  ;; *)

  (** removes all newlines, excess spaces from string *)
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
             match String.equal " " c_str, !writing_space with
             | true, true -> ""
             | true, false ->
               writing_space := true;
               c_str
             | false, true ->
               writing_space := false;
               c_str
             | false, false -> c_str)))
      ""
      s
  ;;

  let quoted (s : string) : string = Printf.sprintf "\"%s\"" s

  type col_kind =
    | List of string list
    | Dict of string list

  (** assumes elements of [ss] are already stringified. *)
  let col (ss : col_kind) : string =
    let ss, lhs, rhs, sep =
      match ss with
      | List ss -> ss, "[", "]", " "
      | Dict ss -> ss, "{", "}", "\n"
    in
    Printf.sprintf
      "%s %s %s"
      lhs
      (List.fold_left
         (fun (acc : string) (s : string) -> Printf.sprintf "%s,%s%s" acc sep s)
         (List.hd ss)
         (List.tl ss))
      rhs
  ;;

  let key_val (k : string) (v : string) : string =
    Printf.sprintf "%s: %s" (quoted k) v
  ;;

  module LTS = struct
    let initial (init : string option) : string =
      match init with
      | None -> key_val "initial" (quoted "None")
      | Some s -> key_val "initial" (quoted (clean s))
    ;;

    let transition (t : Lts.transition) : string =
      key_val
        (Printf.sprintf "%i" t.id)
        (col
           (Dict
              [ key_val "from" (quoted (clean t.from))
              ; key_val "label" (quoted (clean t.label))
              ; key_val "destination" (quoted (clean t.destination))
              ]))
    ;;

    let transitions (ts : Lts.Transitions.t) : string =
      key_val
        "transitions"
        (col
           (Dict
              (Lts.Transitions.fold
                 (fun (t : Lts.transition) (acc : string list) ->
                   List.append acc [ transition t ])
                 ts
                 [])))
    ;;

    let lts (name : string) (m : lts) : string =
      Printf.sprintf
        "{\n\t%s,\n\t%s,\n\t%s,\n\t%s\n}"
        (key_val "name" (quoted name))
        (key_val "kind" (quoted "lts"))
        (initial m.init)
        (transitions m.transitions)
    ;;
  end

  module FSM = struct
    let action (a : action) : string =
      col
        (Dict
           [ key_val "id" (quoted (clean (Printf.sprintf "%i" a.id)))
           ; key_val "label" (quoted (clean a.label))
           ; key_val "is_tau" (quoted (clean (Printf.sprintf "%b" a.is_tau)))
             (* ; key_val "annotations" (quoted (clean a.annotation)) *)
           ])
    ;;

    let alphabet (alphas : Alphabet.t) : string =
      key_val
        "alphabet"
        (col
           (List
              (Fsm.Alphabet.fold
                 (fun (a : action) (acc : string list) ->
                   List.append acc [ action a ])
                 alphas
                 [])))
    ;;

    let state (s : state) : string =
      col
        (Dict
           [ key_val "id" (quoted (clean (Printf.sprintf "%d" s.id)))
           ; key_val "name" (quoted (clean s.name))
           ])
    ;;

    let states ?(key : string = "states") (states : States.t) : string =
      key_val
        key
        (col
           (List
              (Fsm.States.fold
                 (fun (s : state) (acc : string list) ->
                   List.append acc [ state s ])
                 states
                 [])))
    ;;

    let edge (from : state) (a : action) (destinations : States.t) : string =
      col
        (Dict
           [ key_val "from" (state from)
           ; key_val "label" (action a)
           ; states ~key:"destinations" destinations
           ])
    ;;

    let edges (edges : States.t Actions.t Edges.t) : string =
      key_val
        "edges"
        (col
           (List
              (let from_states : States.t =
                 States.of_seq (Edges.to_seq_keys edges)
               in
               States.fold
                 (fun (from_state : state) (from_acc : string list) ->
                   let outgoing_actions : States.t Actions.t =
                     Edges.find edges from_state
                   in
                   let actions : Alphabet.t =
                     Alphabet.of_seq (Actions.to_seq_keys outgoing_actions)
                   in
                   let action_acc : string list =
                     Alphabet.fold
                       (fun (a : action) (action_acc : string list) ->
                         let destination_states : States.t =
                           Actions.find outgoing_actions a
                         in
                         List.append
                           action_acc
                           [ edge from_state a destination_states ])
                       actions
                       []
                   in
                   List.append from_acc action_acc)
                 from_states
                 [])))
    ;;

    let initial (s : state option) : string =
      match s with
      | None -> key_val "initial" (quoted "None")
      | Some s -> key_val "initial" (state s)
    ;;

    let fsm (name : string) (m : fsm) : string =
      Printf.sprintf
        "{\n\t%s,\n\t%s,\n\t%s,\n\t%s,\n\t%s,\n\t%s\n}"
        (key_val "name" (quoted name))
        (key_val "kind" (quoted "fsm"))
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
  : string
  =
  match to_dump, filetype with
  | LTS s, JSON () -> JSON.LTS.lts filename s
  | FSM s, JSON () -> JSON.FSM.fsm filename s
;;

let write_to_file
  (output_dir : output_dir_kind)
  (filename : filename_kind)
  (filetype : filetype_kind)
  (to_dump : dumpable_kind)
  : string
  =
  (* build filepath *)
  let filepath : string = build_filepath output_dir filename filetype in
  (* check parent directory exists *)
  create_parent_dir filepath;
  (* get content to output *)
  let content : string =
    handle_filecontents (get_filename filename) filetype to_dump
  in
  (* write to file *)
  (* Out_channel.with_open_gen
     [ Out_channel.Open_creat; Out_channel.Open_excl ]
     perm
     filepath *)
  (* (fun oc -> Out_channel.output_string oc content); *)
  let oc = open_out filepath in
  Printf.fprintf oc "%s\n" content;
  close_out oc;
  (* return filepath *)
  filepath
;;
