(* open Fsm *)
open Lts

let dir_perm : int = 0o777
let out_perm : int = 0o666
let default_output_dir : string = "./_dumps/"

let open_out_channel () = function
  | filepath ->
    open_out_gen [ Open_creat; Open_text; Open_append ] out_perm filepath
;;

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
           then "'"
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

let get_name (f : filename_kind) : string =
  match f with Auto () -> "unknown" | Just s -> s
;;

let get_filename ((n, f) : string * filename_kind) (is_complete_str : string)
  : string
  =
  match f with
  | Auto () ->
    Printf.sprintf "%s | %s | %s" get_local_timestamp n is_complete_str
  | Just s -> s
;;

type filetype_kind = JSON of unit

let build_filename
      (fnk : string * filename_kind)
      (filetype : filetype_kind)
      (is_complete : bool option)
  : string
  =
  let is_complete_str =
    match is_complete with
    | None -> "unknown completeness"
    | Some true -> "complete"
    | Some false -> "incomplete"
  in
  match filetype with
  | JSON () -> Printf.sprintf "%s.json" (get_filename fnk is_complete_str)
;;

let build_filepath
      (output_dir : output_dir_kind)
      (fnk : string * filename_kind)
      (filetype : filetype_kind)
      (is_complete : bool option)
  : string
  =
  match output_dir, filetype with
  | Default (), JSON () ->
    Filename.concat
      (Printf.sprintf "%sjson/" default_output_dir)
      (build_filename fnk filetype is_complete)
  | Exact s, _ -> Filename.concat s (build_filename fnk filetype is_complete)
;;

(** https://discuss.ocaml.org/t/how-to-create-a-new-file-while-automatically-creating-any-intermediate-directories/14837/5
*)
let rec create_parent_dir (fn : string) =
  let parent_dir = Filename.dirname fn in
  if not (Sys.file_exists parent_dir)
  then (
    create_parent_dir parent_dir;
    Sys.mkdir parent_dir dir_perm)
;;

(* type dumpable_kind = LTS of Lts.lts *)
(* | FSM of Fsm.fsm *)

(* let handle_filecontents (filename : string) (filetype : filetype_kind)
   (to_dump : dumpable_kind) : string * bool option = match to_dump, filetype
   with | LTS s, JSON () -> JSON.LTS.lts filename s, Utils.is_complete s.info
   ;; *)

type json_action_name = string
type json_action_silent = bool
type json_action_annotations = string list

type json_action =
  json_action_name * (json_action_silent * json_action_annotations)

(* module JSON_Alphabet = Set.Make (struct type t = json_action_name *
   (json_action_silent * json_action_annotations)

   let compare a b = match String.compare (fst a) (fst b) with | 0 -> (match (
   Bool.compare (fst (snd a)) (fst (snd b)) , List.compare (fun a' b' ->
   String.compare a' b') (snd (snd a)) (snd (snd b)) ) with | 0, 0 -> 0 | 0, n
   -> n | n, _ -> n) | n -> n ;; end) *)

type json_state_name = string
type json_state_info = string
type json_state = json_state_name * json_state_info

(* module JSON_States = Set.Make (struct type t = json_state

   let compare a b = match String.compare (fst a) (fst b) with | 0 ->
   String.compare (snd a) (snd b) | n -> n ;; end) *)

type json_edge_info = string
type json_edge_silent = string

type json_edge =
  (json_state_name * json_state_name)
  * (json_action_name * json_edge_silent * json_edge_info)

(* module JSON_Edges = Set.Make (struct type t = json_edge

   let compare a b = let c1 = String.compare (fst (fst a)) (fst (fst b)) in let
   c2 = String.compare (snd (fst a)) (snd (fst b)) in let c3 = String.compare
   (fst (snd a)) (fst (snd b)) in let c4 = String.compare (snd (snd a)) (snd
   (snd b)) in match c1, c2, c3, c4 with | 0, 0, 0, 0 -> 0 | 0, 0, 0, n -> n |
   0, 0, n, _ -> n | 0, n, _, _ -> n | n, _, _, _ -> n ;; end) *)

type model_info =
  { name : string
  ; kind : string
  ; extra : Utils.model_info option
  }

type json_model =
  { info : model_info
  ; alphabet : json_action Queue.t
  ; initial_state : json_state_name
  ; state_list : json_state Queue.t
  ; edge_list : json_edge Queue.t
  }

let string_opt (s : string option) : string =
  match s with None -> "null" | Some s -> Printf.sprintf "\"%s\"" (clean s)
;;

let bool_opt (b : bool option) : string =
  match b with None -> "null" | Some b -> Printf.sprintf "%b" b
;;

let is_model_complete (m : json_model) : bool option =
  match m.info.extra with None -> None | Some e -> Some e.is_complete
;;

exception ResultKindNotImplemented of Vernac.result_kind

let to_json_model (filename : string) (model : Vernac.result_kind) : json_model =
  match model with
  | LTS s ->
    let extra = s.info in
    let info = { name = filename; kind = "lts"; extra } in
    let alphabet = Queue.create () in
    let initial_state = string_opt s.init in
    let state_list =
      Lts.States.fold
        (fun (state : Lts.state) (acc : json_state Queue.t) ->
          Queue.push (state.name, string_opt state.info) acc;
          acc)
        s.states
        (Queue.create ())
    in
    let edge_list =
      Lts.Transitions.fold
        (fun (edge : Lts.transition) (acc : json_edge Queue.t) ->
          Queue.push
            ( (edge.from, edge.destination)
            , (edge.label, bool_opt edge.is_silent, string_opt edge.info) )
            acc;
          acc)
        s.transitions
        (Queue.create ())
    in
    { info; alphabet; initial_state; state_list; edge_list }
  | _ -> raise (ResultKindNotImplemented model)
;;

(* | FSM s, JSON () -> JSON.FSM.fsm filename s, Utils.is_complete s.info *)

let handle_if_first (b : bool ref) : string =
  if !b
  then (
    b := false;
    "\n")
  else ",\n"
;;

let write_json_extra_to_file (oc : out_channel) (i : Utils.model_info option)
  : unit
  =
  match i with
  | None -> Printf.fprintf oc "\t\t\"extra\": null\n"
  | Some i ->
    Printf.fprintf oc "\t\t\"extra\": {\n";
    Printf.fprintf oc "\t\t\t\"is_complete\": %b,\n" i.is_complete;
    Printf.fprintf oc "\t\t\t\"bound\": %i,\n" i.bound;
    Printf.fprintf oc "\t\t\t\"num_states\": %i,\n" i.num_states;
    Printf.fprintf oc "\t\t\t\"num_edges\": %i\n" i.num_edges;
    Printf.fprintf oc "\t\t}\n"
;;

let write_json_info_to_file (oc : out_channel) (i : model_info) : unit =
  Printf.fprintf oc (* open info *) "\t\"info\": {\n";
  Printf.fprintf oc "\t\t\"name\": \"%s\",\n" i.name;
  Printf.fprintf oc "\t\t\"kind\": \"%s\",\n" i.kind;
  write_json_extra_to_file oc i.extra;
  Printf.fprintf oc (* close info *) "\t},\n"
;;

let write_json_alphabet_to_file (oc : out_channel) (i : json_action Queue.t)
  : unit
  =
  if Queue.is_empty i
  then Printf.fprintf oc "\t\"alphabet\": [],\n"
  else (
    Printf.fprintf oc "\t\"alphabet\": [";
    let is_first = ref true in
    let rec iterate (n : int) () : unit =
      match n with
      | 0 -> ()
      | _ ->
        let action_name, (action_silent, action_annotations) = Queue.pop i in
        Printf.fprintf oc "%s" (handle_if_first is_first);
        Printf.fprintf oc "\t\t{\n";
        Printf.fprintf oc "\t\t\t\"action\": \"%s\",\n" action_name;
        Printf.fprintf oc "\t\t\t\"is_silent\": %b,\n" action_silent;
        if List.is_empty action_annotations
        then Printf.fprintf oc "\t\t\t\"annotations\": [],\n"
        else (
          let is_first' = ref true in
          Printf.fprintf oc "\t\t\t\"annotations\": [\n";
          List.iter
            (fun action_annotation ->
              Printf.fprintf oc "%s" (handle_if_first is_first');
              Printf.fprintf oc "\t\t\t\t\"%s\"" action_annotation)
            action_annotations;
          Printf.fprintf oc "\t\t\t],\n");
        Printf.fprintf oc "\t\t}";
        iterate (n - 1) ()
    in
    iterate (Queue.length i) ();
    Printf.fprintf oc "\n\t],\n")
;;

let write_xl_string_to_file (oc : out_channel) (xlstr : string) : unit =
  if String.length xlstr > 80
  then (
    let rem, smaller_strings =
      String.fold_left
        (fun (build_str, acc) s ->
          if List.length build_str > 80
          then (
            Queue.push (String.of_seq (List.to_seq (List.rev build_str))) acc;
            [ s ], acc)
          else s :: build_str, acc)
        ([], Queue.create ())
        xlstr
    in
    Queue.push (String.of_seq (List.to_seq (List.rev rem))) smaller_strings;
    let rec iterate (n : int) () : unit =
      match n with
      | 0 -> ()
      | _ ->
        let s = Queue.pop smaller_strings in
        Printf.fprintf oc "%s" s;
        iterate (n - 1) ()
    in
    iterate (Queue.length smaller_strings) ())
  else Printf.fprintf oc "%s" xlstr
;;

let write_json_states_to_file (oc : out_channel) (i : json_state Queue.t) : unit
  =
  if Queue.is_empty i
  then Printf.fprintf oc "\t\"states\": [],\n"
  else (
    Printf.fprintf oc "\t\"states\": [";
    let is_first = ref true in
    let rec iterate (n : int) () : unit =
      match n with
      | 0 -> ()
      | _ ->
        let state_name, state_info = Queue.pop i in
        Printf.fprintf oc "%s" (handle_if_first is_first);
        Printf.fprintf oc "\t\t{\n";
        Printf.fprintf oc "\t\t\t\"name\": \"%s\",\n" state_name;
        (* Printf.fprintf oc "\t\t\t\"info\": %s\n" state_info; *)
        Printf.fprintf oc "\t\t\t\"info\": ";
        write_xl_string_to_file oc state_info;
        Printf.fprintf oc "\n";
        Printf.fprintf oc "\t\t}";
        iterate (n - 1) ()
    in
    iterate (Queue.length i) ();
    Printf.fprintf oc "\n\t]\n")
;;

let write_json_edges_to_file (oc : out_channel) (i : json_edge Queue.t) : unit =
  if Queue.is_empty i
  then Printf.fprintf oc "\t\"edges\": [],\n"
  else (
    Printf.fprintf oc "\t\"edges\": [";
    let is_first = ref true in
    let rec iterate (n : int) () : unit =
      match n with
      | 0 -> ()
      | _ ->
        let (from_state, dest_state), (action_label, action_silent, action_info)
          =
          Queue.pop i
        in
        Printf.fprintf oc "%s" (handle_if_first is_first);
        Printf.fprintf oc "\t\t{\n";
        Printf.fprintf oc "\t\t\t\"from\": \"%s\",\n" from_state;
        Printf.fprintf oc "\t\t\t\"dest\": \"%s\",\n" dest_state;
        Printf.fprintf oc "\t\t\t\"labl\": \"%s\",\n" action_label;
        Printf.fprintf oc "\t\t\t\"info\": %s,\n" action_info;
        Printf.fprintf oc "\t\t\t\"tau\": %s\n" action_silent;
        Printf.fprintf oc "\t\t}";
        iterate (n - 1) ()
    in
    iterate (Queue.length i) ();
    Printf.fprintf oc "\n\t],\n")
;;

let write_json_to_file (m : json_model) (filepath : string) : unit =
  let oc = open_out_channel () filepath in
  Printf.fprintf (* open json *) oc "{\n";
  write_json_info_to_file oc m.info;
  close_out oc;
  let oc = open_out_channel () filepath in
  write_json_alphabet_to_file oc m.alphabet;
  close_out oc;
  let oc = open_out_channel () filepath in
  Printf.fprintf oc "\t\"initial_state\": %s,\n" m.initial_state;
  write_json_edges_to_file oc m.edge_list;
  close_out oc;
  let oc = open_out_channel () filepath in
  write_json_states_to_file oc m.state_list;
  close_out oc;
  let oc = open_out_channel () filepath in
  Printf.fprintf (* close json *) oc "}\n";
  close_out oc
;;

let write_to_file
      (output_dir : output_dir_kind)
      (fnk : string option * filename_kind)
      (filetype : filetype_kind)
      (result : Vernac.result_kind)
  : string
  =
  let filename = match fst fnk with None -> "unknown" | Some n -> n in
  (* convert model to json *)
  let json = to_json_model filename result in
  let is_complete = is_model_complete json in
  (* build filepath *)
  let filepath =
    build_filepath output_dir (filename, snd fnk) filetype is_complete
  in
  (* check parent directory exists *)
  create_parent_dir filepath;
  (* write to file *)
  (* List.iter (fun (line : string) -> Printf.fprintf oc "%s" line) lines; *)
  write_json_to_file json filepath;
  (* return filepath *)
  filepath
;;
