(* open Fsm *)
open Model
open Lts

let dir_perm : int = 0o777
let out_perm : int = 0o666
let default_output_dir : string = "./_dumps/"

let open_out_channel () = function
  | filepath ->
    open_out_gen [ Open_creat; Open_text; Open_append ] out_perm filepath
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

let _get_name (f : filename_kind) : string =
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
type json_label_name = string
type json_action_silent = bool option

(* type json_action_annotations = string *)
type json_action = (json_action_name * json_label_name) * json_action_silent

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

(* type json_edge_silent = string *)
type json_edge_annotations = string list

type json_edge =
  (json_state_name * json_state_name)
  * (json_action_name * (json_edge_info * json_edge_annotations option))

(* module JSON_Edges = Set.Make (struct type t = json_edge

   let compare a b = let c1 = String.compare (fst (fst a)) (fst (fst b)) in let
   c2 = String.compare (snd (fst a)) (snd (fst b)) in let c3 = String.compare
   (fst (snd a)) (fst (snd b)) in let c4 = String.compare (snd (snd a)) (snd
   (snd b)) in match c1, c2, c3, c4 with | 0, 0, 0, 0 -> 0 | 0, 0, 0, n -> n |
   0, 0, n, _ -> n | 0, n, _, _ -> n | n, _, _, _ -> n ;; end) *)

type json_model =
  { name : string
  ; kind : string
  ; info : Info.t option
  ; result : (Algorithms.result * (string * string) option) option
  ; alphabet : json_action Queue.t
  ; initial_state : json_state_name
  ; state_list : json_state Queue.t
  ; edge_list : json_edge Queue.t
  }

let label_to_str (l : Action.Label.t) : string =
  Utils.clean_string (Action.Label.to_string l)
;;

let state_to_str (s : State.t) : string = Utils.clean_string (State.to_string s)

let state_opt_to_str (s : State.t option) : string =
  match s with None -> "null" | Some s -> state_to_str s
;;

let string_opt (s : string option) : string =
  match s with None -> "null" | Some s -> Printf.sprintf "%s" s
;;

let bool_opt (b : bool option) : string =
  match b with None -> "null" | Some b -> Printf.sprintf "%b" b
;;

let is_model_complete (m : json_model) : bool option =
  match m.info with None -> None | Some e -> Some e.is_complete
;;

exception ResultKindNotImplemented of Vernac.result_kind

let transitions_to_json_model_edges (ts : Model.Transitions.t)
  : json_edge Queue.t
  =
  Model.Transitions.fold
    (fun (edge : Transition.t) (acc : json_edge Queue.t) ->
      let from, label, dest, meta = edge in
      let edge_info =
        match meta with
        | None -> "null"
        | Some meta -> Action.MetaData.to_string meta
      in
      Queue.push
        ( (State.to_string from, State.to_string dest)
        , (Action.Label.to_string label, (edge_info, None)) )
        acc;
      acc)
    ts
    (Queue.create ())
;;

let edges_to_json_model_edges (es : States.t Actions.t Edges.t)
  : json_edge Queue.t
  =
  List.fold_left
    (fun (acc : json_edge Queue.t) (e : Model.Edge.t) ->
      let from, action, dest = e in
      Queue.push
        ( (State.to_string from, State.to_string dest)
        , ( Action.Label.to_string action.label
          , ( Action.MetaData.to_string action.meta
            , Some
                (List.fold_left
                   (fun (acc1 : string list) (anno : Action.annotation) ->
                     Action.annotation_to_string anno :: acc1)
                   []
                   action.annos) ) ) )
        acc;
      acc)
    (Queue.create ())
    (edges_to_list es)
;;

let alphabet_to_json_model_alphabet (al : Alphabet.t) : json_action Queue.t =
  Alphabet.fold
    (fun (l : Action.Label.t) (acc : json_action Queue.t) ->
      Queue.push
        ( (label_to_str l, Utils.clean_string (string_opt (fst (snd l))))
        , snd (snd l) )
        acc;
      acc)
    al
    (Queue.create ())
;;

let states_to_json_model_states (ss : States.t) : json_state Queue.t =
  Model.States.fold
    (fun (s : State.t) (acc : json_state Queue.t) ->
      Queue.push (state_to_str s, Utils.clean_string (string_opt (snd s))) acc;
      acc)
    ss
    (Queue.create ())
;;

let to_json_model
      ?(bisim_args : (string * string) option * bool option = None, None)
      (filename : string)
      (model : Vernac.result_kind)
  : json_model
  =
  let name = filename in
  match model with
  | LTS g ->
    let kind = "lts" in
    let info = g.info in
    let result = None in
    let alphabet = alphabet_to_json_model_alphabet g.alphabet in
    let initial_state = state_opt_to_str g.init in
    let state_list = states_to_json_model_states g.states in
    let edge_list = transitions_to_json_model_edges g.transitions in
    { name; kind; info; result; alphabet; initial_state; state_list; edge_list }
  | FSM m ->
    let kind = "fsm" in
    let info = m.info in
    let result = None in
    let alphabet = alphabet_to_json_model_alphabet m.alphabet in
    let initial_state = state_opt_to_str m.init in
    let state_list = states_to_json_model_states m.states in
    let edge_list = edges_to_json_model_edges m.edges in
    { name; kind; info; result; alphabet; initial_state; state_list; edge_list }
  | Alg r ->
    (match r with
     | Bisim b ->
       let kind = "bisim" in
       let (_, the_merged_fsm), _ = b in
       let m = the_merged_fsm in
       let info = m.info in
       let result = Some (r, fst bisim_args) in
       let alphabet = alphabet_to_json_model_alphabet m.alphabet in
       let initial_state = state_opt_to_str m.init in
       let state_list = states_to_json_model_states m.states in
       let edge_list = edges_to_json_model_edges m.edges in
       { name
       ; kind
       ; info
       ; result
       ; alphabet
       ; initial_state
       ; state_list
       ; edge_list
       })
  | Merge b ->
    let kind = "merge" in
    let (_, the_merged_fsm), _ = b in
    let m = the_merged_fsm in
    let info = m.info in
    let result = None in
    let alphabet = alphabet_to_json_model_alphabet m.alphabet in
    let initial_state = state_opt_to_str m.init in
    let state_list = states_to_json_model_states m.states in
    let edge_list = edges_to_json_model_edges m.edges in
    { name; kind; info; result; alphabet; initial_state; state_list; edge_list }
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

let states_to_json_list (ss : States.t) : string =
  let raw_ss = States.to_list ss in
  match raw_ss with
  | [] -> "[]"
  | h :: t ->
    Printf.sprintf
      "\t\t\t[%s]"
      (List.fold_left
         (fun (acc : string) (s : State.t) ->
           Printf.sprintf "%s, %s" acc (State.to_string ~pstr:false s))
         (State.to_string ~pstr:false h)
         t)
;;

let partition_to_json_list (pi : Partition.t) : string =
  let raw_pi = Partition.to_list pi in
  match raw_pi with
  | [] -> "[]"
  | h :: t ->
    Printf.sprintf
      "[\n%s\n\t\t]"
      (List.fold_left
         (fun (acc : string) (ss : States.t) ->
           Printf.sprintf "%s,\n%s" acc (states_to_json_list ss))
         (states_to_json_list h)
         t)
;;

let write_json_result_to_file
      (oc : out_channel)
      (r : (Algorithms.result * (string * string) option) option)
  : unit
  =
  match r with
  | None -> Printf.fprintf oc "\t\"result\": null,\n"
  | Some r ->
    (match r with
     | Bisim b, tup ->
       let _, (_bisim_states, _non_bisim_states) = b in
       Printf.fprintf oc "\t\"result\": {\n";
       Printf.fprintf
         oc
         "\t\t\"bisimilar\": %b,\n"
         (Algorithms.result_to_bool (fst r));
       Printf.fprintf
         oc
         "\t\t\"fsm names\": %s,\n"
         (match tup with
          | None -> "null"
          | Some tup -> Printf.sprintf "[\"%s\", \"%s\"]" (fst tup) (snd tup));
       Printf.fprintf
         oc
         "\t\t\"state totals\": %s,\n"
         (match tup with
          | None -> "null"
          | Some tup ->
            Printf.sprintf
              "[ {\"bisim\": [%i, %i]}, {\"non bisim\": [%i, %i]} ]"
              (Model.get_num_blocks (fst (snd b)))
              (Partition.cardinal (fst (snd b)))
              (Model.get_num_blocks (snd (snd b)))
              (Partition.cardinal (snd (snd b))));
       Printf.fprintf
         oc
         "\t\t\"bisimilar states\": %s"
         (partition_to_json_list (fst (snd b)));
       Printf.fprintf oc ",\n";
       Printf.fprintf
         oc
         "\t\t\"non-bisimilar states\": %s"
         (partition_to_json_list (snd (snd b)));
       Printf.fprintf oc "\n\t},\n";
       ())
;;

let write_json_info_to_file (oc : out_channel) (i : Info.t option) : unit =
  match i with
  | None -> Printf.fprintf oc "\t\"info\": null,\n"
  | Some i ->
    Printf.fprintf oc "\t\"info\": {\n";
    Printf.fprintf oc "\t\t\"complete\": %b,\n" i.is_complete;
    Printf.fprintf oc "\t\t\"bound\": %i,\n" i.bound;
    Printf.fprintf oc "\t\t\"num labels\": %i,\n" i.num_labels;
    Printf.fprintf oc "\t\t\"num states\": %i,\n" i.num_states;
    Printf.fprintf oc "\t\t\"num edges\": %i\n" i.num_edges;
    Printf.fprintf oc "\t},\n"
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
        let (action_name, action_label), action_silent = Queue.pop i in
        Printf.fprintf oc "%s" (handle_if_first is_first);
        Printf.fprintf oc "\t\t{\n";
        Printf.fprintf oc "\t\t\t\"action\": %s,\n" action_name;
        Printf.fprintf oc "\t\t\t\"label\": \"%s\",\n" action_label;
        Printf.fprintf oc "\t\t\t\"silent\": %s\n" (bool_opt action_silent);
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
        Printf.fprintf oc "\t\t\t\"name\": %s,\n" state_name;
        (* Printf.fprintf oc "\t\t\t\"info\": %s\n" state_info; *)
        Printf.fprintf oc "\t\t\t\"info\": \"";
        write_xl_string_to_file oc state_info;
        Printf.fprintf oc "\"\n";
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
        let ( (from_state, dest_state)
            , (action_label, (action_info, action_annotations)) )
          =
          Queue.pop i
        in
        Printf.fprintf oc "%s" (handle_if_first is_first);
        Printf.fprintf oc "\t\t{\n";
        Printf.fprintf oc "\t\t\t\"from\": %s,\n" from_state;
        Printf.fprintf oc "\t\t\t\"dest\": %s,\n" dest_state;
        Printf.fprintf oc "\t\t\t\"action\": %s,\n" action_label;
        Printf.fprintf oc "\t\t\t\"info\": \"%s\",\n" action_info;
        (match action_annotations with
         | None -> Printf.fprintf oc "\t\t\t\"annos\": null\n"
         | Some [] -> Printf.fprintf oc "\t\t\t\"annos\": []\n"
         | Some (h :: t) ->
           Printf.fprintf oc "\t\t\t\"annos\": [\n";
           Printf.fprintf oc "\t\t\t\t\"%s\"" h;
           List.iter
             (fun (anno : string) -> Printf.fprintf oc ",\n\t\t\t\t\"%s\"" anno)
             t;
           Printf.fprintf oc "\n\t\t\t]\n");
        Printf.fprintf oc "\t\t}";
        iterate (n - 1) ()
    in
    iterate (Queue.length i) ();
    Printf.fprintf oc "\n\t],\n")
;;

let write_json_to_file (m : json_model) (filepath : string) : unit =
  let oc = open_out_channel () filepath in
  Printf.fprintf (* open json *) oc "{\n";
  Printf.fprintf oc "\t\"name\": \"%s\",\n" m.name;
  Printf.fprintf oc "\t\"kind\": \"%s\",\n" m.kind;
  write_json_info_to_file oc m.info;
  close_out oc;
  let oc = open_out_channel () filepath in
  write_json_result_to_file oc m.result;
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

let build_json_from_single_model
      (filename : string)
      (result : Vernac.result_kind)
  : json_model * bool option
  =
  match result with
  | LTS _ ->
    let json = to_json_model filename result in
    let is_complete = is_model_complete json in
    json, is_complete
  | FSM _ ->
    let json = to_json_model filename result in
    let is_complete = is_model_complete json in
    json, is_complete
  | _ -> raise (ResultKindNotImplemented result)
;;

let write_to_file
      (output_dir : output_dir_kind)
      ((filename, kind) : string * filename_kind)
      (filetype : filetype_kind)
      (json : json_model)
  : string
  =
  let filepath =
    build_filepath output_dir (filename, kind) filetype (is_model_complete json)
  in
  create_parent_dir filepath;
  write_json_to_file json filepath;
  filepath
;;

let build_json_from_merged_model
      (output_dir : output_dir_kind)
      ((filename, context, kind) : string * string * filename_kind)
      (filetype : filetype_kind)
      (result : Vernac.result_kind)
      (((the_fsm_1, the_fsm_2), the_merged_fsm) : Fsm.pair * Fsm.t)
  : (json_model * bool option) * string
  =
  let the_fsm_1_name = Printf.sprintf "%s | %s (fsm 1)" context filename in
  let json_1, is_complete_1 =
    build_json_from_single_model the_fsm_1_name (FSM the_fsm_1)
  in
  let _ = write_to_file output_dir (the_fsm_1_name, kind) filetype json_1 in
  let the_fsm_2_name = Printf.sprintf "%s | %s (fsm 2)" context filename in
  let json_2, is_complete_2 =
    build_json_from_single_model the_fsm_2_name (FSM the_fsm_2)
  in
  let _ = write_to_file output_dir (the_fsm_2_name, kind) filetype json_2 in
  let the_merged_fsm, is_merged_complete =
    match json_1.info, json_2.info with
    | Some info_1, Some info_2 ->
      the_merged_fsm, Some (Bool.( && ) info_1.is_complete info_2.is_complete)
    | _, _ -> the_merged_fsm, None
  in
  let the_merged_name = Printf.sprintf "%s | %s (merged)" context filename in
  let json =
    to_json_model
      ~bisim_args:(Some (the_fsm_1_name, the_fsm_2_name), is_merged_complete)
      the_merged_name
      result
  in
  ( ( { json with
        info =
          (match the_merged_fsm.info, is_merged_complete with
           | None, None -> None
           | None, Some is_complete ->
             Some
               { is_complete
               ; bound = -1
               ; num_labels = Model.Alphabet.cardinal the_merged_fsm.alphabet
               ; num_states = Model.States.cardinal the_merged_fsm.states
               ; num_edges = Model.get_num_edges the_merged_fsm.edges
               }
           | Some info, Some is_complete -> Some { info with is_complete }
           | Some info, None -> Some info)
      }
    , is_merged_complete )
  , the_merged_name )
;;

let run
      (output_dir : output_dir_kind)
      ((filename, kind) : string option * filename_kind)
      (filetype : filetype_kind)
      (result : Vernac.result_kind)
  : string
  =
  let filename = match filename with None -> "unknown" | Some n -> n in
  let (json, is_complete), filename =
    match result with
    | LTS _ -> build_json_from_single_model filename result, filename
    | FSM _ -> build_json_from_single_model filename result, filename
    | Alg b ->
      (match b with
       | Bisim b ->
         build_json_from_merged_model
           output_dir
           (filename, "bisim", kind)
           filetype
           result
           (fst b))
    | Merge b ->
      build_json_from_merged_model
        output_dir
        (filename, "merge", kind)
        filetype
        result
        b
    | _ -> raise (ResultKindNotImplemented result)
  in
  write_to_file output_dir (filename, kind) filetype json
;;
