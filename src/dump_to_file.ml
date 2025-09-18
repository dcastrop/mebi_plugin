(* open Fsm *)
(* open Model *)
(* open Lts *)

let dir_perm : int = 0o777
let out_perm : int = 0o666
let default_output_dir : string = "./_dumps/"

let _open_out_channel () = function
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

let _build_filepath
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
let rec _create_parent_dir (fn : string) =
  let parent_dir = Filename.dirname fn in
  if not (Sys.file_exists parent_dir)
  then (
    _create_parent_dir parent_dir;
    Sys.mkdir parent_dir dir_perm)
;;

let run
      (output_dir : output_dir_kind)
      ((filename, kind) : string option * filename_kind)
      (filetype : filetype_kind)
  : string
  =
  let filename = match filename with None -> "unknown" | Some n -> n in
  Logging.Log.warning "TODO: dump_to_file.run";
  filename
;;
(* let (json, is_complete), filename =
   match result with
   | LTS _ -> build_json_from_single_model filename result, filename
   | FSM _ -> build_json_from_single_model filename result, filename
   | Alg b ->
   (match b with
   | Satur m -> build_json_from_single_model filename (FSM m), filename
   | Minim (m, _) -> build_json_from_single_model filename (FSM m), filename
   | Bisim (b, m, _) ->
   build_json_from_merged_model
   output_dir
   (filename, "bisim", kind)
   filetype
   result
   (b, m))
   | Merge b ->
   build_json_from_merged_model
   output_dir
   (filename, "merge", kind)
   filetype
   result
   b
   | _ -> raise (ResultKindNotImplemented result)
   in
   write_to_file output_dir (filename, kind) filetype json *)
