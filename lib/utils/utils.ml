(* See the [.mli]: the elements are gathered onto an accumulator, hence
   reversed. *)
let split_at (i : int) (l : 'a list) : 'a list =
  (* [split_at i l acc] is [acc] with the first [i] elements of [l] pushed
     onto it *)
  let rec split_at (i : int) (l : 'a list) (acc : 'a list) : 'a list =
    if i <= 0
    then acc
    else (match l with [] -> acc | h :: t -> split_at (i - 1) t (h :: acc))
  in
  split_at i l []
;;

(* See the [.mli]. *)
let rec compare_chain : int list -> int = function
  | [] -> 0
  | 0 :: tl -> compare_chain tl
  | n :: _ -> n
;;

(* See the [.mli]. *)
let new_int_counter ?(start : int = 0) ()
  : ((unit -> int) * (unit -> int)) * int ref
  =
  let id_counter : int ref = ref start in
  (* [next] in the [.mli] *)
  let get_and_incr_counter () : int =
    let to_return : int = !id_counter + 1 in
    id_counter := to_return;
    to_return
  in
  (* [prev] in the [.mli] *)
  let get_and_decr_counter () : int =
    let to_return : int = !id_counter in
    id_counter := to_return - 1;
    to_return
  in
  (get_and_incr_counter, get_and_decr_counter), id_counter
;;

(* See the [.mli]. *)
let str_sep
      ?(sep : string = "; ")
      ?(last : string = sep)
      ?(empty : string = "")
      (xs : string list)
  : string
  =
  (* [f xs] is [xs] joined, with [last] after the last *)
  let rec f : string list -> string = function
    | [] -> empty
    | h :: [] -> Printf.sprintf "%s%s" h last
    | h :: tl -> Printf.sprintf "%s%s%s" h sep (f tl)
  in
  f xs
;;

(* See the [.mli]. *)
let rec filter_opt : 'a option list -> 'a list = function
  | [] -> []
  | None :: tl -> filter_opt tl
  | Some h :: tl -> h :: filter_opt tl
;;

(* See the [.mli]. *)
let option_fstr (f : 'a -> string) : 'a option -> string = function
  | None -> "None"
  | Some x -> Printf.sprintf "Some (%s)" (f x)
;;

(** [clean_char b after_space c] appends what [c] becomes in
    {!clean_string} to [b]: a newline, tab or space is one space, or nothing
    just after a space; a double quote is a single quote; anything else is
    itself. [after_space] is whether the last character written was a space
    (or nothing is written yet), and is updated; a quote leaves it as it is.
    Raises nothing. *)
let clean_char (b : Buffer.t) (after_space : bool ref) (c : char) : unit =
  match c with
  | '\n' | '\r' | '\t' | ' ' ->
    if Bool.not !after_space
    then (
      Buffer.add_char b ' ';
      after_space := true)
  | '"' -> Buffer.add_char b '\''
  | c ->
    after_space := false;
    Buffer.add_char b c
;;

(* See the [.mli]. *)
let clean_string (s : string) : string =
  let b : Buffer.t = Buffer.create (String.length s) in
  let after_space : bool ref = ref true in
  String.iter (clean_char b after_space) s;
  Buffer.contents b
;;

module FileWriter = struct
  let perm : int = 0o777
  let default_dir : string = "./_dumps/"

  (** The installed source-location provider ({!set_loc_provider}). A hook
      rather than a direct call, because reading the location needs Rocq's
      [Loc]: [src/] installs the Rocq implementation at plugin load (see
      [Mebi_plugin.Rocq_output]) and a plain OCaml caller keeps the
      fallback. That is what keeps this library free of rocq-runtime. *)
  let the_loc_provider : (unit -> string) ref =
    ref (fun () -> "Unknown Location")
  ;;

  (* See the [.mli]. *)
  let set_loc_provider (f : unit -> string) : unit = the_loc_provider := f

  (* See the [.mli]. *)
  let get_loc () : string = !the_loc_provider ()

  (* See the [.mli]. After
     https://discuss.ocaml.org/t/how-to-create-a-new-file-while-automatically-creating-any-intermediate-directories/14837/5
  *)
  let rec create_parent_dir (fn : string) =
    let parent_dir = Filename.dirname fn in
    if not (Sys.file_exists parent_dir)
    then (
      create_parent_dir parent_dir;
      Sys.mkdir parent_dir perm)
  ;;

  (* See the [.mli]. [Unix.tm_mon] is 0-based and [tm_year] is an offset
     from 1900, so both need adjusting. The zero-padding is left to [%02d]
     rather than being hand-rolled per field: the previous version guarded
     on the raw [tm_mon] while printing it, which is exactly the coupling
     that let the month go out by one unnoticed. *)
  let get_local_timestamp : string =
    match Unix.localtime (Unix.time ()) with
    | { tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ } ->
      Printf.sprintf
        "%d %02d %02d - %02d:%02d:%02d"
        (tm_year + 1900)
        (tm_mon + 1)
        tm_mday
        tm_hour
        tm_min
        tm_sec
  ;;
end
