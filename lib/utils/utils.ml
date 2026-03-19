(** [swap (a, b)] is just [(b, a)]. *)
let swap : 'a * 'b -> 'b * 'a = fun (a, b) -> b, a

(** [split_at i l] is the [i]-th tail of list [l]. *)
let split_at (i : int) (l : 'a list) : 'a list =
  let rec split_at (i : int) (l : 'a list) (acc : 'a list) : 'a list =
    if i <= 0
    then acc
    else (match l with [] -> acc | h :: t -> split_at (i - 1) t (h :: acc))
  in
  split_at i l []
;;

(** for chaining comparisons together *)
let rec compare_chain : int list -> int = function
  | [] -> 0
  | 0 :: tl -> compare_chain tl
  | n :: _ -> n
;;

(** [try_seq_opt x fs] returns the first [f x] that returns [Some y], else [None].
*)
let rec try_seq_opt (x : 'a) : ('a -> 'b option) list -> 'b option = function
  | [] -> None
  | f :: tl -> (match f x with None -> try_seq_opt x tl | y -> y)
;;

(** [strip_snd l] is the list of rhs elements in a list of tuples [l] (typically a [constr]).
*)
let rec strip_snd (l : ('a * 'a) list) : 'a list =
  match l with [] -> [] | h :: t -> snd h :: strip_snd t
;;

(** [get_key_of_val tbl v] is a reverse-lookup in [tbl] for the key of value [v].
*)
let get_key_of_val (tbl : ('a, 'b) Hashtbl.t) (v : 'b) : 'a option =
  match
    List.find_opt
      (fun ((_key, value) : 'a * 'b) -> v == value)
      (List.of_seq (Hashtbl.to_seq tbl))
  with
  | None -> None
  | Some (key, _value) -> Some key
;;

(** [new_int_counter] returns a function that when called, will return the value of a counter and then increment it by 1, starting from 0.
*)
let new_int_counter ?(start : int = 0) ()
  : ((unit -> int) * (unit -> int)) * int ref
  =
  let id_counter : int ref = ref start in
  let get_and_incr_counter () : int =
    let to_return : int = !id_counter + 1 in
    id_counter := to_return;
    to_return
  in
  let get_and_decr_counter () : int =
    let to_return : int = !id_counter in
    id_counter := to_return - 1;
    to_return
  in
  (get_and_incr_counter, get_and_decr_counter), id_counter
;;

let str_sep
      ?(sep : string = "; ")
      ?(last : string = sep)
      ?(empty : string = "")
      (xs : string list)
  : string
  =
  let rec f : string list -> string = function
    | [] -> empty
    | h :: [] -> Printf.sprintf "%s%s" h last
    | h :: tl -> Printf.sprintf "%s%s%s" h sep (f tl)
  in
  f xs
;;

let rec filter_opt : 'a option list -> 'a list = function
  | [] -> []
  | None :: tl -> filter_opt tl
  | Some h :: tl -> h :: filter_opt tl
;;

let option_str : string option -> string = function
  | None -> "None"
  | Some x -> Printf.sprintf "Some (%s)" x
;;

let option_fstr (f : 'a -> string) : 'a option -> string = function
  | None -> "None"
  | Some x -> Printf.sprintf "Some (%s)" (f x)
;;

(** removes all newlines, excess spaces from string, and makes " " safe *)
let clean_string (s : string) : string =
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

module FileWriter = struct
  let perm : int = 0o777
  let default_dir : string = "./_dumps/"

  let get_loc () : string =
    match Loc.get_current_command_loc () with
    | Some { line_nb; fname = InFile { file; _ } } ->
      String.map (fun x -> if Char.equal '/' x then ' ' else x) file
      |> Printf.sprintf "line %i | %s" line_nb
    | _ -> "Unknown Location"
  ;;

  (** https://discuss.ocaml.org/t/how-to-create-a-new-file-while-automatically-creating-any-intermediate-directories/14837/5
  *)
  let rec create_parent_dir (fn : string) =
    let parent_dir = Filename.dirname fn in
    if not (Sys.file_exists parent_dir)
    then (
      create_parent_dir parent_dir;
      Sys.mkdir parent_dir perm)
  ;;

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
end
