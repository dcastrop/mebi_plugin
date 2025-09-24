(** [split_at i l acc] is a tuple containing two lists [(l', acc)] split from list [l] at index [i].
*)
let rec split_at i l acc =
  if i <= 0
  then l, acc
  else (match l with [] -> acc, [] | h :: t -> split_at (i - 1) t (h :: acc))
;;

(** [strip_snd l] is the list of rhs elements in a list of tuples [l] (typically a [constr]).
*)
let rec strip_snd (l : ('a * 'a) list) : 'a list =
  match l with [] -> [] | h :: t -> snd h :: strip_snd t
;;

let is_unit_option (override : unit option) : bool =
  match override with None -> false | Some () -> true
;;

let bool_opt_to_string (default : string) (opt : bool option) : string =
  match opt with None -> default | Some o -> Bool.to_string o
;;

let string_opt_to_string (default : string) (opt : string option) : string =
  Option.default default opt
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

(** [print ?show to_print] is a wrapper for [Printf.printf].
    @param ?show determines if [to_print] is outputted. *)
let print ?(show : bool = false) (to_print : string) : unit =
  match show with true -> Printf.printf "%s" to_print | false -> ()
;;

(** [default_indent_val] is the default number of spaces to use perindent in [to_string].
*)
let default_indent_val = 2

(** [str_tabs ?size n] is [n] number of [?size]d spaces. *)
let rec str_tabs ?(size : int = default_indent_val) (n : int) : string =
  (*** [tab num] is [n'] number of spaces. *)
  let rec tab (n' : int) : string =
    if n' > 0 then Printf.sprintf " %s" (tab (n' - 1)) else ""
  in
  if n > 0
  then Printf.sprintf "%s%s" (tab size) (str_tabs ~size (n - 1))
  else ""
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
let new_int_counter ?(start : int = 0) () : unit -> int =
  let id_counter : int ref = ref start in
  let get_and_incr_counter () : int =
    let to_return = !id_counter in
    id_counter := to_return + 1;
    to_return
  in
  get_and_incr_counter
;;

(********************)
(*** pretty print ***)
(********************)

let pstr_string_list (ss : string list) : string =
  match ss with
  | [] -> "[ ] (empty)"
  | h :: [] -> Printf.sprintf "[ %s ]" h
  | h :: t ->
    Printf.sprintf
      "[\n%s\n]"
      (List.fold_left
         (fun (acc : string) (s : string) ->
           Printf.sprintf ",\n%s%s" (str_tabs 1) s)
         (Printf.sprintf "%s%s" (str_tabs 1) h)
         t)
;;
