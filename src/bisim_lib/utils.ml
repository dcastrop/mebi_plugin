(** [print ?show to_print] is a wrapper for [Printf.printf].
    @param ?show determines if [to_print] is outputted. *)
let print ?(show : bool = false) (to_print : string) : unit =
  match show with
  | true -> Printf.printf "%s" to_print
  | false -> ()
;;

(** [default_indent_val] is the default number of spaces to use perindent in [to_string]. *)
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

(** [get_key_of_val tbl v] is a reverse-lookup in [tbl] for the key of value [v]. *)
let get_key_of_val (tbl : ('a, 'b) Hashtbl.t) (v : 'b) : 'a option =
  match
    List.find_opt
      (fun ((_key, value) : 'a * 'b) -> v == value)
      (List.of_seq (Hashtbl.to_seq tbl))
  with
  | None -> None
  | Some (key, _value) -> Some key
;;
