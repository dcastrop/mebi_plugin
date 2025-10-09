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

let ppstr (x : Pp.t) : string = Pp.string_of_ppcmds x

(********************)
(*** pretty print ***)
(********************)

let pstr_list
      ?(force_newline : bool = false)
      ?(empty_msg : string = "empty")
      ?(indent : int = 0)
      (strfy : 'a -> string)
  : 'a list -> string
  = function
  | [] ->
    Printf.sprintf
      "%s[ ] (%s)"
      (if force_newline then Printf.sprintf "\n%s" (str_tabs indent) else "")
      empty_msg
  | h :: [] ->
    Printf.sprintf
      "%s[ %s %s] (len 1)"
      (if force_newline then Printf.sprintf "\n%s" (str_tabs indent) else "")
      (strfy h)
      (if force_newline then Printf.sprintf "\n%s" (str_tabs indent) else "")
  | h :: t ->
    Printf.sprintf
      "\n%s[ %s\n%s] (len %i)"
      (str_tabs indent)
      (List.fold_left
         (fun (acc : string) (e : 'a) ->
           Printf.sprintf "%s\n%s; %s" acc (str_tabs indent) (strfy e))
         (strfy h)
         t)
      (str_tabs indent)
      (List.length t + 1)
;;

let pstr_list2
      ?(empty_msgA : string = "empty")
      ?(empty_msgB : string = "empty")
      ?(indent : int = 0)
      (strfyA : 'b -> string)
      (strfyB : 'c -> string)
  : ('b list * 'c list) list -> string
  = function
  | [] -> Printf.sprintf "\n[ ] (empty)"
  | (hA, hB) :: [] ->
    Printf.sprintf
      "\n%s[ %s\n%s, %s\n%s] (len 1)"
      (str_tabs indent)
      (pstr_list
         ~force_newline:true
         ~empty_msg:empty_msgA
         ~indent:(indent + 1)
         strfyA
         hA)
      (str_tabs (indent + 1))
      (pstr_list
         ~force_newline:true
         ~empty_msg:empty_msgB
         ~indent:(indent + 1)
         strfyB
         hB)
      (str_tabs indent)
  | (hA, hB) :: t ->
    Printf.sprintf
      "\n%s[ %s\n] (len %i)"
      (str_tabs indent)
      (List.fold_left
         (fun (acc : string) (e : 'a) ->
           Printf.sprintf
             "%s\n%s; %s\n%s, %s"
             acc
             (str_tabs indent)
             (pstr_list
                ~force_newline:true
                ~empty_msg:empty_msgA
                ~indent:(indent + 1)
                strfyA
                hA)
             (str_tabs (indent + 1))
             (pstr_list
                ~force_newline:true
                ~empty_msg:empty_msgB
                ~indent:(indent + 1)
                strfyB
                hB))
         (Printf.sprintf
            "\n%s[ %s\n%s; %s\n%s] (len 1)"
            (str_tabs indent)
            (pstr_list ~force_newline:true ~indent:(indent + 1) strfyA hA)
            (str_tabs indent)
            (pstr_list ~force_newline:true ~indent:(indent + 1) strfyB hB)
            (str_tabs indent))
         t)
      (List.length t + 1)
;;

module Strfy = struct
  let str : string -> string = fun (x : string) -> x

  let option (f : 'a -> string) : 'a option -> string = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some %s" (f x)
  ;;

  let evar : Evar.t -> string = fun (x : Evar.t) -> ppstr (Evar.print x)

  let constr env sigma : Constr.t -> string =
    fun (x : Constr.t) -> ppstr (Printer.pr_constr_env env sigma x)
  ;;

  let constr_opt env sigma : Constr.t option -> string =
    fun (x : Constr.t option) -> option (constr env sigma) x
  ;;

  let constr_rel_decl env sigma : Constr.rel_declaration -> string =
    fun (x : Constr.rel_declaration) -> ppstr (Printer.pr_rel_decl env sigma x)
  ;;

  let econstr env sigma : EConstr.t -> string =
    fun (x : EConstr.t) -> ppstr (Printer.pr_econstr_env env sigma x)
  ;;

  let econstr_rel_decl env sigma : EConstr.rel_declaration -> string =
    fun (x : EConstr.rel_declaration) ->
    ppstr (Printer.pr_erel_decl env sigma x)
  ;;

  let goal env sigma : Evar.t -> string =
    fun (x : Evar.t) -> ppstr (Printer.pr_existential_key env sigma x)
  ;;

  let global : Names.GlobRef.t -> string =
    fun (x : Names.GlobRef.t) -> ppstr (Printer.pr_global x)
  ;;
end
