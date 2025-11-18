let swap : 'a * 'b -> 'b * 'a = fun (a, b) -> b, a

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

let infix : string -> string = function "" -> "" | p -> Printf.sprintf "%s" p

let prefix : string -> string = function
  | "" -> ""
  | p -> Printf.sprintf "%s, " p
;;

let suffix : string -> string = function
  | "" -> ""
  | p -> Printf.sprintf ", %s" p
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

module Strfy = struct
  (**********************************)
  (****** OCAML *********************)
  (**********************************)

  type collection_delimiter =
    | Comma
    | Semi
    | Colon
    | Line
    | Use of string

  let collection_delimiter : collection_delimiter -> string = function
    | Comma -> ","
    | Semi -> ";"
    | Colon -> ":"
    | Line -> "|"
    | Use x -> x
  ;;

  type collection_marker =
    | Brace
    | Squig
    | Square
    | Angle
    | Use of string * string

  let collection_marker : collection_marker -> string * string = function
    | Brace -> "(", ")"
    | Squig -> "{", "}"
    | Square -> "[", "]"
    | Angle -> "<", ">"
    | Use (a, b) -> a, b
  ;;

  type collection_style =
    { marker : collection_marker
    ; delimiter : collection_delimiter
    ; inline : bool
    ; size : bool
    }

  let delimiter : collection_style -> string = function
    | { delimiter; inline = true; _ } -> collection_delimiter delimiter
    | { delimiter; inline = false; _ } ->
      Printf.sprintf "\n%s" (collection_delimiter delimiter)
  ;;

  let marker : collection_style -> string * string = function
    | { marker; inline = true; _ } -> collection_marker marker
    | { marker; inline = false; _ } ->
      let lhs, rhs = collection_marker marker in
      Printf.sprintf "\n%s" lhs, Printf.sprintf "\n%s" rhs
  ;;

  type collection_kind =
    | Tuple
    | Record
    | List
    | Use of collection_style

  let collection_style : collection_kind -> collection_style = function
    | Tuple ->
      { marker = Brace; delimiter = Comma; inline = true; size = false }
    | Record ->
      { marker = Squig; delimiter = Semi; inline = false; size = false }
    | List -> { marker = Square; delimiter = Semi; inline = false; size = true }
    | Use x -> x
  ;;

  (** ..
      @param nested
        is is a flag set to true by collection, and is used by other collections to determine how to format their opening marker.
  *)
  type style_args =
    { mutable indent : int
    ; mutable newline : bool
    ; mutable nested : bool
    ; name : string option
    }

  let style_args
        ?(indent : int = 0)
        ?(newline : bool = false)
        ?(nested : bool = false)
        ?(name : string = String.empty)
        ()
    : style_args
    =
    { indent
    ; newline
    ; nested
    ; name = (if String.equal name String.empty then None else Some name)
    }
  ;;

  (** newline indent *)
  let nlindent (indent : int) : string = Printf.sprintf "\n%s" (str_tabs indent)

  let mkindent (n : int) : bool -> string = function
    | true -> nlindent n
    | false -> str_tabs n
  ;;

  let empty_msg : style_args -> string = function
    | { name = None; _ } -> "empty"
    | { name = Some name; _ } -> Printf.sprintf "%s: empty" name
  ;;

  let size_msg (size : int) : style_args -> string = function
    | { name = None; _ } -> Printf.sprintf "Size: %i" size
    | { name = Some name; _ } -> Printf.sprintf "%s: %i" name size
  ;;

  let prefix : style_args -> string = function
    | { indent; newline; _ } -> mkindent indent newline
  ;;

  let push (args : style_args) : style_args =
    args.indent <- args.indent + 1;
    args.nested <- true;
    args
  ;;

  let _pull (args : style_args) : style_args =
    match args with
    | { indent = 0; _ } -> args
    | _ ->
      args.indent <- args.indent - 1;
      args
  ;;

  let nest (args : style_args) : style_args =
    push { args with newline = false; nested = true }
  ;;

  let wrap (style : collection_style) (args : style_args)
    : string list -> string
    =
    let prefix : string = prefix args in
    let ((lhs, rhs) : string * string) = marker style in
    let delim : string = delimiter style in
    let f : string -> string -> string =
      fun acc a -> Printf.sprintf "%s%s %s" acc delim a
    in
    function
    | [] -> Printf.sprintf "%s%s %s (%s)" prefix lhs rhs (empty_msg args)
    | h :: t ->
      Printf.sprintf
        "%s%s %s %s (%s)"
        prefix
        lhs
        (List.fold_left f h t)
        rhs
        (size_msg (List.length t + 1) args)
  ;;

  let str ?(args : style_args = style_args ()) : string -> string =
    fun (x : string) -> x
  ;;

  let int ?(args : style_args = style_args ()) : int -> string =
    fun (x : int) -> Printf.sprintf "%i" x
  ;;

  let bool ?(args : style_args = style_args ()) : bool -> string =
    fun (x : bool) -> Printf.sprintf "%b" x
  ;;

  let option
        (f : ?args:style_args -> 'a -> string)
        ?(args : style_args = style_args ())
    : 'a option -> string
    = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some %s" (f ~args x)
  ;;

  let tuple
        (f : ?args:style_args -> 'a -> string)
        (g : ?args:style_args -> 'b -> string)
        ?(style : collection_style = collection_style Tuple)
        ?(args : style_args = style_args ())
    : 'a * 'b -> string
    =
    fun ((a, b) : 'a * 'b) ->
    let astr : string = f ~args:(nest args) a in
    let bstr : string = g ~args:(nest args) b in
    wrap style args [ astr; bstr ]
  ;;

  let keyval
        (f : ?args:style_args -> 'a -> string)
        ?(style : collection_style = collection_style Record)
        ?(args : style_args = style_args ())
    : string * 'a -> string
    =
    tuple ~style:{ style with inline = true; delimiter = Colon } ~args str f
  ;;

  let list
        (f : ?args:style_args -> 'a -> string)
        ?(style : collection_style = collection_style List)
        ?(args : style_args = style_args ())
        (alist : 'a list)
    : string
    =
    wrap style args (List.map (fun a -> f ~args:(nest args) a) alist)
  ;;

  let array
        (f : ?args:style_args -> 'a -> string)
        ?(style : collection_style = collection_style List)
        ?(args : style_args = style_args ())
        (arr : 'a array)
    : string
    =
    list ~style ~args f (Array.to_list arr)
  ;;

  let record
        ?(style : collection_style = collection_style Record)
        ?(args : style_args = style_args ())
        (alist : (string * string) list)
    : string
    =
    list ~style ~args (keyval ~style str) alist
  ;;

  (* new line sep *)
  (* let nlsep ?(force_newline : bool = false) ?(indent : int = 0) () : string =
    if force_newline then Printf.sprintf "\n%s" (str_tabs indent) else ""
  ;; *)

  (* let tuple
     ?(force_newline : bool = false)
     ?(is_keyval : bool = false)
     ?(indent : int = 0)
     (f : 'a -> string)
     (g : 'b -> string)
     : 'a * 'b -> string
     =
     fun ((a, b) : 'a * 'b) ->
     let sep, con, lhs, rhs =
     match force_newline, is_keyval with
     | true, false -> nlsep ~force_newline:true ~indent (), ",", "( ", ")"
     | _, false -> nlsep ~force_newline ~indent (), ",", "( ", " )"
     | _, true -> nlsep ~force_newline ~indent (), ":", "", ""
     in
     Printf.sprintf "%s%s%s%s %s%s%s" lhs (f a) sep con (g b) sep rhs
     ;; *)

  (* let list
     ?(force_newline : bool = false)
     ?(label : string = "List")
     ?(indent : int = 0)
     ?(use : string * string = "[", "]")
     (strfy : 'a -> string)
     : 'a list -> string
     = function
     | [] ->
     let lhs, rhs = use in
     Printf.sprintf "%s %s (%s empty)" lhs rhs label
     | h :: [] ->
     let lhs, rhs = use in
     let sep = nlsep ~force_newline ~indent () in
     let suffix =
     if use = ("[", "]") then Printf.sprintf "(%s: 1)" label else ""
     in
     Printf.sprintf "%s %s %s%s %s" lhs (strfy h) sep rhs suffix
     | h :: t ->
     let lhs, rhs = use in
     let sep = nlsep ~force_newline ~indent () in
     let len = List.length t + 1 in
     let suffix =
     if use = ("[", "]") then Printf.sprintf "(%s: %i)" label len else ""
     in
     let lstr =
     List.fold_left
     (fun (acc : string) (e : 'a) ->
     Printf.sprintf "%s%s; %s" acc sep (strfy e))
     (strfy h)
     t
     in
     Printf.sprintf "%s %s %s%s %s" lhs lstr sep rhs suffix
     ;; *)

  (* let array
     ?(force_newline : bool = false)
     ?(label : string = "List")
     ?(indent : int = 0)
     ?(use : string * string = "[", "]")
     (strfy : 'a -> string)
     (arr : 'a array)
     : string
     =
     list ~force_newline ~label ~indent ~use strfy (Array.to_list arr)
     ;; *)
end
