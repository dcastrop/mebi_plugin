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

let ppstr ?(clean : bool = true) (x : Pp.t) : string =
  let s = Pp.string_of_ppcmds x in
  if clean then clean_string s else s
;;

(* new line sep *)
let nlsep ?(force_newline : bool = false) ?(indent : int = 0) () : string =
  if force_newline then Printf.sprintf "\n%s" (str_tabs indent) else ""
;;

let pstr_list
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
    let sep : string = nlsep ~force_newline ~indent () in
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
;;

(*
   let pstr_list2
   ?(empty_msgA : string = "empty")
   ?(empty_msgB : string = "empty")
   ?(indent : int = 0)
   (strfyA : 'a -> string)
   (strfyB : 'b -> string)
   : ('a list * 'b list) list -> string
   = function
   | [] -> Printf.sprintf "%s[ ] (empty)" (nlsep ~force_newline:true ~indent ())
   | h :: [] ->
   let sep : string = nlsep ~force_newline:true ~indent () in
   let indent : int = indent + 1 in
   let pstr_list_a = pstr_list ~force_newline:true ~indent strfyA in
   let pstr_list_b = pstr_list ~force_newline:true ~indent strfyB in
   let pstr_e =
   Strfy.tuple
   ~force_newline:true
   ~indent
   (fun (a : 'a list) -> pstr_list_a a)
   (fun (b : 'b list) -> pstr_list_b b)
   in
   Printf.sprintf "%s[ %s %s] (len 1)" sep (pstr_e h) sep
   | h :: t ->
   let sep : string = nlsep ~force_newline:true ~indent () in
   let indent : int = indent + 1 in
   let pstr_list_a = pstr_list ~force_newline:true ~indent strfyA in
   let pstr_list_b = pstr_list ~force_newline:true ~indent strfyB in
   let pstr_e =
   Strfy.tuple
   ~force_newline:true
   ~indent
   (fun (a : 'a list) -> pstr_list_a a)
   (fun (b : 'b list) -> pstr_list_b b)
   in
   Printf.sprintf
   "%s[ %s %s] (len %i)"
   sep
   (List.fold_left
   (fun (acc : string) (e : 'a * 'b) ->
   Printf.sprintf "%s%s; %s" acc sep (pstr_e e))
   (pstr_e h)
   t)
   sep
   (List.length t + 1)
   ;; *)

module Strfy = struct
  let str : string -> string = fun (x : string) -> x
  let int : int -> string = fun (x : int) -> Printf.sprintf "%i" x
  let bool : bool -> string = fun (x : bool) -> Printf.sprintf "%b" x

  let option (f : 'a -> string) : 'a option -> string = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some %s" (f x)
  ;;

  let tuple
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
  ;;

  let evar : Evar.t -> string = fun (x : Evar.t) -> ppstr (Evar.print x)

  let evar' env sigma : Evar.t -> string =
    fun (x : Evar.t) -> ppstr (Printer.pr_existential_key env sigma x)
  ;;

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

  let econstr_types ?(indent : int = 0) env sigma : EConstr.types -> string =
    fun (x : EConstr.types) ->
    match EConstr.kind_of_type sigma x with
    | AtomicType (ty, tys) ->
      Printf.sprintf
        "%s => \n%s%s"
        (econstr env sigma ty)
        (str_tabs indent)
        (pstr_list
           ~force_newline:true
           ~label:"Type Arguments"
           ~indent
           (econstr env sigma)
           (Array.to_list tys))
    | CastType (_tys, _ty) -> "TODO: CastType"
    | LetInType (_name_binder_annot, _t1, _t2, _t3) -> "TODO: LetInType"
    | ProdType (_name_binder_annot, _t1, _t2) -> "TODO: ProdType"
    | SortType _sorts -> "TODO: SortType"
  ;;

  let name_id : Names.Id.t -> string = Names.Id.to_string

  let global : Names.GlobRef.t -> string =
    fun (x : Names.GlobRef.t) -> ppstr (Printer.pr_global x)
  ;;

  let concl env sigma : EConstr.constr -> string =
    fun (x : EConstr.constr) -> econstr env sigma x
  ;;

  let erel _env sigma : EConstr.ERelevance.t -> string =
    fun (x : EConstr.ERelevance.t) ->
    if EConstr.ERelevance.is_irrelevant sigma x
    then "irrelevant"
    else "relevant"
  ;;

  type hyp =
    ( EConstr.constr
      , EConstr.types
      , EConstr.ERelevance.t )
      Context.Named.Declaration.pt

  let hyp ?(force_newline : bool = false) ?(indent : int = 0) env sigma
    : hyp -> string
    =
    fun (x : hyp) ->
    pstr_list
      ~force_newline:true
      ~indent:(indent + 1)
      ~use:("{", "}")
      (tuple ~is_keyval:true ~indent:(indent + 0) str str)
      [ "name", name_id (Context.Named.Declaration.get_id x)
      ; "rel", erel env sigma (Context.Named.Declaration.get_relevance x)
      ; ( "tys"
        , econstr_types
            ~indent:(indent + 2)
            env
            sigma
            (Context.Named.Declaration.get_type x) )
      ]
  ;;

  let goal ?(indent : int = 0) : Proofview.Goal.t -> string =
    fun (x : Proofview.Goal.t) ->
    let env : Environ.env = Proofview.Goal.env x in
    let sigma : Evd.evar_map = Proofview.Goal.sigma x in
    let concl_str = concl env sigma (Proofview.Goal.concl x) in
    let hyps_str =
      Printf.sprintf
        "\n%s%s"
        (str_tabs (indent + 3))
        (pstr_list
           ~force_newline:true
           ~label:"Hypotheses"
           ~indent:(indent + 3)
           (hyp ~force_newline:true ~indent:(indent + 3) env sigma)
           (Proofview.Goal.hyps x))
    in
    Printf.sprintf
      "%s%s"
      (str_tabs indent)
      (pstr_list
         ~force_newline:true
         ~indent:(indent + 2)
         ~use:("{", "}")
         (tuple ~is_keyval:true str str)
         [ "concl", concl_str; "hyps", hyps_str ])
  ;;
end
