(** [split_at i l acc] is a tuple containing two lists [(l', acc)] split from list [l] at index [i]. *)
let rec split_at i l acc =
  if i <= 0
  then l, acc
  else (
    match l with
    | [] -> acc, []
    | h :: t -> split_at (i - 1) t (h :: acc))
;;

(** [strip_snd l] is the list of rhs elements in a list of tuples [l] (typically a [constr]). *)
let rec strip_snd (l : (Evd.econstr * Evd.econstr) list) : Evd.econstr list =
  match l with
  | [] -> []
  | h :: t -> snd h :: strip_snd t
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
