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
let rec strip_snd (l : (EConstr.t * EConstr.t) list) : EConstr.t list =
  match l with
  | [] -> []
  | h :: t -> snd h :: strip_snd t
;;

(** used by [g_mebi.mlg]*)
let ref_list_to_glob_list (l : Libnames.qualid list) : Names.GlobRef.t list =
  List.fold_left
    (fun (acc : Names.GlobRef.t list) (s : Libnames.qualid) ->
      List.append acc [ Nametab.global s ])
    []
    l
;;

(** ['a mm] is a function type mapping from [coq_context ref] to ['a in_context]. *)
type 'a mm = 'a Mebi_monad.t

(* TODO: should maybe be moved to [mebi_monad.ml]? *)

(** [econstr_to_string target] is a [string] representing [target]. *)
let econstr_to_string (target : EConstr.t) : string =
  let open Mebi_monad in
  let open Mebi_monad.Monad_syntax in
  let econstr_to_string' (target : EConstr.t) : string mm =
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma target))
  in
  run (econstr_to_string' target)
;;

(** [constr_to_string target] is a [string] representing [target]. *)
let constr_to_string (target : Constr.t) : string =
  let open Mebi_monad in
  let open Mebi_monad.Monad_syntax in
  let constr_to_string' (target : Constr.t) : string mm =
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_constr_env env sigma target))
  in
  run (constr_to_string' target)
;;
