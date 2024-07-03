open Pp

type mebi_error =
  | InvalidLTSSort of Sorts.family
  | InvalidArity of Environ.env * Evd.evar_map * Constr.types
  | InvalidLTSRef of Names.GlobRef.t

exception MEBI_exn of mebi_error

let invalid_sort f = MEBI_exn (InvalidLTSSort f)
let invalid_arity ev sg t = MEBI_exn (InvalidArity (ev, sg, t))
let invalid_ref r = MEBI_exn (InvalidLTSRef r)

let mebi_handler = function
  | InvalidLTSSort f ->
    str "Invalid LTS Sort: expecting Prop, got " ++ Sorts.pr_sort_family f
  | InvalidArity (ev, sg, t) ->
    str "Invalid arity for LTS: "
    ++ Printer.pr_constr_env ev sg t
    ++ strbrk "\n"
    ++ str "Expecting: forall params, ?terms -> ?labels -> ?terms -> Prop"
  | InvalidLTSRef r -> str "Invalid LTS ref: " ++ Printer.pr_global r
;;

let _ =
  CErrors.register_handler (fun e ->
    match e with
    | MEBI_exn e -> Some (mebi_handler e)
    | _ -> None)
;;
