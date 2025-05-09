open Pp

type mebi_error =
  | InvalidLTSSort of Sorts.family
  | InvalidArity of Environ.env * Evd.evar_map * Constr.types
  | InvalidLTSRef of Names.GlobRef.t
  | CoqTermDoesNotMatchSemantics of
      (Environ.env * Evd.evar_map * (EConstr.t * EConstr.t * EConstr.t list))
  | CannotFindTypeOfTermToVisit of
      (Environ.env * Evd.evar_map * (EConstr.t * EConstr.t * EConstr.t list))

exception MEBI_exn of mebi_error

let invalid_sort f = MEBI_exn (InvalidLTSSort f)
let invalid_arity ev sg t = MEBI_exn (InvalidArity (ev, sg, t))
let invalid_ref r = MEBI_exn (InvalidLTSRef r)

(** for the first term *)
let unknown_tref_type ev sg tmty =
  MEBI_exn (CoqTermDoesNotMatchSemantics (ev, sg, tmty))
;;

(** for any terms encountered when exploring/building the lts *)
let unknown_term_type ev sg tmty =
  MEBI_exn (CannotFindTypeOfTermToVisit (ev, sg, tmty))
;;

let mebi_handler = function
  | InvalidLTSSort f ->
    str "Invalid LTS Sort: expecting Prop, got " ++ Sorts.pr_sort_family f
  | InvalidArity (ev, sg, t) ->
    str "Invalid arity for LTS: "
    ++ Printer.pr_constr_env ev sg t
    ++ strbrk "\n"
    ++ str "Expecting: forall params, ?terms -> ?labels -> ?terms -> Prop"
  | InvalidLTSRef r -> str "Invalid LTS ref: " ++ Printer.pr_global r
  | CannotFindTypeOfTermToVisit (ev, sg, (tm, ty, trkeys)) ->
    str
      "None of the constructors provided matched type of term to visit. \
       (unknown_term_type) "
    ++ strbrk "\n"
    ++ str "Term: "
    ++ Printer.pr_econstr_env ev sg tm
    ++ strbrk "\n"
    ++ str "Type: "
    ++ Printer.pr_econstr_env ev sg ty
    ++ strbrk "\n"
    ++ str
         (Printf.sprintf
            "Keys: %s"
            (if List.is_empty trkeys
             then "[ ] (empty)"
             else
               Printf.sprintf
                 "[%s ]"
                 (List.fold_left
                    (fun (acc : string) (k : EConstr.t) ->
                      Printf.sprintf
                        "%s '%s'"
                        acc
                        (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg ty)))
                    ""
                    trkeys)))
    ++ strbrk "\n"
    ++ str
         (Printf.sprintf
            "Does Type match EConstr of any Key? = %b"
            (List.exists
               (fun (k : EConstr.t) -> EConstr.eq_constr sg ty k)
               trkeys))
    ++ strbrk "\n"
    ++ str
         (let tystr = Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg ty) in
          Printf.sprintf
            "Does Type match String of any Key? = %b"
            (List.exists
               (fun (k : EConstr.t) ->
                 String.equal
                   tystr
                   (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg k)))
               trkeys))
  | CoqTermDoesNotMatchSemantics (ev, sg, (tr, ty, trkeys)) ->
    str
      "None of the constructors provided matched type of term to visit. \
       (unknown_tref_type) "
    ++ strbrk "\n"
    ++ str "Tref: "
    ++ Printer.pr_econstr_env ev sg tr
    ++ strbrk "\n"
    ++ str "Type: "
    ++ Printer.pr_econstr_env ev sg ty
    ++ strbrk "\n"
    ++ str
         (Printf.sprintf
            "Keys: %s"
            (if List.is_empty trkeys
             then "[ ] (empty)"
             else
               Printf.sprintf
                 "[%s ]"
                 (List.fold_left
                    (fun (acc : string) (k : EConstr.t) ->
                      Printf.sprintf
                        "%s '%s'"
                        acc
                        (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg ty)))
                    ""
                    trkeys)))
    ++ strbrk "\n"
    ++ str
         (Printf.sprintf
            "Does Type match EConstr of any Key? = %b"
            (List.exists
               (fun (k : EConstr.t) -> EConstr.eq_constr sg ty k)
               trkeys))
    ++ strbrk "\n"
    ++ str
         (Printf.sprintf
            "Does Type match String of any Key? = %b"
            (let tystr =
               Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg ty)
             in
             List.exists
               (fun (k : EConstr.t) ->
                 String.equal
                   tystr
                   (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg k)))
               trkeys))
;;

let _ =
  CErrors.register_handler (fun e ->
    match e with
    | MEBI_exn e -> Some (mebi_handler e)
    | _ -> None)
;;
