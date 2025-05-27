open Pp

type mebi_error =
  | InvalidLTSSort of Sorts.family
  | InvalidArity of Environ.env * Evd.evar_map * Constr.types
  | InvalidLTSRef of Names.GlobRef.t
  | UnknownTermType of
      (Environ.env * Evd.evar_map * (EConstr.t * EConstr.t * EConstr.t list))
  | PrimaryLTSNotFound of
      (Environ.env * Evd.evar_map * EConstr.t * EConstr.t list)
  | UnknownDecodeKey of
      (Environ.env * Evd.evar_map * 'a * EConstr.t list)

exception MEBI_exn of mebi_error

(** Error when input LTS has the wrong arity *)
let invalid_sort f = MEBI_exn (InvalidLTSSort f)
(** Error when input LTS has the wrong Sort *)
let invalid_arity ev sg t = MEBI_exn (InvalidArity (ev, sg, t))
(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_ref r = MEBI_exn (InvalidLTSRef r)

(** Error when term is of unknown type *)
let unknown_term_type ev sg tmty =
  MEBI_exn (UnknownTermType (ev, sg, tmty))
;;

(** Error when multiple coq-LTS provided, but none of them match term. *)
let primary_lts_not_found ev sg t names =
  MEBI_exn (PrimaryLTSNotFound (ev, sg, t, names))
;;

(** Error when multiple coq-LTS provided, but none of them match term. *)
let unknown_decode_key ev sg t names =
  MEBI_exn (UnknownDecodeKey (ev, sg, t, names))
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
  | UnknownTermType (ev, sg, (tm, ty, trkeys)) ->
    str
      "None of the constructors provided matched type of term to visit. \
       (unknown_term_type) "
    ++ strbrk "\n\n"
    ++ str "Term: "
    ++ Printer.pr_econstr_env ev sg tm
    ++ strbrk "\n\n"
    ++ str "Type: "
    ++ Printer.pr_econstr_env ev sg ty
    ++ strbrk "\n\n"
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
                        (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg k)))
                    ""
                    trkeys)))
    ++ strbrk "\n\n"
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
  | PrimaryLTSNotFound (ev, sg, t, names) -> str "..."
;;

let _ =
  CErrors.register_handler (fun e ->
    match e with MEBI_exn e -> Some (mebi_handler e) | _ -> None)
;;
