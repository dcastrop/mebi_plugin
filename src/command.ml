open Pp
open Mebi_utils
module Err = Mebi_errors

let arity_is_Prop mip =
  match Inductive.inductive_sort_family mip with
  | Sorts.InProp -> ()
  | family -> raise (Err.invalid_sort family)
;;

let get_lts_labels_and_terms env sigma mib mip =
  let open Declarations in
  let typ = Inductive.type_of_inductive (UVars.in_punivs (mib, mip)) in
  let i_ctx = mip.mind_arity_ctxt in
  let _, i_idx = split_at mip.mind_nrealdecls i_ctx [] in
  match i_idx with
  | [ t1; a; t2 ] ->
    let open Context.Rel in
    if Declaration.equal Constr.equal t1 t2
    then a, t1
    else raise (Err.invalid_arity env sigma typ)
  | _ -> raise (Err.invalid_arity env sigma typ)
;;

let check_ref_lts env sigma gref =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let mib, mip = Inductive.lookup_mind_specif env i in
    arity_is_Prop mip;
    let lbl, term = get_lts_labels_and_terms env sigma mib mip in
    let univ = mib.mind_univ_hyps in
    let lts = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    ( lts
    , EConstr.of_constr (Context.Rel.Declaration.get_type lbl)
    , EConstr.of_constr (Context.Rel.Declaration.get_type term) )
  | _ -> raise (Err.invalid_ref gref)
;;

let get_constructors env sigma gref =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let _, mip = Inductive.lookup_mind_specif env i in
    mip.mind_consnames, mip.mind_nf_lc
  | _ -> assert false
;;

(** Checks if two terms unify
    TODO: lots of doubts
    - Conversion.CUMUL?
    - Is [w_unify] the best way?
    - ... *)
let m_unify env sigma t0 t1 =
  try
    let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
    Some sigma
  with
  | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify _) -> None
;;

(** Generates [LTS term ?act ?term2] for unification *)
let mk_template env sigma lts termL lbl_ty term_ty =
  let sigma, act = Evarutil.new_evar env sigma lbl_ty in
  let sigma, termR = Evarutil.new_evar env sigma term_ty in
  let template = EConstr.mkApp (lts, [| termL; act; termR |]) in
  sigma, template
;;

(** Checks possible transitions for this term: *)
let check_valid_constructor env sigma lts t term_ty lbl_ty transitions =
  Array.fold_left
    (fun (sigma, acc) tm ->
      let sigma, to_unif = mk_template env sigma lts t lbl_ty term_ty in
      match m_unify env sigma to_unif (EConstr.of_constr (snd tm)) with
      | Some sigma -> sigma, acc + 1
      | None -> sigma, acc)
    (sigma, 0)
    transitions
;;

(* END FIXME *)

(* TODO: check which are all possible next transitions *)
(* TODO: check following functions/modules: *)
(* [ ] Unification *)
(* [ ] Reductionops.infer_conv *)
(*  *)

(** Builds an LTS from a Term [t : T] and an LTS [P : forall Ts, T -> A -> T -> Prop]

    Constraints:
    - [ T \& A \not\in Ts ]

    Notes:
    - Constructors of [P] are the transitions
    - States are the sets of possible transitions
    - A term [t] is represented by the state of the transitions that can be taken *)
let lts (iref : Names.GlobRef.t) (tref : Constrexpr.constr_expr_r CAst.t) : unit
  =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let lts_ty, lbls, terms = check_ref_lts env sigma iref in
  let sigma, t = Constrintern.interp_constr_evars env sigma tref in
  let sigma = Typing.check env sigma t terms in
  let c_names, transitions = get_constructors env sigma iref in
  let sigma, constrs =
    check_valid_constructor env sigma lts_ty t terms lbls transitions
  in
  Feedback.msg_notice
    (str "Types of terms: "
     ++ Printer.pr_econstr_env env sigma terms
     ++ strbrk "");
  Feedback.msg_notice
    (str "Types of labels: "
     ++ Printer.pr_econstr_env env sigma lbls
     ++ strbrk "");
  Feedback.msg_notice
    (str "Constructors: "
     ++ Pp.prvect_with_sep (fun _ -> str ", ") Names.Id.print c_names);
  Feedback.msg_notice
    (str "Transitions: "
     ++ Pp.prvect_with_sep
          (fun _ -> strbrk "\n")
          (fun t -> Printer.pr_constr_env env sigma (snd t))
          transitions);
  Feedback.msg_notice (str "Target matches: " ++ Pp.int constrs)
;;
