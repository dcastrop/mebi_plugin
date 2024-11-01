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
    (* lts of inductive type *)
    let lts = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    ( lts
    , EConstr.of_constr (Context.Rel.Declaration.get_type lbl)
    , EConstr.of_constr (Context.Rel.Declaration.get_type term) )
  | _ -> raise (Err.invalid_ref gref)
;;

(* let rec instantiate_ctor_args env sigma t = *)
(*   let open Constr in *)
(*   match kind t with *)
(*   (\* ∀ (a : b), c *\) *)
(*   | Prod (a, b, c) ->  *)
(*     let sigma, ea = Evarutil.new_evar env sigma b in *)

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
  sigma, termR, template
;;

(* Can I instantiate the bound variables with metavariables instead? *)
let rec instantiate_ctx env sigma (c : EConstr.t) = function
  | [] -> sigma, c
  | t :: ts ->
    let sigma, vt = Evarutil.new_evar env sigma t in
    instantiate_ctx env sigma (EConstr.Vars.subst1 vt c) ts
;;

(** Checks possible transitions for this term: *)
let check_valid_constructor env sigma lts t term_ty lbl_ty transitions =
  let (ctors : (Evd.evar_map * (EConstr.t * EConstr.t) list) ref) =
    { contents = sigma, [] } in
  for i = 0 to Array.length transitions - 1 do
    let sigma, ctor_vals = !ctors in
    let ctx, tm = transitions.(i) in
    let tm = EConstr.of_constr tm in
    (* Feedback.msg_notice (str "num_ctx: " ++ Pp.int n_ctx); *)
    let ctx_tys = List.map Context.Rel.Declaration.get_type ctx in
    let sigma, tm =
      instantiate_ctx env sigma tm (List.map EConstr.of_constr ctx_tys)
    in
    let sigma, tgt_term, to_unif = mk_template env sigma lts t lbl_ty term_ty in
    match m_unify env sigma to_unif tm with
    | Some sigma -> ctors := sigma, (tgt_term, to_unif) :: ctor_vals
    | None -> ()
  done;
  !ctors
;;

(* END FIXME *)

(* pp individual edges *)
let pp_constr_edge env sigma term =
  (Printer.pr_econstr_env env sigma (fst term))
    ++ (str " :: ")
    ++ (Printer.pr_econstr_env env sigma (snd term))
;;

(* pp list containing all edges of one constr *)
let rec pp_constr_edges env sigma constrs = 
  match constrs with
  | [] -> []
  | h::t_constrs ->
    (pp_constr_edge env sigma h)
    ::(pp_constr_edges env sigma t_constrs)
;;

(* pp list containing all edges of each constrs *)
let rec pp_list_outgoing_edges env sigma lts_ty constrs terms lbls transitions =
  match constrs with
    | [] -> []
    (* | (h_term,_)::t -> *)
    | h_term::t ->
      let sigma, h_constrs = 
        check_valid_constructor env sigma lts_ty h_term terms lbls transitions
      in 
        (* pp h_constrs *)
        (pp_constr_edges env sigma h_constrs)
        (* repeat on tail *)
        ::(pp_list_outgoing_edges env sigma lts_ty t terms lbls transitions)
;;

(* pp str of all edges of each constrs *)
(* 
  TODO: what does strbrk even do? 

  i've been trying to get things to print on separate lines, 
  but the more \n i add the worse it looks -- the end half of
  lines keep then appearing on the rhs side of the feedback.

  i've then tried to use strbrk since the name suggests something
  to do with "string break". maybe a signal of where it can 
  insert a breakline? without any \n it prints perfectly, but i
  would prefer to be able to list them vertically, all aligned.

  --- oh i see
  there are specific commands given for this stuff in pp.ml
  -> fnl "force new line"
  -> brk "print line break"

  i think strbrk is just for "optional" breaking at the right point.
*)
let pp_edges env sigma lts_ty constrs terms lbls transitions =
    (Pp.prlist_with_sep
      (* sep  *) (fun _ -> str "," ++ fnl())
      (* fun  *) (fun i -> str "  " ++ i)
      (* list *) (List.concat (pp_list_outgoing_edges env sigma lts_ty constrs terms lbls transitions))
      (* list constrs *)
    )
;;
(* 
(* gets outgoing edges from current constr *)
let rec outgoing_edges env sigma lts_ty constrs terms lbls transitions =
  match constrs with
    | [] -> []
    | (h_term,_)::t_constrs ->
      let sigma, h_constrs = 
        check_valid_constructor env sigma lts_ty h_term terms lbls transitions
      in 
        h_constrs
        ::(outgoing_edges env sigma lts_ty t_constrs terms lbls transitions)
;;
  
let pp_outgoing_edges env sigma lts_ty constrs terms lbls transitions =
  (Pp.prlist_with_sep
      (* sep  *) (fun _ -> str "," ++ fnl())
      (* fun  *) (fun i -> str "  " ++ i)
      (* list *) (pp_list_outgoing_edges env sigma lts_ty (List.concat (outgoing_edges env sigma lts_ty constrs terms lbls transitions)) terms lbls transitions)
      
      
    )
;; *)

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
(* 
  (* THIS IS A HACK TO TEST STUFF! *)
  (* We can now recursively apply check_valid_constructor to build an LTS *)
  let sigma, constrs' =
    match constrs with
    | [] -> sigma, []
    (* get head of list *)
    | (hack_tm, hack_test) :: _ ->
      check_valid_constructor env sigma lts_ty hack_tm terms lbls transitions
  in
  (* END OF HACK TO TEST STUFF! *)
   *)
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

  (* Q: what is different about these transitions and the constrs below? *)
  Feedback.msg_notice
    (str "Transitions: "
     ++ Pp.prvect_with_sep
          (fun _ -> str ", ")
          (fun t -> Printer.pr_constr_env env sigma (snd t))
          transitions);

  (* Feedback.msg_notice
    (str "Target matches constructors  [\n"
     ++ (pp_edges env sigma lts_ty constrs terms lbls transitions)
     ++ strbrk "]\n"); *)

  (* print all outgoing edges *)
  Feedback.msg_notice
     (str "Target (outgoing) matches constructors  [\n" 
     ++ (pp_outgoing_edges env sigma lts_ty constrs terms lbls transitions)
     ++ strbrk "\n]\n");
;;


