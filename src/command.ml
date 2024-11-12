open Pp
open Mebi_utils
module Err = Mebi_errors
open Mebi_structs
(* open Pp_ext *)
(* open Stringify *)
(* open Translation_layer *)
(* open Fsm *)

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

(** [check_valid_constructor end sigma ... transitions to_check] is the tuple containing an updated [sigma] and the possible [transitions] for term [to_check]. *)
let check_valid_constructor
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (lts : Evd.econstr)
  (term_ty : Evd.econstr)
  (lbl_ty : Evd.econstr)
  (transitions : (Constr.rel_context * Constr.t) array)
  (to_check : Evd.econstr)
  : Evd.evar_map * (Evd.econstr * Evd.econstr) list
  =
  let (ctors : (Evd.evar_map * (EConstr.t * EConstr.t) list) ref) =
    { contents = sigma, [] }
  in
  for i = 0 to Array.length transitions - 1 do
    let sigma, ctor_vals = !ctors in
    let ctx, tm = transitions.(i) in
    let tm = EConstr.of_constr tm in
    (* Feedback.msg_notice (str "num_ctx: " ++ Pp.int n_ctx); *)
    let ctx_tys = List.map Context.Rel.Declaration.get_type ctx in
    let sigma, tm =
      instantiate_ctx env sigma tm (List.map EConstr.of_constr ctx_tys)
    in
    let sigma, tgt_term, to_unif =
      mk_template env sigma lts to_check lbl_ty term_ty
    in
    match m_unify env sigma to_unif tm with
    | Some sigma -> ctors := sigma, (tgt_term, to_unif) :: ctor_vals
    | None -> ()
  done;
  !ctors
;;

(** [bound] is the total number of states to be reached by [explore_lts]. *)
let bound : int = 4

let rec bound_search
  ?(states_to_check : Evd.econstr list = [])
  (lts : Mebi_structs.coq_lts)
  : Mebi_structs.coq_lts
  =
  let num_states = List.length lts.states in
  match bound <= num_states with
  | true ->
    (* stop, too many states. *)
    Feedback.msg_debug
      (str
         (Printf.sprintf
            "\n\n\
             ===\n\
             bound_search, Stopping: found reached (%d <= %d). Still had (%d) \
             states to check."
            bound
            num_states
            (List.length states_to_check)));
    lts
  | _ ->
    (* within bounds, continue -> *)
    (match states_to_check with
     | [] ->
       (* no states to check, is this the first time entering? *)
       (match lts.states with
        | [] ->
          (* this is the first time entering, re-enter with [lts.start_term]. *)
          bound_search ~states_to_check:[ lts.start_term ] lts
        | _ ->
          (* no more states to check, we must have finished. *)
          Feedback.msg_debug
            (str
               (Printf.sprintf
                  "\n\n\
                   ===\n\
                   bound_search, Finished: found (%d) states, (%d) edges."
                  (List.length lts.states)
                  (List.length lts.edges)));
          lts)
     | h_to_check :: t_to_check ->
       (* explore more [states_to_check]. *)
       (* update [lts.states] with [h_to_check]. *)
       let lts =
         Mebi_structs.coq_lts_update_states
           lts
           (List.concat [ lts.states; [ h_to_check ] ])
       in
       Feedback.msg_debug
         (str
            (Printf.sprintf
               "\n\
                --\n\
                bound_search, checking state (%s): currently found (%d) \
                states, (%d) edges.\n"
               (Stringify.econstr_to_string lts.env lts.sigma h_to_check)
               (List.length lts.states)
               (List.length lts.edges)));
       (* unpack [lts]. *)
       (match lts with
        | { env
          ; sigma
          ; lts_type
          ; term_type
          ; type_lbls
          ; constr_names
          ; constr_transitions
          ; start_term
          ; states
          ; edges
          ; _
          } ->
          (* get the outgoing edges ([contrs]) from state ([h_to_check]). *)
          let sigma, constrs =
            check_valid_constructor
              env
              sigma
              lts_type
              term_type
              type_lbls
              constr_transitions
              h_to_check
          in
          (* update [sigma] of [lts]. *)
          let lts = Mebi_structs.coq_lts_update_sigma lts sigma in
          (* determine which edges ([constrs]) are new. *)
          let new_edges =
            (*** [new_edges' ?acc edges] is the sublist of [edges] that do
              not appear already in [acc], which is initially [lts.edges].
              (note, we also strip the tuple down to the [snd] item.
              while we could instead keep them both as it would simplify [extract_new_states] (below), in the long run we would have
              to strip them away anyway. so we do it now to make sure
              that what we do in [extract_new_states] works. *)
            let rec new_edges'
              ?(acc : Evd.econstr list = lts.edges)
              (edges : (Evd.econstr * Evd.econstr) list)
              : Evd.econstr list
              =
              (* for each [h_edge], *)
              match edges with
              | [] -> []
              | (_, h_edge) :: t_edges ->
                (* continue with on [t_edges], return [h_edge] and add
                   to [acc] if not already in [acc]
                   (i.e., not already encountered). *)
                let to_add =
                  if econstr_mem env sigma h_edge acc
                  then (
                    Feedback.msg_debug
                      (str
                         (Printf.sprintf
                            "edge (%s) already in acc."
                            (Stringify.econstr_to_string env sigma h_edge)));
                    [])
                  else (
                    Feedback.msg_debug
                      (str
                         (Printf.sprintf
                            "edge (%s) is new!"
                            (Stringify.econstr_to_string env sigma h_edge)));
                    [ h_edge ])
                in
                List.concat
                  [ to_add
                  ; new_edges' ~acc:(List.concat [ acc; to_add ]) t_edges
                  ]
            in
            (* get [new_edges]. *)
            new_edges' ~acc:lts.edges constrs
          in
          (* check [new_edges] for any [new_states_to_check]. *)
          let new_states_to_check =
            (*** [extract_new_states edges acc] is a list of [states] that
              do not appear already in [to_ignore] or [acc], extracted from
              the destination of [edges]. *)
            let rec extract_new_states
              (edges : Evd.econstr list)
              (acc : Evd.econstr list)
              (to_ignore : Evd.econstr list)
              : Evd.econstr list
              =
              match edges with
              | [] ->
                (* return [acc] *)
                acc
              | h_edge :: t_edges ->
                (* extract from states and labels from [h_edge] *)
                let _lhs, _label, rhs =
                  match EConstr.decompose_app sigma h_edge with
                  | h_edge' ->
                    let h_edge'' = Array.to_list (snd h_edge') in
                    (* get [lhs]. *)
                    ( List.nth h_edge'' 0
                    , (* get [label]. *)
                      List.nth h_edge'' 1
                    , (* get [rhs]. *)
                      List.nth h_edge'' 2 )
                in
                (* Feedback.msg_debug
                   (str
                   (Printf.sprintf
                   "bound_search.extract_new_states,\n\
                   \  lhs: %s;\n\
                   \  label: %s;\n\
                   \  rhs: %s.\n"
                   (Stringify.econstr_to_string env sigma _lhs)
                   (Stringify.econstr_to_string env sigma _label)
                   (Stringify.econstr_to_string env sigma rhs))); *)
                (* update [acc] with [rhs] if not already in [acc]. *)
                let acc' =
                  List.concat
                    [ acc
                    ; (if econstr_mem
                            env
                            sigma
                            rhs
                            (List.concat [ acc; to_ignore ])
                       then (
                         Feedback.msg_debug
                           (str
                              (Printf.sprintf
                                 "state (%s) already in acc."
                                 (Stringify.econstr_to_string env sigma rhs)));
                         [])
                       else (
                         Feedback.msg_debug
                           (str
                              (Printf.sprintf
                                 "state (%s) is new!"
                                 (Stringify.econstr_to_string env sigma rhs)));
                         [ rhs ]))
                    ]
                in
                (* continue checking [t_edges]. *)
                extract_new_states t_edges acc' to_ignore
            in
            (* get [new_states_to_check]. *)
            extract_new_states new_edges t_to_check lts.states
          in
          (* update [lts] with [new_edges]. *)
          let lts =
            Mebi_structs.coq_lts_update_edges
              lts
              (List.concat [ lts.edges; new_edges ])
          in
          (* continue exploring *)
          bound_search ~states_to_check:new_states_to_check lts))
;;

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
  (* using [iref], get the following from the relevant type definition in coq. *)
  let lts_type, type_lbls, term_type = check_ref_lts env sigma iref in
  (* fetch the specific valuation [t] of the term/type shown above. *)
  let sigma, t = Constrintern.interp_constr_evars env sigma tref in
  (* ? typing should always pass -- used to update [sigma]. *)
  let sigma = Typing.check env sigma t term_type in
  (* get each of the constructors of the relevant type definition, split across the names and transitions. *)
  let constr_names, constr_transitions = get_constructors env sigma iref in
  (* setup coq lts table containing all necessary meta-information or
     contexts (e.g., env, sigma) which are also to be updated throughout. *)
  let coq_lts =
    Mebi_structs.coq_lts
      env
      sigma
      lts_type
      term_type
      type_lbls
      constr_names
      constr_transitions
      t
  in
  Feedback.msg_debug (str "init: " ++ Pp_ext.pp_coq_lts coq_lts ++ str "\n---\n");
  (* explore using [coq_lts], up to [bound]-many states. *)
  let coq_lts = bound_search coq_lts in
  Feedback.msg_info (str "final: " ++ Pp_ext.pp_coq_lts coq_lts ++ str "\n---\n");
  (* ! remove below
     let sigma, constrs =
     check_valid_constructor
     env
     sigma
     lts_type
     term_type
     type_lbls
     constr_transitions
     t
     in *)
  (* let sigma, coq_fsm =
     explore_lts'
     env
     sigma
     lts_ty
     constrs
     (* (Mebi_utils.strip_snd constrs) *)
     terms
     lbls
     transitions
     ([], [ t ], bound, bound)
     in *)
  (* match coq_fsm with
  | { states; edges; _ } ->
    Feedback.msg_notice (str "(b) Edges: " ++ pp_edges env sigma edges); *)
  (* Feedback.msg_info
     (str "(b) CoqFsm: " ++ pp_coq_fsm env sigma (coq_fsm.states, coq_fsm.edges)); *)
  (* print out other information too *)
  (* Feedback.msg_debug
     (str "terms: " ++ Printer.pr_econstr_env env sigma term_type);
     Feedback.msg_debug (str "lbls: " ++ Printer.pr_econstr_env env sigma type_lbls);
     Feedback.msg_debug
     (str "lts_ty: " ++ Printer.pr_econstr_env env sigma lts_type);
     Feedback.msg_debug (str "t: " ++ Printer.pr_econstr_env env sigma t); *)
  (* tests on edges *)
  (* match coq_fsm.edges with
     | [] -> Feedback.msg_notice (str "coq_fsm.edges empty. cannot continue")
     | h :: _t ->
     Feedback.msg_notice
     (str "h edge: "
     ++ pp_edge env sigma h
     ++ str "\n\ntests: \n"
     ++ str (Printf.sprintf "isApp: %b" (EConstr.isApp sigma h))
     ++ str "\nend of tests.\n"); *)
  (* lts to fsm *)
  (* let _tbl, _fsm =
     lts_to_fsm
     env
     sigma
     lts_ty
     terms
     lbls
     t
     transitions
     coq_fsm.states
     coq_fsm.edges
     in *)
  (* ( Hashtbl.iter (fun x y -> Printf.sprintf "tbl: %s -> %s\n" x y) _tbl.state_map;;); *)
  (* Feedback.msg_notice
     (str (Printf.sprintf "translated fsm: %s" ++
     (let rec sprintf_tbl  = Printer.pr_econstr_env env sigma )
     )); *)
  (* Feedback.msg_notice
     (str
     (Printf.sprintf
     "translated fsm: %s\n"
     (to_string ~context:ShowIDs (Fsm _fsm)))); *)
  Feedback.msg_notice (str "\n--------\n")
;;

(* Feedback.msg_notice (str "lts_ty: " ++ Printer.pr_econstr_env env sigma lts_ty); *)
