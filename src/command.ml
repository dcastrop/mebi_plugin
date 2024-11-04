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




(** [pp_list l] is a pretty printed list ([l]). *)
let pp_list l = 
  (* ! use [fnl()] for newlines (will only be used if necessary). *)
  str "[" ++ Pp.prlist_with_sep 
  (* sep  *) (fun _ -> str ", ")
  (* fun  *) (fun i -> fnl() ++ str "  " ++ i)
  (* list *) l
  ++ fnl() ++ str "]\n"
;;




(** [pp_transition env sigma transition] is a pretty printed [transition]. *)
let pp_transition env sigma (transition : (Constr.rel_context * Constr.t)) =
  (Printer.pr_constr_env env sigma (snd transition))
  (* ++ str " " ++ str (
  match (snd transition) with
  | _ -> "(unknown)"
  ) *)
;;

(** [pp_transitions_to_list env sigma constrs] is. *)
let pp_transitions_to_list env sigma transitions = 
    let rec transitions_to_list i res =
       if i < 0 then res
       else transitions_to_list (i - 1) (pp_transition env sigma (Array.unsafe_get transitions i) :: res)
    in
    transitions_to_list (Array.length transitions - 1) []
;;

(** [pp_transitions env sigma transitions] is an [array] of [transitions] pretty printed as a [list]. *)
let pp_transitions env sigma transitions = 
  pp_list (pp_transitions_to_list env sigma transitions)
;;

(* 

let pp_state env sigma state = 
  (str "[" ++ (Printer.pr_econstr_env env sigma state) ++ str "]")
;;

(*  *)
let pp_edge_label env sigma label = 
  (str ">-(" ++ (Printer.pr_econstr_env env sigma label) ++ str ")->")
  (* str ">-(" ++ label ++ str ")->" *)
;; *)

(* exception Foo of string;; *)

(** [pp_edge env sigma edge] is a pretty printed [edge]. *)
let pp_edge env sigma edge = Printer.pr_econstr_env env sigma (fst edge)
(* (edge : (Evd.econstr * Evd.econstr)) = *)
  (* let edge_econstr = snd edge in *)
  (* start state -> *)
(* 
  let state_a = 
    edge_econstr in

  (* let state_a = (EConstr.kind sigma edge_econstr) in *)
  (* let state_a = 
    
    
      EConstr.isType sigma 
      edge_econstr


  in *)


  (* ! using below as reference, trying to see how to access 
      any of the contents of [edge_econstr] *)

(* ! (EConstr.isApp edge_econstr) returned true *)
(* ! Constr.App : 'constr * 'constr array *)
(* 
  match edge_econstr with
  | Constr.App (h, t) as _k ->
    h
(* | Constr.Rel _ -> 
  | (Constr.Var _
     | (Constr.Meta _
        | (Constr.Evar _
           | (Constr.Sort _
              | (Constr.Cast (_, _, _)
                 | (Constr.Prod (_, _, _)
                    | (Constr.Lambda (_, _, _)
                       | (Constr.LetIn (_, _, _, _)
                          | (Constr.Const _
                             | (Constr.Ind _
                                | (Constr.Construct _
                                   | (Constr.Case (_, _, _, _, _, _, _)
                                      | (Constr.Fix _
                                         | (Constr.CoFix _
                                            | (Constr.Proj (_, _, _)
                                               | (Constr.Int _
                                                  | (Constr.Float _
                                                     | Constr.Array
                                                     (_, _, _, _)))))))))))))))))) *)
  | _ -> raise (Foo "edge_constr not handled correctly.")


  in *)

  (* let open Names.GlobRef in
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
   *)




  (* edge label *)
  (* let edge_label = (pp_edge_label env sigma "") in *)
  (* let edge_label = edge_econstr.(3) in *)

  (* -> result state *)
  let state_b = (fst edge) in

  (* ++ (str " :: ") *)
  (* ++ (Printer.pr_econstr_env env sigma (snd edge)) *)

  (pp_state env sigma state_a) 
  (* ++ str " " ++ (pp_edge_label env sigma edge_label) *)
  ++ str " " ++ (pp_state env sigma state_b) 

  ++ fnl() ++ str "  " *)

(* 
  ++ str (
    Printf.sprintf "isEvar: %b" (
      EConstr.isEvar sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isInd: %b" (
      EConstr.isInd sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isType: %b" (
      EConstr.isType sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isMeta: %b" (
      EConstr.isMeta sigma 
      edge_econstr
    )
  )
  
  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isVar: %b" (
      EConstr.isVar sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isProd: %b" (
      EConstr.isProd sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isProj: %b" (
      EConstr.isProj sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isSort: %b" (
      EConstr.isSort sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isApp: %b" (
      EConstr.isApp sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isRel: %b" (
      EConstr.isRel sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isConstruct: %b" (
      EConstr.isConstruct sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isArity: %b" (
      EConstr.isArity sigma 
      edge_econstr
    )
  ) *)

(* 
  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isType: %b" (
      EConstr.isType sigma 
      edge_econstr
    )
  )

  ++ fnl() ++ str "  "

  ++ str (
    Printf.sprintf "isType: %b" (
      EConstr.isType sigma 
      edge_econstr
    )
  ) 
    *)

;;

(** [pp_edges_to_list env sigma constrs] is a pretty printed list of edges ([constrs]). *)
let rec pp_edges_to_list env sigma constrs = 
  match constrs with
  | [] -> []
  | h_edge::t_edges ->
    pp_edge env sigma h_edge
    ::(pp_edges_to_list env sigma t_edges)
;;

(** [pp_edges env sigma constrs] is a [t] (str) of pretty printed dges ([constrs]). *)
let pp_edges env sigma constrs =
  pp_list (pp_edges_to_list env sigma constrs)
;;




(** [get_next_edges ...] is a list of edges from the next constrs. 
    
    Unfolds each of the outgoing edges in constrs, and collects all
    of the outgoing edges from there. 

    In terms of fsm: from the current state [a], for each outgoing 
    edge the resulting state [b] is checked for any outgoing edges. 
    The returned list contains all outgoing edges from all possible 
    next states [b] reachable within 1 step.
 *)
let rec get_next_edges env sigma lts_ty constrs terms lbls transitions = 
  let sigma, h_edges, t_edges = 
    match constrs with
    | [] -> sigma, [], []
    | (h_edge,_)::t_edges ->
        let sigma', constrs' = 
          check_valid_constructor env sigma lts_ty h_edge terms lbls transitions
        in
        sigma', constrs', t_edges
  in
  match t_edges with 
  | [] -> sigma, h_edges
  | _::_ -> 
    let sigma, edges = 
    get_next_edges env sigma lts_ty t_edges terms lbls transitions 
    in
    sigma, List.concat [ h_edges; edges ]
;;

(** [pp_next_edges] is [pp_edges] on [get_next_edges edges]. *)
let pp_next_edges env sigma lts_ty constrs terms lbls transitions =
  let sigma, edges = 
  get_next_edges env sigma lts_ty constrs terms lbls transitions
  in
    pp_edges env sigma edges
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

  (* prints all transitions -- the possible constructors
      a term may take as part of its structure. 
      these are dependant on the definition of a type *)
  Feedback.msg_notice
    (str "Transitions: "
        ++ (pp_transitions env sigma transitions)
        ++ strbrk "\n");

  (* print all edges -- describing the possible 
      applications of a type on a given term *)
  Feedback.msg_notice
    (str "Target matches constructors "
      ++ (pp_edges env sigma constrs)
      ++ strbrk "\n");

  (* print all next edges *)
  Feedback.msg_notice
    (str "(next edges) Target matches constructors  " 
      ++ (pp_next_edges env sigma lts_ty constrs terms lbls transitions)
      ++ strbrk "\n");

;;


