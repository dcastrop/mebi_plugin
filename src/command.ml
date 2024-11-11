open Pp
open Mebi_utils
module Err = Mebi_errors
open Pp_ext
open Translation_layer
open Fsm

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
    let sigma, tgt_term, to_unif = mk_template env sigma lts t lbl_ty term_ty in
    match m_unify env sigma to_unif tm with
    | Some sigma -> ctors := sigma, (tgt_term, to_unif) :: ctor_vals
    | None -> ()
  done;
  !ctors
;;

(** [get_next_edges ...] is a list of edges from the next constrs (edges).

    Unfolds each of the outgoing edges in constrs, and collects all
    of the outgoing edges from there.

    In terms of fsm: from the current state [a], for each outgoing
    edge the resulting state [b] is checked for any outgoing edges.
    The returned list contains all outgoing edges from all possible
    next states [b] reachable within 1 step. *)
let rec get_next_edges
  env
  sigma
  lts_ty
  (edges : (Evd.econstr * Evd.econstr) list)
  terms
  lbls
  transitions
  =
  (* update sigma with constructors *)
  let sigma, h_edges, t_edges =
    match edges with
    | [] -> sigma, [], []
    | h_edge :: t_edges ->
      let sigma', constrs' =
        check_valid_constructor
          env
          sigma
          lts_ty
          (fst h_edge)
          terms
          lbls
          transitions
      in
      sigma', constrs', t_edges
  in
  (* continue going through edges *)
  match t_edges with
  | [] -> sigma, h_edges
  | _ :: _ ->
    let sigma, edges =
      get_next_edges env sigma lts_ty t_edges terms lbls transitions
    in
    (* return updated sigma and edges *)
    sigma, List.concat [ h_edges; edges ]
;;

(* (** [pp_next_edges] is [pp_edges] on [get_next_edges edges]. *)
   let pp_next_edges
   env
   sigma
   lts_ty
   constrs
   terms
   lbls
   transitions
   (* : Environ.env * Evd.evar_map * ((Evd.econstr * Evd.econstr) list) *)
   =
   let sigma, edges =
   get_next_edges env sigma lts_ty constrs terms lbls transitions
   in
   pp_edges env sigma (Mebi_utils.strip_snd edges)
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
    (str "Transitions: " ++ pp_transitions env sigma transitions ++ strbrk "\n");
  (* print all edges -- describing the possible
     applications of a type on a given term *)
  Feedback.msg_notice
    (str "Target matches constructors "
     ++ pp_edges env sigma (Mebi_utils.strip_snd constrs)
     ++ strbrk "\n");
  (* print all next edges *)
  (* Feedback.msg_notice
     (str "(next edges) Target matches constructors  "
     ++ pp_next_edges env sigma lts_ty constrs terms lbls transitions
     ++ strbrk "\n") *)
  Feedback.msg_notice (str "done.")
;;

(* [mem m l] is [true] if [m] is in [l]. *)
let rec mem env sigma m l : bool =
  match l with
  | [] -> false
  | h :: t ->
    (match EConstr.eq_constr sigma h m with
     | true -> true
     | _ -> mem env sigma m t)
;;

let rec mem' env sigma m (l : (Evd.econstr * Evd.econstr) list) : bool =
  match l with
  | [] -> false
  | h :: t ->
    (match EConstr.eq_constr sigma (snd h) (snd m) with
     | true -> true
     | _ -> mem' env sigma m t)
;;

(** [cap_edges es to_check] is the list in of elements in [to_check] that do not appear in [es].*)
(* let rec cap_edges
   env
   sigma
   (es : Evd.econstr list)
   (to_check : Evd.econstr list)
   : Evd.econstr list
   =
   match to_check with
   (* return [] *)
   | [] -> []
   (*  *)
   | h :: t ->
   (match mem env sigma h es with
   | true ->
   (* Feedback.msg_info (str " --: (" ++ (Printer.pr_econstr_env env sigma (snd h)) ++ str ")  already found, skipping.\n" ); *)
   cap_edges env sigma es t
   | _ ->
   (* Feedback.msg_info (str " +-: (" ++ (Printer.pr_econstr_env env sigma (snd h)) ++ str ")  not found, adding.\n" ); *)
   List.concat [ cap_edges env sigma es t; [ h ] ])
   ;; *)

let rec cap_edges'
  env
  sigma
  (es : (Evd.econstr * Evd.econstr) list)
  (to_check : (Evd.econstr * Evd.econstr) list)
  : (Evd.econstr * Evd.econstr) list
  =
  match to_check with
  (* return [] *)
  | [] -> []
  (*  *)
  | h :: t ->
    (match mem' env sigma h es with
     | true ->
       (* Feedback.msg_info (str " --: (" ++ (Printer.pr_econstr_env env sigma (snd h)) ++ str ")  already found, skipping.\n" ); *)
       cap_edges' env sigma es t
     | _ ->
       (* Feedback.msg_info (str " +-: (" ++ (Printer.pr_econstr_env env sigma (snd h)) ++ str ")  not found, adding.\n" ); *)
       List.concat [ cap_edges' env sigma es t; [ h ] ])
;;

(** [merge env sigma l1 l2] is the combination of [l1] and [l2] with any duplicates removed. *)
(* let rec merge env sigma l1 l2 =
   match l1 with
   | [] -> l2
   | h :: t ->
   (match mem env sigma h l2 with
   | true -> merge env sigma t l2
   (* (EConstr_list t) (EConstr_list l2) *)
   | _ -> h :: merge env sigma t l2)
   ;; *)

let rec merge' env sigma l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t ->
    (match mem' env sigma h l2 with
     | true -> merge' env sigma t l2
     (* (EConstr_list t) (EConstr_list l2) *)
     | _ -> h :: merge' env sigma t l2)
;;

(** [unique env sigma l] is list [l] with any duplicates removed. *)
let rec unique env sigma l =
  match l with
  | [] -> []
  | h :: t ->
    (match mem env sigma h t with
     (* if dupe, skip this one add the next *)
     | true -> unique env sigma t
     (* else keep *)
     | _ -> h :: unique env sigma t)
;;

(** [coq_fsm] is . *)
type coq_fsm =
  { states : Evd.econstr list
  ; edges : Evd.econstr list
  }

(** [explore_lts] is the list of [constrs] (edges) reachable, within [max] bounds.*)
let rec explore_lts
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (lts_ty : Evd.econstr)
  (constrs : (Evd.econstr * Evd.econstr) list)
  (* (constrs : Evd.econstr list) *)
    (terms : Evd.econstr)
  (lbls : Evd.econstr)
  transitions
  ( (* (lts : Evd.econstr list) *)
    (lts : (Evd.econstr * Evd.econstr) list)
  , states
  , bound
  , max )
  : Evd.evar_map * coq_fsm
  =
  assert (bound >= 0);
  match constrs, lts with
  (* error if both are empty *)
  | [], [] ->
    Feedback.msg_notice
      (str "ExploreLTS, both constrs and lts empty, returning empty lts.");
    sigma, { states; edges = [] }
  (* first entering *)
  | _, [] ->
    Feedback.msg_notice
      (str
         (Printf.sprintf
            "ExploreLTS, bound ( %d / %d ), lts was empty, using constrs."
            (max - bound)
            max));
    explore_lts
      env
      sigma
      lts_ty
      constrs
      terms
      lbls
      transitions
      (constrs, states, bound, max)
  (* no more edges *)
  | [], _ ->
    Feedback.msg_notice (str "ExploreLTS, no more edges (constrs) to explore.");
    sigma, { states; edges = unique env sigma (Mebi_utils.strip_snd lts) }
  (* continue exploring *)
  | _, _ ->
    (match bound with
     (* stop -- bound exceeded *)
     | 0 ->
       Feedback.msg_info
         (str
            (Printf.sprintf "ExploreLTS, stopping -- reached bound (%d).\n" max));
       sigma, { states; edges = unique env sigma (Mebi_utils.strip_snd lts) }
     (* continue -- within bounds *)
     | _ ->
       (* get constructors *)
       let sigma', (edges : (Evd.econstr * Evd.econstr) list) =
         get_next_edges env sigma lts_ty constrs terms lbls transitions
       in
       (* get states (terms) from each of the edges. *)
       let rec extract_states
         env
         sigma
         (edges : (Evd.econstr * Evd.econstr) list)
         (acc : Evd.econstr list)
         : Evd.econstr list
         =
         match edges with
         | [] -> acc
         | h :: t ->
           extract_states
             env
             sigma
             t
             ((* extract the states from the edge *)
              let lhs_id, rhs_id =
                match EConstr.decompose_app sigma (snd h) with
                | h' ->
                  (match h' with
                   | _lhs, rhs ->
                     let rhs' = Array.to_list rhs in
                     List.nth rhs' 0, List.nth rhs' 2)
              in
              (* only add the new states. *)
              List.concat
                [ (if mem env sigma' lhs_id acc then [] else [ lhs_id ])
                ; (if mem env sigma' rhs_id acc then [] else [ rhs_id ])
                ; acc
                ])
       in
       (*** [states'] is the list of [states] encountered so far. *)
       let states' = extract_states env sigma edges states in
       Feedback.msg_info (str "fsm: " ++ pp_edges' env sigma' lts);
       Feedback.msg_info (str "edges: " ++ pp_edges' env sigma' edges);
       (*** [constrs'] is the list of [edges] not contained within [lts]. *)
       let constrs' =
         cap_edges' env sigma' lts edges
         (* (Mebi_utils.strip_snd lts)
            (Mebi_utils.strip_snd edges) *)
       in
       (*** [lts'] is the combination of [lts] and [edges] with duplicates removed. *)
       let lts' =
         merge' env sigma' lts edges
         (* (Mebi_utils.strip_snd lts)
            (Mebi_utils.strip_snd edges) *)
       in
       (match List.is_empty constrs' with
        (* finished within bounds *)
        | true ->
          Feedback.msg_notice
            (str
               (Printf.sprintf
                  "ExploreLTS, finished on bound ( %d / %d ).\n"
                  (max - bound)
                  max));
          ( sigma'
          , { states = states'
            ; edges = unique env sigma' (Mebi_utils.strip_snd lts')
            } )
        (* keep going *)
        | _ ->
          explore_lts
            env
            sigma'
            lts_ty
            constrs'
            terms
            lbls
            transitions
            (lts', states', bound - 1, max)))
;;

let bound : int = 3

let bounded_lts
  (iref : Names.GlobRef.t)
  (tref : Constrexpr.constr_expr_r CAst.t)
  : unit
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
    (str "(b) Types of terms: "
     ++ Printer.pr_econstr_env env sigma terms
     ++ strbrk "");
  Feedback.msg_notice
    (str "(b) Types of labels: "
     ++ Printer.pr_econstr_env env sigma lbls
     ++ strbrk "");
  Feedback.msg_notice
    (str "(b) Constructors: "
     ++ Pp.prvect_with_sep (fun _ -> str ", ") Names.Id.print c_names);
  (* prints all transitions -- the possible constructors
     a term may take as part of its structure.
     these are dependant on the definition of a type *)
  Feedback.msg_notice
    (str "Transitions: " ++ pp_transitions env sigma transitions ++ strbrk "\n");
  let sigma, coq_fsm =
    explore_lts
      env
      sigma
      lts_ty
      constrs
      (* (Mebi_utils.strip_snd constrs) *)
      terms
      lbls
      transitions
      ([], [ t ], bound, bound)
  in
  (* match coq_fsm with
  | { states; edges; _ } ->
    Feedback.msg_notice (str "(b) Edges: " ++ pp_edges env sigma edges); *)
  Feedback.msg_notice
    (str "(b) CoqFsm: " ++ pp_coq_fsm env sigma (coq_fsm.states, coq_fsm.edges));
  (* print out other information too *)
  Feedback.msg_notice (str "terms: " ++ Printer.pr_econstr_env env sigma terms);
  Feedback.msg_notice (str "lbls: " ++ Printer.pr_econstr_env env sigma lbls);
  Feedback.msg_notice (str "lts_ty: " ++ Printer.pr_econstr_env env sigma lts_ty);
  Feedback.msg_notice (str "t: " ++ Printer.pr_econstr_env env sigma t);
  (* tests on edges *)
  match coq_fsm.edges with
  | [] -> ()
  | h :: _t ->
    Feedback.msg_notice
      (str "h edge: "
       ++ pp_edge env sigma h
       ++ str "\n\ntests: \n"
       ++ str (Printf.sprintf "isApp: %b" (EConstr.isApp sigma h))
       ++ str "\nend of tests.\n");
    (* lts to fsm *)
    let _tbl, _fsm =
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
    in
    (* ( Hashtbl.iter (fun x y -> Printf.sprintf "tbl: %s -> %s\n" x y) _tbl.state_map;;); *)
    (* Feedback.msg_notice
       (str (Printf.sprintf "translated fsm: %s" ++
       (let rec sprintf_tbl  = Printer.pr_econstr_env env sigma )
       )); *)
    Feedback.msg_notice
      (str
         (Printf.sprintf
            "translated fsm: %s\n\n--------\n"
            (to_string ~context:ShowIDs (Fsm _fsm))))
;;

(* Feedback.msg_notice (str "lts_ty: " ++ Printer.pr_econstr_env env sigma lts_ty); *)
