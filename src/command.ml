open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax
open Pp_ext
(* open Translation_layer *)
(* open Fsm *)

type 'a mm = 'a Mebi_monad.t

let arity_is_Prop (mip : Declarations.one_inductive_body) : unit mm =
  let open Declarations in
  match mip.mind_arity with
  | RegularArity s ->
    if not (Sorts.is_prop s.mind_sort)
    then invalid_sort (Sorts.family s.mind_sort)
    else return ()
  | TemplateArity t -> invalid_sort (Sorts.family t.template_level)
;;

let get_lts_labels_and_terms mib mip =
  let open Declarations in
  let typ = Inductive.type_of_inductive (UVars.in_punivs (mib, mip)) in
  let i_ctx = mip.mind_arity_ctxt in
  let _, i_idx = split_at mip.mind_nrealdecls i_ctx [] in
  match i_idx with
  | [ t1; a; t2 ] ->
    let open Context.Rel in
    if Declaration.equal Sorts.relevance_equal Constr.equal t1 t2
    then return (a, t1)
    else invalid_arity typ
  | _ -> invalid_arity typ
;;

(** Coq LTS *)
type lts =
  { coq_lts : EConstr.constr (** The Coq LTS type constructor. *)
  ; trm_type : EConstr.types (** The type of terms for the LTS. *)
  ; lbl_type : EConstr.types (** The type of labels for the LTS. *)
  ; coq_ctor_names : Names.Id.t array
  ; transitions : (Constr.rel_context * Constr.types) array
  (** Coq constructors (i.e. our transitions) *)
  }

let check_ref_lts gref =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env i in
    let* _ = arity_is_Prop mip in
    let* lbl, term = get_lts_labels_and_terms mib mip in
    let univ = mib.mind_univ_hyps in
    (* lts of inductive type *)
    let lts = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    return
      { coq_lts = lts
      ; trm_type = EConstr.of_constr (Context.Rel.Declaration.get_type term)
      ; lbl_type = EConstr.of_constr (Context.Rel.Declaration.get_type lbl)
      ; coq_ctor_names = mip.mind_consnames
      ; transitions = mip.mind_nf_lc
      }
  | _ -> invalid_ref gref
;;

(* FIXME: Weird interaction between exceptions and monadic code. Try/cut *)
(* CANNOT be wrapped around monadic code. Otherwise, the exception is *)
(* *not* caught *)
(** Checks if two terms unify
    TODO: lots of doubts
    - Conversion.CUMUL?
    - Is [w_unify] the best way?
    - ... *)
let m_unify t0 t1 =
  state (fun env sigma ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify _) ->
      sigma, false)
;;

(** Generates [LTS termL ?act ?term2] for unification *)
let mk_template (lts : lts) (termL : EConstr.t) : (EConstr.t * EConstr.t) mm =
  let$ act env sigma = Evarutil.new_evar env sigma lts.lbl_type in
  let$ termR env sigma = Evarutil.new_evar env sigma lts.trm_type in
  let template = EConstr.mkApp (lts.coq_lts, [| termL; act; termR |]) in
  return (termR, template)
;;

(* Can I instantiate the bound variables with metavariables instead? *)
let rec instantiate_ctx lts (c : EConstr.t) = function
  | [] -> return c
  | t :: ts ->
    let* env = get_env in
    let* sigma = get_sigma in
    Feedback.msg_info
      (str "CONSTRUCTOR: "
       ++ int (List.length ts)
       ++ str " "
       ++ Printer.pr_econstr_env env sigma t
       ++ str "  <======= "
       ++ Printer.pr_econstr_env env sigma c);
    let$ vt env sigma = Evarutil.new_evar env sigma t in
    instantiate_ctx
      lts
      (EConstr.Vars.subst1 vt c)
      (List.map (EConstr.Vars.subst1 vt) ts)
;;

(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

(** Checks possible transitions for this term: *)
let check_valid_constructor lts t =
  let$+ t env sigma = Reductionops.nf_all env sigma t in
  let iter_body i ctor_vals =
    let ctx, tm = lts.transitions.(i) in
    let tm = EConstr.of_constr tm in
    let ctx_tys = List.map Context.Rel.Declaration.get_type ctx in
    let* tm = instantiate_ctx lts tm (List.map EConstr.of_constr ctx_tys) in
    let* tgt_term, to_unif = mk_template lts t in
    let* success = m_unify to_unif tm in
    let* sigma = get_sigma in
    (* FIXME: do we need nf_evar here? *)
    let tgt_term = Evarutil.nf_evar sigma tgt_term in
    match success with
    | true -> return ((i, tgt_term, to_unif) :: ctor_vals)
    | false -> return ctor_vals
  in
  iterate 0 (Array.length lts.transitions - 1) [] iter_body
;;

(** [bound] is the total depth that will be explored of a given lts by [explore_lts]. *)
let bound : int = 5

(* FIXME: refactor the below somewhere else, self-contained, with standard *)
(* OCaml naming (e.g. Graph.Make, Graph.S, etc) *)
type lts_transition =
  { edge_ctor : int (** Ctor number *)
  ; to_node : EConstr.constr
  }

module type GraphB = sig
  module H : Hashtbl.S with type key = EConstr.t

  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* Queue for BFS *)
    ; edges : lts_transition H.t
    }

  val build_graph : lts -> lts_graph -> lts_graph mm
  val pp_graph_edges : Environ.env -> Evd.evar_map -> lts_graph -> unit
end

module MkGraph (M : Hashtbl.S with type key = EConstr.t) = struct
  module H = M

  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* Queue for BFS *)
    ; edges : lts_transition H.t
    }

  let rec build_graph (the_lts : lts) (g : lts_graph) : lts_graph mm =
    if H.length g.edges >= bound
    then return g (* FIXME: raise error *)
    else if Queue.is_empty g.to_visit
    then return g
    else
      let* t = return (Queue.pop g.to_visit) in
      let* constrs = check_valid_constructor the_lts t in
      List.iter
        (fun (i, tgt, _) -> H.add g.edges t { edge_ctor = i; to_node = tgt })
        constrs;
      let* sigma = get_sigma in
      List.iter
        (fun (i, tgt, _) ->
          if H.mem g.edges tgt || EConstr.eq_constr sigma tgt t
          then ()
          else Queue.push tgt g.to_visit)
        constrs;
      build_graph the_lts g
  ;;

  let pp_graph_edges env sigma (g : lts_graph) =
    H.iter
      (fun f t ->
        Feedback.msg_notice
          (Printer.pr_econstr_env env sigma f
           ++ Pp.str " ---{ "
           ++ Pp.int t.edge_ctor
           ++ Pp.str " }--> "
           ++ Printer.pr_econstr_env env sigma t.to_node))
      g.edges
  ;;
end

let make_graph_builder =
  let* m = make_constr_tbl in
  let module G = MkGraph ((val m)) in
  return (module G : GraphB)
;;

(* FIXME: Should be user-configurable, not hardcoded *)

(* (\** [coq_fsm] is . *\) *)
(* type coq_fsm = *)
(*   { states : Evd.econstr list *)
(*   ; edges : Evd.econstr list *)
(*   } *)

(* (\* TODO: check which are all possible next transitions *\) *)
(* (\* TODO: check following functions/modules: *\) *)
(* (\* [ ] Unification *\) *)
(* (\* [ ] Reductionops.infer_conv *\) *)
(* (\*  *\) *)

(** Builds an LTS from a Term [t : T] and an LTS [P : forall Ts, T -> A -> T -> Prop]

    Constraints:
    - [ T \& A \not\in Ts ]

    Notes:
    - Constructors of [P] are the transitions
    - States are the sets of possible transitions
    - A term [t] is represented by the state of the transitions that can be taken *)
let bounded_lts
  (iref : Names.GlobRef.t)
  (tref : Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  let* the_lts = check_ref_lts iref in
  let$ t env sigma = Constrintern.interp_constr_evars env sigma tref in
  let$* u env sigma = Typing.check env sigma t the_lts.trm_type in
  let$ t env sigma = sigma, Reductionops.nf_all env sigma t in
  let* graph_Module = make_graph_builder in
  let module Graph = (val graph_Module) in
  let q = Queue.create () in
  let* _ = return (Queue.push t q) in
  let* graph =
    Graph.build_graph the_lts { to_visit = q; edges = Graph.H.create bound }
  in
  let* env = get_env in
  let* sigma = get_sigma in
  Feedback.msg_notice
    (str "(a) Types of terms: "
     ++ Printer.pr_econstr_env env sigma the_lts.trm_type
     ++ strbrk "");
  Feedback.msg_notice
    (str "(b) Types of labels: "
     ++ Printer.pr_econstr_env env sigma the_lts.lbl_type
     ++ strbrk "");
  Feedback.msg_notice
    (str "(c) Constructors: "
     ++ Pp.prvect_with_sep
          (fun _ -> str ", ")
          Names.Id.print
          the_lts.coq_ctor_names);
  (* prints all transitions -- the possible constructors
     a term may take as part of its structure.
     these are dependant on the definition of a type *)
  Feedback.msg_debug
    (str "(d) Transitions: "
     ++ pp_transitions env sigma the_lts.transitions
     ++ strbrk "\n");
  Feedback.msg_debug
    (str "(e) Starting term: " ++ Printer.pr_econstr_env env sigma t);
  Feedback.msg_notice (strbrk "(f) Graph Edges: \n");
  Graph.pp_graph_edges env sigma graph;
  (* Feedback.msg_info *)
  (*   (str "(b) CoqFsm: " ++ pp_coq_fsm env sigma (coq_fsm.states, coq_fsm.edges)); *)
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
  Feedback.msg_notice (str "\n--------\n");
  return ()
;;
