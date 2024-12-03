open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax
open Pp_ext

(** ['a mm] is a function type mapping from [coq_context ref] to ['a in_context]. *)
type 'a mm = 'a Mebi_monad.t

(** [arity_is_prop mip] raises an error if [mip.mind_arity] is not a [prop]. *)
let arity_is_prop (mip : Declarations.one_inductive_body) : unit mm =
  let open Declarations in
  match mip.mind_arity with
  | RegularArity s ->
    if not (Sorts.is_prop s.mind_sort)
    then invalid_sort (Sorts.family s.mind_sort)
    else return ()
  | TemplateArity t -> invalid_sort (Sorts.family t.template_level)
;;

(** [get_lts_labels_and_terms mib mip] is the mapping of terms (states) and labels (outgoing edges) from [mip].
    Raises error ([invalid_arity]) if lts terms and labels cannot be obtained from [mip]. [mib] is only used in case of error. *)
let get_lts_labels_and_terms
  (mib : Declarations.mutual_inductive_body)
  (mip : Declarations.one_inductive_body)
  : (Constr.rel_declaration * Constr.rel_declaration) mm
  =
  let open Declarations in
  (* get the type of [mip] from [mib]. *)
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

(** [lts] is a type used to describe the Coq LTS of Coq-based terms.
    [coq_lts] is the type constructor.
    [trm_type] is the type of terms for the LTS.
    [lbl_type] is the type of labels for the LTS.
    [coq_ctor_names] is the array of names for each constructor of the Coq term.
    [transitions] is the array of constructors of the Coq term (i.e., the transitions or outgoing edges). *)
type lts =
  { coq_lts : EConstr.constr
  ; trm_type : EConstr.types
  ; lbl_type : EConstr.types
  ; coq_ctor_names : Names.Id.t array
  ; transitions : (Constr.rel_context * Constr.types) array
  }

(** [check_ref_lts gref] is the [lts] of [gref].
    Raises error ([invalid_ref]) if [gref] is not a reference to an inductive type. *)
let check_ref_lts (gref : Names.GlobRef.t) : lts mm =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env i in
    let* _ = arity_is_prop mip in
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
  (* raise error if [gref] is not an inductive type *)
  | _ -> invalid_ref gref
;;

(* FIXME: All of the code below, up to [check_valid_constructor] needs
   reworking *)
(* FIXME: Weird interaction between exceptions and monadic code. Try/cut *)
(* CANNOT be wrapped around monadic code. Otherwise, the exception is *)
(* *not* caught *)
(** Checks if two terms unify
    TODO: lots of doubts
    - Conversion.CUMUL?
    - Is [w_unify] the best way?
    - ... *)
let m_unify (t0 : Evd.econstr) (t1 : Evd.econstr) : bool mm =
  let* _ =
    debug (fun (env : Environ.env) (sigma : Evd.evar_map) ->
      str "Unifying "
      ++ Printer.pr_econstr_env env sigma t0
      ++ strbrk "\n"
      ++ str "Unifying "
      ++ Printer.pr_econstr_env env sigma t1)
  in
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      Feedback.msg_debug (str "\t\tSuccess");
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify (m, n, e))
      ->
      Feedback.msg_debug (str "\t\tCould not unify");
      sigma, false)
;;

(** [mk_ctx_substl] *)
let rec mk_ctx_substl (substl : EConstr.t list)
  : ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list -> Evd.econstr list mm
  = function
  | [] -> return substl
  | t :: ts ->
    (* get type of [t] *)
    let ty = EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t) in
    (*  *)
    let$ vt env sigma = Evarutil.new_evar env sigma ty in
    mk_ctx_substl (vt :: substl) ts
;;

(** Extract args of a type [LTS termL act termR]
    Prerequisite: input *must* be of this shape *)
let extract_args (substl : EConstr.Vars.substl) (tm : Constr.t) =
  match Constr.kind tm with
  | App (_, args) ->
    assert (Array.length args == 3);
    let args = EConstr.of_constr_array args in
    let args = Array.map (EConstr.Vars.substl substl) args in
    args.(0), args.(1), args.(2)
  | _ -> assert false
;;

type unif_problem =
  { termL : EConstr.t
  ; termR : EConstr.t
  }

let rec unify_all (i : (int * unif_problem) list) : bool t =
  match i with
  | [] -> return true
  | (_, u) :: t ->
    let* _ =
      debug (fun env sigma ->
        str "UNIFYALL:::::::::: "
        ++ Printer.pr_econstr_env env sigma u.termL
        ++ strbrk "\n::::::::::"
        ++ Printer.pr_econstr_env env sigma u.termR)
    in
    let* success = m_unify u.termL u.termR in
    if success then unify_all t else return false
;;

let sandboxed_unify tgt_term u =
  let* _ =
    debug (fun env sigma ->
      str "TGT:::::: " ++ Printer.pr_econstr_env env sigma tgt_term)
  in
  sandbox
    (let* success = unify_all u in
     match success with
     | true ->
       let$+ term env sigma = Reductionops.nf_all env sigma tgt_term in
       let$+ undefined _ sigma = EConstr.isEvar sigma term in
       if undefined then return None else return (Some term)
     | false -> return None)
;;

let rec retrieve_tgt_nodes acc i tgt_term
  : (int * unif_problem) list list -> (int * EConstr.t) list t
  = function
  | [] -> return acc
  | u1 :: nctors ->
    let* success = sandbox (sandboxed_unify tgt_term u1) in
    (match success with
     | Some tgt -> retrieve_tgt_nodes ((i, tgt) :: acc) i tgt_term nctors
     | None -> retrieve_tgt_nodes acc i tgt_term nctors)
;;

(* Should return a list of unification problems *)
let rec check_updated_ctx acc lts
  :  EConstr.t list * EConstr.rel_declaration list
  -> (int * unif_problem) list list option t
  = function
  | [], [] -> return (Some acc)
  | _ :: substl, t :: tl ->
    let$+ upd_t env sigma =
      EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t)
    in
    let* sigma = get_sigma in
    (match EConstr.kind sigma upd_t with
     | App (fn, args) ->
       if EConstr.eq_constr sigma fn lts.coq_lts
       then
         let$+ nextT env sigma = Reductionops.nf_evar sigma args.(0) in
         let* ctors = check_valid_constructor lts nextT in
         if List.is_empty ctors
         then return None
         else (
           let ctors =
             List.map (fun (i, tL) -> i, { termL = tL; termR = args.(2) }) ctors
           in
           (* We need to cross-product all possible unifications. This is in case
              we have a constructor of the form LTS t11 a1 t12 -> LTS t21 a2
              t22 -> ... -> LTS tn an t2n. Repetition may occur. It is not
              unavoidable, but we should make sure we understand well the
              problem before removing the source of repetition. *)
           check_updated_ctx
             (List.concat_map (fun x -> List.map (fun y -> y :: x) ctors) acc)
             lts
             (substl, tl))
       else check_updated_ctx acc lts (substl, tl)
     | _ -> check_updated_ctx acc lts (substl, tl))
  | _, _ -> assert false
(* Impossible! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

(** Checks possible transitions for this term: *)
and check_valid_constructor lts t : (int * EConstr.t) list t =
  let$+ t env sigma = Reductionops.nf_all env sigma t in
  let iter_body i ctor_vals =
    let* _ =
      debug (fun env sigma ->
        str "CHECKING CONSTRUCTOR "
        ++ int i
        ++ str ". Term: "
        ++ Printer.pr_econstr_env env sigma t)
    in
    let ctx, tm = lts.transitions.(i) in
    let ctx_tys = List.map EConstr.of_rel_decl ctx in
    let* substl = mk_ctx_substl [] (List.rev ctx_tys) in
    let termL, act, termR = extract_args substl tm in
    let* success = m_unify t termL in
    match success with
    | true ->
      let* next_ctors = check_updated_ctx [ [] ] lts (substl, ctx_tys) in
      let tgt_term = EConstr.Vars.substl substl termR in
      (match next_ctors with
       | None -> return ctor_vals
       | Some [] ->
         let* sg = get_sigma in
         if EConstr.isEvar sg tgt_term
         then return ctor_vals
         else return ((i, tgt_term) :: ctor_vals)
       | Some nctors -> retrieve_tgt_nodes ctor_vals i tgt_term nctors)
    | false -> return ctor_vals
  in
  iterate 0 (Array.length lts.transitions - 1) [] iter_body
;;

(** [bound] is the total depth that will be explored of a given lts by [explore_lts]. *)
let bound : int = 5

(* type 'a tree = *)
(*   | Node of 'a * 'a tree list *)

(* FIXME: refactor the below somewhere else, self-contained, with standard *)
(* OCaml naming (e.g. Graph.Make, Graph.S, etc) *)
type lts_transition =
  { edge_ctor : int (* FIXME: [edge_ctor] should be a int tree *)
  (** Ctor number *)
  ; to_node : EConstr.constr
  }

(** [GraphB] is ...
    (Essentially acts as a `.mli` for the [MkGraph] module.) *)
module type GraphB = sig
  module H : Hashtbl.S with type key = EConstr.t

  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* Queue for BFS *)
    ; transitions : lts_transition H.t
    }

  val build_graph : lts -> Constrexpr.constr_expr_r CAst.t -> lts_graph mm
  val pp_graph_transitions : Environ.env -> Evd.evar_map -> lts_graph -> unit

  type state_translation = (EConstr.t, Fsm.state) Hashtbl.t
  type ed = (Fsm.label, Fsm.state) Fsm.transition
  type es = (Fsm.state, ed) Hashtbl.t

  val build_translation : lts_graph -> state_translation mm
  val build_edges : lts_graph -> state_translation -> es mm

  val lts_to_fsm
    :  lts_graph
    -> (* EConstr.constr *)
       Constrexpr.constr_expr_r CAst.t
    -> EConstr.types
    -> (Fsm.fsm_aux * state_translation) mm

  val pp_fsm : ?long:unit -> Environ.env -> Evd.evar_map -> Fsm.fsm_aux -> unit
  val pstr_state : ?long:unit -> Fsm.state -> string
  val pstr_edge : ?long:unit -> Fsm.state * ed -> string

  (* val pp_fsm_states
     :  Environ.env
     -> Evd.evar_map
     -> Fsm.state list
     -> (Evd.econstr, int) Hashtbl.t
     -> unit

     val pp_fsm_edges : Environ.env -> Evd.evar_map -> Fsm.edge list -> unit *)
end

(** [MkGraph M] is ...
    [M] is a ... *)
module MkGraph (M : Hashtbl.S with type key = EConstr.t) : GraphB = struct
  module H = M

  (** [lts_graph] is a type used when building an lts graph from Coq-based terms.
      [to_visit] is a queue of coq terms to explore.
      [transitions] is a hashtable mapping integers (of hashed constructors) to Coq terms. *)
  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* Queue for BFS *)
    ; transitions : lts_transition H.t
    }

  (** [build_lts the_lts g] is an [lts_graph] [g] obtained by exploring [the_lts].
      [the_lts] describes the Coq-based term.
      [g] is an [lts_graph] accumulated while exploring [the_lts]. *)
  let rec build_lts (the_lts : lts) (g : lts_graph) : lts_graph mm =
    if H.length g.transitions >= bound
    then return g (* FIXME: raise error *)
    else if Queue.is_empty g.to_visit
    then return g
    else
      let* t = return (Queue.pop g.to_visit) in
      let* constrs = check_valid_constructor the_lts t in
      let* env = get_env in
      let* sigma = get_sigma in
      List.iter
        (fun (i, tgt) ->
          Feedback.msg_debug
            (str "\n\nTransition to" ++ Printer.pr_econstr_env env sigma tgt);
          H.add g.transitions t { edge_ctor = i; to_node = tgt };
          if H.mem g.transitions tgt || EConstr.eq_constr sigma tgt t
          then ()
          else Queue.push tgt g.to_visit;
          Feedback.msg_debug
            (str "\nVisiting next: " ++ int (Queue.length g.to_visit)))
        constrs;
      build_lts the_lts g
  ;;

  (** [build_graph the_lts t] is ...
      [the_lts] is ...
      [t] is ... *)
  let build_graph (the_lts : lts) (t : Constrexpr.constr_expr_r CAst.t)
    : lts_graph mm
    =
    let$ t env sigma = Constrintern.interp_constr_evars env sigma t in
    let$* u env sigma = Typing.check env sigma t the_lts.trm_type in
    let$ t env sigma = sigma, Reductionops.nf_all env sigma t in
    let q = Queue.create () in
    let* _ = return (Queue.push t q) in
    build_lts the_lts { to_visit = q; transitions = H.create bound }
  ;;

  (** [pp_graph_transitions env sigma g] is ... *)
  let pp_graph_transitions
    (env : Environ.env)
    (sigma : Evd.evar_map)
    (g : lts_graph)
    : unit
    =
    H.iter
      (fun from transition ->
        Feedback.msg_debug
          (Printer.pr_econstr_env env sigma from
           ++ Pp.str " ---{ "
           ++ Pp.int transition.edge_ctor
           ++ Pp.str " }--> "
           ++ Printer.pr_econstr_env env sigma transition.to_node))
      g.transitions
  ;;

  (** [econstr_to_string env sigma target] is a [string] representing [target]. *)
  let econstr_to_string
    (env : Environ.env)
    (sigma : Evd.evar_map)
    (target : Evd.econstr)
    : string
    =
    Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma target)
  ;;

  open Fsm

  (** [translation_map] is ... *)
  (* type translation_map = (Evd.econstr * int) list *)

  (** [translation_table] is ... *)
  (* type translation_table = (Evd.econstr, id) Hashtbl.t *)

  (** [] is ...
      [constr_hash] is an [id] obtained by hashing the Coq-based [Evd.econstr] extracted from a transition, where a [state] corresponds to a [term] and an [edge] corresponds to a [ctor].
      [id] is an [int] used to identify a [fsm] component (either acting as a [state.id] or [edge.id]). *)
  (* type lts_to_fsm_translation_table =
    { constr_hash : int
    ; id : int
    } *)

  (** [state_translation] is is a hashtable mapping [EConstr.t] of terms to [Fsm.states]. *)
  type state_translation = (EConstr.t, Fsm.state) Hashtbl.t

  (** [build_translation g] is the [state_translation] of [lts_graph] [g] of Coq-based terms to pure OCaml [Fsm]. *)
  let build_translation (g : lts_graph) : state_translation mm =
    let* env = get_env in
    let* sigma = get_sigma in
    let hash t =
      EConstr.to_constr ?abort_on_undefined_evars:(Some false) sigma t
      |> Constr.hash
    in
    let keys = H.to_seq_keys g.transitions in
    let tr_tbl = Seq.length keys |> Hashtbl.create in
    let _ =
      Seq.iter
        (fun t ->
          (* check if [t] is already captured *)
          if false == Hashtbl.mem tr_tbl t
          then
            Hashtbl.add
              tr_tbl
              t
              { id = hash t; pp = econstr_to_string env sigma t };
          (* check if [dest_state] is already captured *)
          let dest_state = (H.find g.transitions t).to_node in
          if false == Hashtbl.mem tr_tbl dest_state
          then
            Hashtbl.add
              tr_tbl
              dest_state
              { id = hash dest_state
              ; pp = econstr_to_string env sigma dest_state
              })
        keys
    in
    return tr_tbl
  ;;

  type ed = (Fsm.label, Fsm.state) Fsm.transition

  type es = (Fsm.state, ed) Hashtbl.t (**  *)

  (**  *)
  let build_edges (g : lts_graph) (s : state_translation) : es mm =
    let* env = get_env in
    let* sigma = get_sigma in
    let keys = H.to_seq_keys g.transitions in
    let es_tbl = Seq.length keys |> Hashtbl.create in
    (* H.iter
      (fun from transition ->
        Feedback.msg_debug
          (Printer.pr_econstr_env env sigma from
           ++ Pp.str " ---{ "
           ++ Pp.int transition.edge_ctor
           ++ Pp.str " }--> "
           ++ Printer.pr_econstr_env env sigma transition.to_node))
      g.transitions *)
    H.iter
      (fun from transition ->
        let edge_from = Hashtbl.find s from in
        let edge_label = transition.edge_ctor in
        let edge_dest = Hashtbl.find s transition.to_node in
        Hashtbl.add
          es_tbl
          edge_from
          { label = edge_label; to_state = edge_dest })
      g.transitions;
    (* in *)
    return es_tbl
  ;;

  (* Feedback.msg_debug
          (Printer.pr_econstr_env env sigma from
           ++ Pp.str " ---{ "
           ++ Pp.int transition.edge_ctor
           ++ Pp.str " }--> "
           ++ Printer.pr_econstr_env env sigma transition.to_node))
      g.transitions;

    let _ =
      Seq.iter
        (fun t ->
          Feedback.msg_warning
            (str (Printf.sprintf "edge_from: %b" (Hashtbl.mem s t)));
          Feedback.msg_warning
            (str
               (Printf.sprintf "edge_label: %b (mem)" (H.mem g.transitions t)));
          Feedback.msg_warning
            (str
               (Printf.sprintf
                  "edge_label: %d"
                  (H.find g.transitions t).edge_ctor));
          Feedback.msg_warning
            (str
               (Printf.sprintf
                  "edge_dest: %b (mem g) :: %s"
                  (H.mem g.transitions t)
                  (econstr_to_string env sigma (H.find g.transitions t).to_node)));
          Feedback.msg_warning
            (str
               (Printf.sprintf
                  "edge_dest: %b"
                  (Hashtbl.mem s (H.find g.transitions t).to_node)));
          let edge_from = Hashtbl.find s t in
          let edge_label = (H.find g.transitions t).edge_ctor in
          let edge_dest = Hashtbl.find s (H.find g.transitions t).to_node in
          Hashtbl.add
            es_tbl
            edge_from
            { label = edge_label; to_state = edge_dest })
        (* keys *)
    in
    return es_tbl
  ;; *)

  (** Error when trying to translate unfinished LTS to FSM. *)
  exception UnfinishedLTS of lts_graph

  exception NoStates of state_translation
  (* exception NoEdges of es *)

  let pstr_state ?(long : unit option) (state : Fsm.state) : string =
    match long with
    | None -> Printf.sprintf "(%d)" state.id
    | Some () -> Printf.sprintf "(%s)" state.pp
  ;;

  let pstr_edge ?(long : unit option) ((from_state : Fsm.state), (edge : ed))
    : string
    =
    let from_state_str, dest_state_str =
      match long with
      | None -> pstr_state from_state, pstr_state edge.to_state
      | Some () ->
        pstr_state ~long:() from_state, pstr_state ~long:() edge.to_state
    in
    let edge_label_str = Printf.sprintf "--{ %d }->" edge.label in
    Printf.sprintf "{%s %s %s}" from_state_str edge_label_str dest_state_str
  ;;

  let pp_fsm
    ?(long : unit option)
    (env : Environ.env)
    (sigma : Evd.evar_map)
    (the_fsm_aux : Fsm.fsm_aux)
    : unit
    =
    Feedback.msg_debug
      (str
         (Printf.sprintf
            "init state: %s"
            (* (pstr_state env sigma the_fsm_aux.init) *)
            (match long with
             | None -> pstr_state the_fsm_aux.init
             | Some () -> pstr_state ~long:() the_fsm_aux.init)));
    Hashtbl.iter
      (fun (from_state : Fsm.state)
        (outgoing_transition : ed)
          (* (t : (Fsm.label, Fsm.state) Fsm.transition) *) ->
        (* let edge_label = t.label in
           let dest_state = t.to_state in *)
        Feedback.msg_debug
          (str
             (match long with
              | None -> pstr_edge (from_state, outgoing_transition)
              | Some () -> pstr_edge ~long:() (from_state, outgoing_transition))
             (* (Printf.sprintf
                "(from: %s)\n\t(label: %d)\n\t(dest: %s)."
                (pstr_state from_state)
                edge_label
                (pstr_state dest_state) (* (e.label) (pstr_state e.to_state) *))
             *)))
      the_fsm_aux.edges;
    ()
  ;;

  (*  *)
  let lts_to_fsm
    (g : lts_graph)
    (* (initial_term : EConstr.constr) *)
      (init_t : Constrexpr.constr_expr_r CAst.t)
    (term_type : EConstr.types)
    : (Fsm.fsm_aux * state_translation) mm
    =
    match g with
    | { to_visit; transitions; _ } ->
      if Queue.is_empty to_visit
      then (
        Feedback.msg_debug (str "queue not empty");
        let* states = build_translation g in
        if Hashtbl.to_seq_keys states |> Seq.is_empty
        then raise (NoStates states)
        else
          let* env = get_env in
          let* sigma = get_sigma in
          let states_keys = Hashtbl.to_seq_keys states in
          Feedback.msg_debug
            (str
               (Printf.sprintf
                  "%s"
                  (Seq.fold_left
                     (fun acc x ->
                       Printf.sprintf
                         "%s%s;\n"
                         acc
                         (econstr_to_string env sigma x))
                     ""
                     states_keys)));
          Feedback.msg_debug
            (str (Printf.sprintf "after states (%d)." (Seq.length states_keys)));
          let* edges = build_edges g states in
          (* if Hashtbl.to_seq_keys edges |> Seq.is_empty
             then raise (NoEdges edges)
             else *)
          let edges_keys = Hashtbl.to_seq_keys edges in
          Feedback.msg_debug
            (str
               (Printf.sprintf
                  "%s"
                  (Seq.fold_left
                     (fun acc x -> Printf.sprintf "%s%s;\n" acc (pstr_state x))
                     ""
                     edges_keys)));
          Feedback.msg_debug (str "after edges.");
          let$ t env sigma =
            Constrintern.interp_constr_evars env sigma init_t
          in
          let$* u env sigma = Typing.check env sigma t term_type in
          let$ init_t env sigma = sigma, Reductionops.nf_all env sigma t in
          Feedback.msg_warning
            (str
               (Printf.sprintf
                  "mem init: %b (%s)"
                  (Hashtbl.mem states init_t)
                  (econstr_to_string env sigma init_t)));
          let init =
            Hashtbl.find states init_t
            (* (H.find lts_graph.transitions initial_term ) *)
            (* { id = -1; pp = "init" } *)
            (* Hashtbl.find states ( List.nth (Hashtbl.to_seq_keys states |> List.of_seq) 0) *)
          in
          Feedback.msg_warning
            (str
               (Printf.sprintf
                  "init state: %s"
                  (Fsm.Stringify.to_string (State init))));
          return ({ init; edges }, states))
      else (
        Feedback.msg_warning
          (str
             (Printf.sprintf
                "lts is not complete, still had at least (%d) terms to visit."
                (Queue.length to_visit)));
        raise (UnfinishedLTS g))
  ;;

  (*
     (*
     (** [fsm_translation] is... *)
  type fsm_translation =
    { to_visit : EConstr.constr Queue.t (* Queue for BFS *)
    ; transitions : lts_transition H.t
    } *)

  (* let get_state_translation_table
    ?(tr : translation = { state_table = H.create 1; edge_table = H.create 1 })
    (transitions : lts_transition H.t)
    : translation mm
    =
    let* env = get_env in
    let* sigma = get_sigma in
    H.iter
      (fun f t ->
        H.add
          tr.state_table
          t
          { constr_hash = Constr.hash (EConstr.to_constr sigma f)
          ; id = 1
          })
      transitions;
    return tr
  ;; *)

  (* let rec get_state_translation_table' (ts:lts_transition M.t) : lts_to_fsm_translation_table =

     match ts with
     | [] ->

     in
     get_state_translation_table' transitions *)
  (* ;; *)

  (** [get_states transitions] is ... *)
  let get_states (transitions : lts_transition M.t)
    : (state list * (Evd.econstr * int) list) mm
    (* : (state list * lts_to_fsm_translation_table) mm *)
    =
    let* env = get_env in
    let* sigma = get_sigma in
    let rec get_states'
      (ts : (Evd.econstr * lts_transition) list)
      (acc : state list)
      (map : (Evd.econstr * int) list)
      (* (map: lts_to_fsm_translation_table) *)
        (state_id : int)
      : state list * (Evd.econstr * int) list
      =
      (* : state list * lts_to_fsm_translation_table *)
      match ts with
      | [] -> acc, map
      | (h_from, _h_transition) :: t ->
        (* let hash = (Constr.hash (EConstr.to_constr sigma h_econstr)) in *)
        (* H.add map t {constr_hash = (Constr.hash (EConstr.to_constr sigma h_econstr)) ; id = state_id}; *)
        get_states'
          t
          (List.concat
             [ [ state
                   ~name:
                     (Printf.sprintf
                        "s:%s"
                        (econstr_to_string env sigma h_from))
                   state_id
               ]
             ; acc
             ])
          map
          (* (List.concat [ [ h_econstr, state_id ]; map ]) *)
          (state_id + 1)
    in
    return (get_states' (List.of_seq (H.to_seq transitions)) [] [] 0)
  ;;

  (** [get_edges transitions state_table] is ... *)
  let get_edges
    (transitions : lts_transition M.t)
    (state_table : (Evd.econstr, int) Hashtbl.t)
    : (edge list * translation_map) mm
    =
    Feedback.msg_info (str "h (before *)");
    let* env = get_env in
    let* sigma = get_sigma in
    Feedback.msg_info (str "i (after *)");
    let rec get_edges'
      (ts : (Evd.econstr * lts_transition) list)
      (acc : edge list)
      (map : (Evd.econstr * int) list)
      (edge_id : int)
      : edge list * translation_map
      =
      Feedback.msg_info (str "k (inside ')");
      match ts with
      | [] ->
        Feedback.msg_info (str "l (returning from ')");
        acc, map
      | (h_from, h_transition) :: t ->
        Feedback.msg_info (str "m (found h)");
        let lhs_econstr, label_econstr, rhs_econstr =
          h_from, h_transition.edge_ctor, h_transition.to_node
          (* match EConstr.decompose_app sigma h_econstr with
             | _, edge_econstr ->
             let edge_econstr' = Array.to_list edge_econstr in
             ( List.nth edge_econstr' 0
             , List.nth edge_econstr' 1
             , List.nth edge_econstr' 2 ) *)
        in
        Feedback.msg_info
          (str "lhs_ecosntr:" ++ Printer.pr_econstr_env env sigma lhs_econstr);
        Feedback.msg_info (str "label_econstr:" ++ Pp.int label_econstr);
        Feedback.msg_info
          (str "rhs_ecosntr:" ++ Printer.pr_econstr_env env sigma rhs_econstr);
        Feedback.msg_info (str "n (got lhs rhs label)");
        (* let _lhs_hash = Constr.hash (EConstr.to_constr sigma lhs_econstr) in *)
        let lhs_id = Hashtbl.find state_table lhs_econstr in
        (* let _rhs_hash = Constr.hash (EConstr.to_constr sigma rhs_econstr) in *)
        let rhs_id = Hashtbl.find state_table rhs_econstr in
        Feedback.msg_info (str "o (after ids)");
        Feedback.msg_info
          (str
             (Printf.sprintf
                "lhs_id: %d\nrhs_id:%d\nlabel: %d."
                lhs_id
                rhs_id
                label_econstr));
        get_edges'
          t
          (List.concat
             [ [ edge
                   ~label:(econstr_to_string env sigma lhs_econstr)
                   edge_id
                   (ID lhs_id)
                   (ID rhs_id)
               ]
             ; acc
             ])
          (List.concat [ [ h_from, edge_id ]; map ])
          (edge_id + 1)
    in
    Feedback.msg_info (str "j (after def ')");
    return (get_edges' (List.of_seq (H.to_seq transitions)) [] [] 0)
  ;;
  *)

  (*
     (** [pp_fsm_states env sigma states state_table] is ... *)
     let pp_fsm_states
     (env : Environ.env)
     (sigma : Evd.evar_map)
     (states : state list)
     (state_table : (Evd.econstr, int) Hashtbl.t)
     : unit
     =
     Hashtbl.iter
     (fun f i ->
     Feedback.msg_debug
     (Printer.pr_econstr_env env sigma f
     ++ Pp.str
     (Printf.sprintf
     " => \"%s\" (state <%d>)"
     (match find_state i (States states) with
     | None -> "[Error, not found.]"
     | Some state -> state.name)
     i)))
     state_table
     ;;

     (** [pp_fsm_edges env sigma edges edge_table] is ... *)
     let pp_fsm_edges
     (env : Environ.env)
     (sigma : Evd.evar_map)
     (edges : edge list)
     : unit
     =
     (* (edge_table : (Evd.econstr, int) Hashtbl.t) *)

     (* Hashtbl.iter *)
     (* (fun f i -> *)
     Feedback.msg_debug
     (* (Printer.pr_econstr_env env sigma f *)
     (* ++ *)
     (Pp.str (Fsm.Stringify.to_string (Edges edges)))
     ;; *)

  (*
     (** [lts_to_fsm g] is ... *)
  let lts_to_fsm (g : lts_graph) : (fsm * translation) mm =
    let* env = get_env in
    let* sigma = get_sigma in
    match g with
    | { to_visit; transitions; _ } ->
      if Queue.is_empty to_visit
      then (
        (* [states] is a list of [fsm.state]. *)
        (* [state_map] is a tuple list mapping Coq-based [Evd.econstr] to state id [int]. *)
        let* states, state_map = get_states transitions in
        Feedback.msg_info
          (str
             (Printf.sprintf
                "states: %s"
                (Fsm.Stringify.to_string (States states))));
        (* convert [state_map] to a [Hashtbl]. *)
        let (state_table : (Evd.econstr, int) Hashtbl.t) = Hashtbl.create 10 in
        Hashtbl.add_seq state_table (List.to_seq state_map);
        (*  *)
        Feedback.msg_info (str "states_table: ");
        Hashtbl.iter
          (fun f i ->
            Feedback.msg_info
              (Printer.pr_econstr_env env sigma f
               ++ Pp.str (Printf.sprintf " => (state <%d>)" i)))
          state_table;
        Feedback.msg_info (str "(finished printing states_table).\n");
        (* pp_fsm_states env sigma states state_table; *)
        (* [edge_map] is a tuple list mapping Coq-based [Evd.econstr] to edge id [int]. *)
        let* edges, edge_map = get_edges transitions state_table in
        Feedback.msg_info
          (str
             (Printf.sprintf
                "edges: %s"
                (Fsm.Stringify.to_string (Edges edges))));
        (* convert [edge_map] to a [Hashtbl]. *)
        let (edge_table : (Evd.econstr, int) Hashtbl.t) = Hashtbl.create 10 in
        Hashtbl.add_seq edge_table (List.to_seq edge_map);
        (*  *)
        Feedback.msg_info (str "edges_table: ");
        Hashtbl.iter
          (fun f i ->
            Feedback.msg_info
              (Printer.pr_econstr_env env sigma f
               ++ Pp.str (Printf.sprintf " => (edge <%d>)" i)))
          edge_table;
        Feedback.msg_info (str "(finished printing edges_table).\n");
        (* TODO: in the above, change the tables to use the hash of the Evd.econstr. *)
        (* TODO: additionally, then update the [get_edges'] function to use the [_lhs_hash] to obtain the [lhs_id]. *)

        (* ! ! ! note to self: *)
        (* TODO: the [lts_transition M.t] is itself the [lhs -(label)-> rhs]. *)
        (* ? see [pp_graph_transitions] where [H.iter (fun from transition -> ... ] shows that the [key] [from] is the [lhs] state (coq-term) and then the [transition] itself has [transition.edge_ctor] for the label of the transition and [transition.to_node] for the [rhs] state (being a coq-term).*)
        (* TODO: need to check where things are hashed, since i recall and can observe the functionality in [Mebi_monad]. I believe that this is happening later on in [make_graph_builder] when it calls [make_constr_tbl]. I think that I should be using the hashed versions of these [lhs] and [rhs] [Evd.econstr] terms, so that the ocaml side of things does not have to worry about these things, and so that the translation can actually be baked into the output [fsm], and we don't have to carry around a translation table with us. *)

        (*  *)
        let fsm_translation : translation = { state_table; edge_table } in
        (* temp... *)
        return (fsm states edges, fsm_translation))
      else (
        Feedback.msg_warning
          (str
             (Printf.sprintf
                "lts is not complete, still had at least (%d) terms to visit."
                (Queue.length to_visit)));
        raise (UnfinishedLTS g))
  ;; *)

  (* ) *)

  (* )
     edge_table *)
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  let* m = make_constr_tbl in
  let module G = MkGraph ((val m)) in
  return (module G : GraphB)
;;

(**  *)

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
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  let* graph = G.build_graph the_lts tref in
  (* bind [env] and [sigma] to [get_env st] and [get_sigma st] in [mebi_monad], respectively. *)
  let* env = get_env in
  let* sigma = get_sigma in
  Feedback.msg_debug
    (str "(a) Types of terms: "
     ++ Printer.pr_econstr_env env sigma the_lts.trm_type
     ++ strbrk "");
  Feedback.msg_debug
    (str "(b) Types of labels: "
     ++ Printer.pr_econstr_env env sigma the_lts.lbl_type
     ++ strbrk "");
  Feedback.msg_debug
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
  Feedback.msg_debug (strbrk "(e) Graph Edges: \n");
  G.pp_graph_transitions env sigma graph;
  (* lts to fsm *)
  let* the_fsm_aux, translation =
    G.lts_to_fsm graph tref the_lts.trm_type
    (* the_lts.coq_lts *)
  in
  Feedback.msg_info (str "(f) Fsm: \n");
  G.pp_fsm ~long:() env sigma the_fsm_aux;
  (* print states *)
  (* Feedback.msg_info (str "(f) Fsm.states: \n");
     G.pp_fsm_states env sigma the_fsm.states translation.state_table;
     (* print edges *)
     Feedback.msg_info (str "(g) Fsm.edges: \n");
     G.pp_fsm_edges env sigma the_fsm.edges; *)
  (* Feedback.msg_info *)
  (*   (str "(b) CoqFsm: " ++ pp_coq_fsm env sigma (coq_fsm.states, coq_fsm.edges)); *)
  (* match coq_fsm.edges with
     | [] -> Feedback.msg_debug (str "coq_fsm.edges empty. cannot continue")
     | h :: _t ->
     Feedback.msg_debug
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
  (* Feedback.msg_debug
     (str (Printf.sprintf "translated fsm: %s" ++
     (let rec sprintf_tbl  = Printer.pr_econstr_env env sigma )
     )); *)
  (* Feedback.msg_debug
     (str
     (Printf.sprintf
     "translated fsm: %s\n"
     (to_string ~context:ShowIDs (Fsm _fsm)))); *)
  Feedback.msg_debug (str "\n--------\n");
  return ()
;;
