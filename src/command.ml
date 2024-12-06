open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax
open Bisim_algs
open Pp_ext

(** ['a mm] is a function type mapping from [coq_context ref] to ['a in_context]. *)
type 'a mm = 'a Mebi_monad.t

(* TODO: should maybe be moved to [mebi_monad.ml]? *)

(** [econstr_to_string target] is a [string] representing [target]. *)
let econstr_to_string (target : EConstr.t) : string =
  let econstr_to_string' (target : Evd.econstr) : string mm =
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma target))
  in
  run (econstr_to_string' target)
;;

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

(** [get_lts_labels_and_terms mib mip] is the mapping of terms (states)
    and labels (outgoing edges) from [mip].

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

(** [raw_lts] is an LTS type used to describe Coq-based terms.
    [coq_lts] is the type constructor.
    [trm_type] is the type of terms for the LTS.
    [lbl_type] is the type of labels for the LTS.
    [coq_ctor_names] is the array of names for each constructor of the Coq term.
    [transitions] is the array of constructors of the Coq term
    (i.e., the transitions or outgoing edges). *)
type raw_lts =
  { coq_lts : EConstr.constr
  ; trm_type : EConstr.types
  ; lbl_type : EConstr.types
  ; coq_ctor_names : Names.Id.t array
  ; constructor_transitions : (Constr.rel_context * Constr.types) array
  }

(** [check_ref_lts gref] is the [raw_lts] of [gref].

    Raises error ([invalid_ref]) if [gref] is not a reference to an inductive type. *)
let check_ref_lts (gref : Names.GlobRef.t) : raw_lts mm =
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
      ; constructor_transitions = mip.mind_nf_lc
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
    let ctx, tm = lts.constructor_transitions.(i) in
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
  iterate 0 (Array.length lts.constructor_transitions - 1) [] iter_body
;;

(** [bound] is the total depth that will be explored of a given lts by [explore_lts]. *)
let bound : int = 5
(* TODO: get this as user input! *)

(** [GraphB] is ...
    (Essentially acts as a `.mli` for the [MkGraph] module.) *)
module type GraphB = sig
  module H : Hashtbl.S with type key = EConstr.t

  type lts_transition = (Fsm.action, EConstr.constr) Fsm.transition

  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* Queue for BFS *)
    ; transitions : lts_transition H.t
    }

  val build_graph : raw_lts -> Constrexpr.constr_expr_r CAst.t -> lts_graph mm

  type state_translation_table = (EConstr.t, Fsm.state) Hashtbl.t

  val build_state_translation_table : lts_graph -> state_translation_table mm

  val build_edges
    :  lts_graph
    -> state_translation_table
    -> (Fsm.edges * Fsm.Actions.t) mm

  val lts_to_fsm
    :  lts_graph
    -> Constrexpr.constr_expr_r CAst.t
    -> (Fsm.fsm * state_translation_table) mm

  val pstr_lts_transition
    :  ?long:unit
    -> EConstr.constr * lts_transition
    -> string

  val pstr_lts_transitions
    :  ?long:unit
    -> ?indent:int
    -> lts_transition H.t
    -> string

  val pstr_lts : ?long:unit -> lts_graph -> string
  (* val pp_fsm : ?long:unit -> Fsm.fsm -> unit *)
end

(** [MkGraph M] is ...
    [M] is a ... *)
module MkGraph (M : Hashtbl.S with type key = EConstr.t) : GraphB = struct
  module H = M

  (** [lts_transition] is a type for describing outgoing transitions of a Coq-based LTS.
      [Fsm.action] is the constructor number.
      [EConstr.constr] is the destination node. *)
  type lts_transition = (Fsm.action, EConstr.constr) Fsm.transition

  (** [lts_graph] is a type used when building an LTS (graph) from Coq-based terms.
      [to_visit] is a queue of coq terms to explore.
      [transitions] is a hashtable mapping integers (of hashed constructors) to Coq terms. *)
  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* Queue for BFS *)
    ; transitions : lts_transition H.t
    }

  (** [build_lts the_lts g] is an [lts_graph] [g] obtained by exploring [the_lts].
      [the_lts] describes the Coq-based term.
      [g] is an [lts_graph] accumulated while exploring [the_lts]. *)
  let rec build_lts (the_lts : raw_lts) (g : lts_graph) : lts_graph mm =
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
          H.add
            g.transitions
            t
            { action = { id = i; label = econstr_to_string tgt }
            ; to_state = tgt
            };
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
      [t] is the original Coq-term. *)
  let build_graph (the_lts : raw_lts) (t : Constrexpr.constr_expr_r CAst.t)
    : lts_graph mm
    =
    let$ t env sigma = Constrintern.interp_constr_evars env sigma t in
    let$* u env sigma = Typing.check env sigma t the_lts.trm_type in
    let$ t env sigma = sigma, Reductionops.nf_all env sigma t in
    let q = Queue.create () in
    let* _ = return (Queue.push t q) in
    build_lts the_lts { to_visit = q; transitions = H.create bound }
  ;;

  open Fsm

  (** [state_translation_table] is is a hashtable mapping [EConstr.t] of terms to [Fsm.states]. *)
  type state_translation_table = (EConstr.t, Fsm.state) Hashtbl.t

  (** [build_state_translation_table g] is a hashtable mapping the Coq-based endpoints
      of transitions of LTS [g] to pure OCaml FSM states. *)
  let build_state_translation_table (g : lts_graph) : state_translation_table mm
    =
    let* sigma = get_sigma in
    let hash t =
      EConstr.to_constr ?abort_on_undefined_evars:(Some false) sigma t
      |> Constr.hash
    in
    let keys = H.to_seq_keys g.transitions in
    let tr_tbl = Seq.length keys |> Hashtbl.create in
    (* set up counter for state ids *)
    let state_id_counter = ref 0 in
    let get_state_id () : int =
      let to_return = !state_id_counter in
      state_id_counter := !state_id_counter + 1;
      to_return
    in
    (* for each key, extract states from transitions. *)
    let _ =
      Seq.iter
        (fun t ->
          (* check if [t] is already captured *)
          if false == Hashtbl.mem tr_tbl t
          then
            Hashtbl.add
              tr_tbl
              t
              { id = get_state_id (); hash = hash t; pp = econstr_to_string t };
          (* check if [dest_state] is already captured *)
          let dest_state = (H.find g.transitions t).to_state in
          if false == Hashtbl.mem tr_tbl dest_state
          then
            Hashtbl.add
              tr_tbl
              dest_state
              { id = get_state_id ()
              ; hash = hash dest_state
              ; pp = econstr_to_string dest_state
              })
        keys
    in
    return tr_tbl
  ;;

  (** [build_edges g s] is a hashtable mapping FSM states to
      outgoing edges comprised of labels and destination states.
      [g] is the LTS with transitions to build the FSM edges from.
      [s] is the translation map from Coq-terms to FSM states. *)
  let build_edges (g : lts_graph) (s : state_translation_table)
    : (Fsm.edges * Fsm.Actions.t) mm
    =
    let* env = get_env in
    let* sigma = get_sigma in
    let keys = H.to_seq_keys g.transitions in
    let edges = Seq.length keys |> Hashtbl.create in
    (* get actions first *)
    let actions =
      H.fold
        (fun (from : EConstr.t)
          (transition : (Fsm.action, EConstr.constr) Fsm.transition)
          (acc : Fsm.Actions.t) ->
          if Fsm.Actions.exists
               (fun (a : Fsm.action) ->
                 String.equal a.label transition.action.label)
               acc
          then acc
          else
            Fsm.Actions.add
              { id = Fsm.Actions.cardinal acc; label = transition.action.label }
              acc)
        g.transitions
        Fsm.Actions.empty
    in
    H.iter
      (fun (from : EConstr.t)
        (transition : (Fsm.action, EConstr.constr) Fsm.transition) ->
        let edge_from = Hashtbl.find s from in
        let edge_dest = Hashtbl.find s transition.to_state in
        let edge_action =
          List.nth
            (let pstr_actions =
               Fsm.Actions.filter
                 (fun (a : action) ->
                   String.equal a.label transition.action.label)
                 actions
             in
             Fsm.Actions.to_list pstr_actions)
            0
        in
        Hashtbl.add
          edges
          edge_from
          { action = edge_action; to_state = edge_dest })
      g.transitions;
    return (edges, actions)
  ;;

  (** Error when trying to translate an unfinished LTS to FSM. *)
  exception UnfinishedLTS of lts_graph

  (** Error when a translation from an [lts_graph] to an FSM yields no [states].*)
  exception NoStates of state_translation_table

  (** Error when duplication actions found. *)
  exception DuplicateActions of Actions.t

  (* TODO: below is unused, delete? *)
  (* let pp_fsm ?(long : unit option) (the_fsm : Fsm.fsm) : unit =
     Feedback.msg_debug
     (str
     (match long with
     | None -> pstr_fsm the_fsm
     | Some () -> pstr_fsm ~long:() the_fsm))
     ;; *)

  (** [pstr_lts_transition (from, t)] is a string transition:
      [(from) --(t.edge_ctor)--> (t.to_node)].
      [from] is the starting node.
      [t] is an (outgoing) [lts_transition] composed of a label [t.edge_ctor]
      and a destination node [t.to_node]. *)
  let pstr_lts_transition
    ?(long : unit option)
    ((from : EConstr.constr), (t : lts_transition))
    : string
    =
    match t with
    | { action; to_state } ->
      Printf.sprintf
        "%s ---{ %s }--> %s"
        (econstr_to_string from)
        (match long with
         | None -> Printf.sprintf "%d" action.id
         | Some () -> action.label)
        (econstr_to_string to_state)
  ;;

  (** [pstr_lts_transitions transitions] is a string of [transitions]. *)
  let pstr_lts_transitions
    ?(long : unit option)
    ?(indent : int = 1)
    (transitions : lts_transition H.t)
    : string
    =
    if H.to_seq_keys transitions |> Seq.is_empty
    then "[ ] (empty)"
    else
      Printf.sprintf
        "[%s]"
        (H.fold
           (fun (from_node : EConstr.constr)
             (outgoing_transition : lts_transition)
             (acc : string) ->
             Printf.sprintf
               "%s%s{%s}\n"
               acc
               (str_tabs indent)
               (pstr_lts_transition (from_node, outgoing_transition)))
           transitions
           "\n")
  ;;

  (** [pstr_lts_to_visit ?indent nodes_to_visit] is a string of [nodes_to_visit]. *)
  let pstr_lts_to_visit
    ?(indent : int = 1)
    (nodes_to_visit : EConstr.constr Queue.t)
    : string
    =
    let s =
      Printf.sprintf
        "[%s]"
        (Queue.fold
           (fun (acc : string) (node_to_visit : EConstr.constr) ->
             Printf.sprintf "%s{%s}\n" acc (econstr_to_string node_to_visit))
           "\n"
           nodes_to_visit)
    in
    if s == "[\n]" then "[ ] (empty)" else s
  ;;

  (** [pstr_lts g] is a string of the LTS [g]. *)
  let pstr_lts ?(long : unit option) (g : lts_graph) : string =
    Printf.sprintf
      "%s\n%s"
      (Printf.sprintf "to_visit: %s" (pstr_lts_to_visit g.to_visit))
      (Printf.sprintf "transitions: %s" (pstr_lts_transitions g.transitions))
  ;;

  (** [lts_to_fsm g init_term term] translates LTS [g] to an FSM.
      [g] is an [lts_graph] to be translated.
      [init_term] is the original actual Coq-term used to build the LTS.
      (Used to determine the initial state of the FSM). *)
  let lts_to_fsm (g : lts_graph) (init_term : Constrexpr.constr_expr_r CAst.t)
    : (Fsm.fsm * state_translation_table) mm
    =
    match g with
    | { to_visit; transitions; _ } ->
      if Queue.is_empty to_visit
      then (
        let* state_translation = build_state_translation_table g in
        (* extract states *)
        let states =
          States.of_list
            (Hashtbl.fold
               (fun _ state acc -> List.concat [ acc; [ state ] ])
               state_translation
               [])
        in
        if Hashtbl.to_seq_keys state_translation |> Seq.is_empty
        then (
          Feedback.msg_warning
            (str
               (Printf.sprintf
                  "no states were extracted from lts: %s"
                  (pstr_lts g)));
          raise (NoStates state_translation))
        else
          (* TODO: make [build_edges] use [States] rather than [state_translation]. *)
          let* edges, actions = build_edges g state_translation in
          (* check no duplicate actions *)
          if Actions.for_all
               (fun (a : action) ->
                 Actions.exists
                   (fun (b : action) ->
                     Bool.not
                       (Int.equal a.id b.id == String.equal a.label b.label))
                   actions)
               actions
          then (
            Feedback.msg_warning
              (str
                 (Printf.sprintf
                    "duplicate actions found: %s"
                    (Fsm.pstr_actions actions)));
            raise (DuplicateActions actions));
          (* need to get initial state.
             ( copied from [build_graph].) *)
          let$ t env sigma =
            Constrintern.interp_constr_evars env sigma init_term
          in
          let$ init_term env sigma = sigma, Reductionops.nf_all env sigma t in
          let init_state = Hashtbl.find state_translation init_term in
          return
            ({ init = init_state; states; actions; edges }, state_translation))
      else (
        Feedback.msg_warning
          (str
             (Printf.sprintf
                "lts is not complete, still had at least (%d) terms to visit."
                (Queue.length to_visit)));
        raise (UnfinishedLTS g))
  ;;
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
  (* TODO: how to get input from user also? *)
  let* raw_lts = check_ref_lts iref in
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  let* graph_lts = G.build_graph raw_lts tref in
  (* bind [env] and [sigma] to [get_env st] and [get_sigma st] in [mebi_monad], respectively. *)
  let* env = get_env in
  let* sigma = get_sigma in
  Feedback.msg_debug
    (str
       (Printf.sprintf
          "(a) Types of terms: %s.\n"
          (econstr_to_string raw_lts.trm_type)));
  Feedback.msg_debug
    (str
       (Printf.sprintf
          "(b) Types of labels: %s.\n"
          (econstr_to_string raw_lts.lbl_type)));
  Feedback.msg_debug
    (str "(c) Constructors: "
     ++ Pp.prvect_with_sep
          (fun _ -> str ", ")
          Names.Id.print
          raw_lts.coq_ctor_names);
  (* prints all transitions -- the possible constructors
     a term may take as part of its structure.
     these are dependant on the definition of a type *)
  (* TODO: update to be consistent with above and use [econstr_to_string ...].
     TODO: (requires changing [pp_transitions] (and alike) to print to string instead.) *)
  Feedback.msg_debug
    (str "(d) Transitions: "
     ++ pp_transitions env sigma raw_lts.constructor_transitions
     ++ strbrk "\n");
  Feedback.msg_debug
    (str
       (Printf.sprintf
          "(e) Graph Edges: %s.\n"
          (G.pstr_lts_transitions graph_lts.transitions)));
  (* lts to fsm *)
  let* the_fsm, translation = G.lts_to_fsm graph_lts tref in
  Feedback.msg_debug
    (str
       (Printf.sprintf "(f) Fsm: %s.\n" (Fsm.pstr_fsm ~ids:() ~pp:() the_fsm)));
  (*  *)
  Feedback.msg_debug (str "\n--------\n");
  return ()
;;

(** [bisim_exa1_ks90] *)
let bisim_exa1_ks90 : unit =
  let s, t = RCP.Examples.exa_1 in
  Feedback.msg_warning
    (str (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (Exa1)\n"));
  Feedback.msg_warning
    (str (Printf.sprintf "exa1.s: %s" (Fsm.pstr_fsm ~pp:() s)));
  Feedback.msg_warning
    (str (Printf.sprintf "exa1.t: %s" (Fsm.pstr_fsm ~pp:() t)));
  (* run algorithm *)
  let pi = RCP.KS90.run s t in
  (* print out results *)
  Feedback.msg_warning
    (str
       (Printf.sprintf "\n--------\npi: %s" (RCP.KS90.pstr_partition ~pp:() pi)));
  Feedback.msg_warning (str (Printf.sprintf "\n= = = = = = = = = =\n"));
  ()
;;

(** [bisim_exa2_ks90] *)
let bisim_exa2_ks90 : unit =
  let s, t = RCP.Examples.exa_1 in
  Feedback.msg_warning
    (str (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (Exa2)\n"));
  Feedback.msg_warning
    (str (Printf.sprintf "exa2.s: %s" (Fsm.pstr_fsm ~pp:() s)));
  Feedback.msg_warning
    (str (Printf.sprintf "exa2.t: %s" (Fsm.pstr_fsm ~pp:() t)));
  (* run algorithm *)
  let pi = RCP.KS90.run s t in
  (* print out results *)
  Feedback.msg_warning
    (str
       (Printf.sprintf "\n--------\npi: %s" (RCP.KS90.pstr_partition ~pp:() pi)));
  (* let _ =
     RCP.KS90.Partition.fold
     (fun (states : Fsm.States.t) (acc : int) ->
     Feedback.msg_warning
     (str (Printf.sprintf "s_pi %d : %s" acc (Fsm.pstr_states states)));
     acc + 1)
     s_pi
     0
     in
     Feedback.msg_warning (str (Printf.sprintf "\n--------\nt_pi:"));
     let _ =
     RCP.KS90.Partition.fold
     (fun (states : Fsm.States.t) (acc : int) ->
     Feedback.msg_warning
     (str (Printf.sprintf "t_pi %d : %s" acc (Fsm.pstr_states states)));
     acc + 1)
     t_pi
     0
     in *)
  Feedback.msg_warning (str (Printf.sprintf "\n= = = = = = = = = =\n"));
  ()
;;
