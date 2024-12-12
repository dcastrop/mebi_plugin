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
           (* FIXME: Test this *)
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
  module S : Set.S with type elt = EConstr.t
  (* module L : Set.S with type elt = EConstr.t *)

  type ('a, 'b) transition =
    { action : 'a
    ; destination : 'b
    }

  type lts_transition = (Fsm.action, EConstr.constr) transition

  type lts_graph =
    { to_visit : EConstr.constr Queue.t
        (* Queue for BFS *)
        (* ; labels : L.t *)
    ; states : S.t
    ; transitions : lts_transition H.t
    }

  val build_graph : raw_lts -> Constrexpr.constr_expr_r CAst.t -> lts_graph mm

  type state_translation_table = (EConstr.t, Fsm.state) Hashtbl.t

  val build_states : lts_graph -> (Fsm.States.t * state_translation_table) mm

  val build_edges
    :  lts_graph
    -> state_translation_table
    -> (Fsm.States.t Fsm.Actions.t Fsm.Edges.t * Fsm.Alphabet.t) mm

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
module MkGraph
    (M : Hashtbl.S with type key = EConstr.t)
    (N : Set.S with type elt = EConstr.t) : GraphB =
(* (O : Set.S with type elt = EConstr.t) *)
struct
  module H = M
  module S = N
  (* module L = O *)

  (** [('a, 'b) transition] is a 2-tuple with a [label] and [destination].
      [label] is of type ['a].
      [go_to] is of type ['b]. *)
  type ('a, 'b) transition =
    { action : 'a
    ; destination : 'b
    }

  (** [lts_transition] is a type for describing outgoing transitions of a Coq-based LTS.
      [Fsm.action] is the constructor number.
      [EConstr.constr] is the destination node. *)
  type lts_transition = (Fsm.action, EConstr.constr) transition

  (** [lts_graph] is a type used when building an LTS (graph) from Coq-based terms.
      [to_visit] is a queue of coq terms to explore in BFS.
      [transitions] is a hashtable mapping integers (of hashed constructors) to Coq terms. *)
  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* ; labels : L.t *)
    ; states : S.t
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
      let new_states = ref (S.singleton t) in
      List.iter
        (fun (i, tgt) ->
          Feedback.msg_debug
            (str "\n\nTransition to" ++ Printer.pr_econstr_env env sigma tgt);
          new_states := S.add tgt !new_states;
          H.add
            g.transitions
            t
            { action = { id = i; label = econstr_to_string tgt }
            ; destination = tgt
            };
          if H.mem g.transitions tgt || EConstr.eq_constr sigma tgt t
          then ()
          else Queue.push tgt g.to_visit;
          Feedback.msg_debug
            (str "\nVisiting next: " ++ int (Queue.length g.to_visit)))
        constrs;
      let g = { g with states = S.union g.states !new_states } in
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
    build_lts
      the_lts
      { to_visit = q (* ; labels = L.empty *)
      ; states = S.empty
      ; transitions = H.create bound
      }
  ;;

  open Fsm

  (** [state_translation_table] is is a hashtable mapping [EConstr.t] of terms to [Fsm.states]. *)
  type state_translation_table = (EConstr.t, Fsm.state) Hashtbl.t

  (** [build_states g] returns the set of Fsm.States and for each a mapping from EConstr.t. *)
  let build_states (g : lts_graph) : (Fsm.States.t * state_translation_table) mm
    =
    let map_of_states = S.cardinal g.states |> Hashtbl.create in
    (* set up counter for state ids *)
    let state_id_counter = ref 0 in
    let get_state_id () : int =
      let to_return = !state_id_counter in
      state_id_counter := !state_id_counter + 1;
      to_return
    in
    (* extract all states *)
    let states =
      S.fold
        (fun (s : EConstr.t) (acc : Fsm.States.t) ->
          let new_state =
            Fsm.make_state ~pp:(econstr_to_string s) (get_state_id ())
          in
          Hashtbl.add map_of_states s new_state;
          States.add new_state acc)
        g.states
        Fsm.States.empty
    in
    (*  *)
    return (states, map_of_states)
  ;;

  (** [build_edges g s] is a hashtable mapping FSM states to
      outgoing edges comprised of labels and destination states.
      [g] is the LTS with transitions to build the FSM edges from.
      [s] is the translation map from Coq-terms to FSM states. *)
  let build_edges (g : lts_graph) (s : state_translation_table)
    : (Fsm.States.t Fsm.Actions.t Fsm.Edges.t * Fsm.Alphabet.t) mm
    =
    let* env = get_env in
    let* sigma = get_sigma in
    let keys = H.to_seq_keys g.transitions in
    let (edges : States.t Actions.t Edges.t) =
      Seq.length keys |> Fsm.Edges.create
    in
    (* get actions first *)
    let alphabet =
      H.fold
        (fun (from : EConstr.t)
          (transition : (Fsm.action, EConstr.constr) transition)
          (acc : Fsm.Alphabet.t) ->
          (* only add if action with same label doesnt exist *)
          if Fsm.Alphabet.exists
               (fun (a : Fsm.action) ->
                 String.equal a.label transition.action.label)
               acc
          then acc
          else
            Fsm.Alphabet.add
              (Fsm.make_action
                 ~label:transition.action.label
                 (Fsm.Alphabet.cardinal acc))
              acc)
        g.transitions
        Fsm.Alphabet.empty
    in
    (* build edges *)
    H.iter
      (fun (from : EConstr.t)
        (transition : (Fsm.action, EConstr.constr) transition) ->
        let edge_from = Hashtbl.find s from in
        let edge_dest = Hashtbl.find s transition.destination in
        let edge_action =
          List.nth
            (let pstr_actions =
               Fsm.Alphabet.filter
                 (fun (a : action) ->
                   String.equal a.label transition.action.label)
                 alphabet
             in
             Fsm.Alphabet.to_list pstr_actions)
            0
        in
        (* check state already has edges *)
        match Edges.find_opt edges edge_from with
        | None ->
          (* add new *)
          Edges.add
            edges
            edge_from
            (Actions.of_seq
               (List.to_seq [ edge_action, States.of_list [ edge_dest ] ]))
        | Some actions ->
          (* add to outgoing edges *)
          Edges.add
            edges
            edge_from
            (Actions.of_seq
               (List.to_seq
                  (List.append
                     (List.of_seq (Actions.to_seq actions))
                     [ ( edge_action
                       , match Actions.find_opt actions edge_action with
                         (* add new *)
                         | None -> States.of_list [ edge_dest ]
                         (* add to actions of outgoing edges*)
                         | Some destinations ->
                           States.add edge_dest destinations )
                     ]))))
      g.transitions;
    return (edges, alphabet)
  ;;

  (** Error when trying to translate an unfinished LTS to FSM. *)
  exception UnfinishedLTS of lts_graph

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
    | { action; destination } ->
      Printf.sprintf
        "%s ---{ %s }--> %s"
        (econstr_to_string from)
        (match long with
         | None -> Printf.sprintf "%d" action.id
         | Some () -> action.label)
        (econstr_to_string destination)
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
      (* we require the lts to be complete *)
      if Bool.not (Queue.is_empty to_visit)
      then (
        (* do not proceed *)
        Feedback.msg_warning
          (str
             (Printf.sprintf
                "lts is not complete, still had at least (%d) terms to visit."
                (Queue.length to_visit)));
        raise (UnfinishedLTS g))
      else
        (* extract states *)
        let* states, state_translation_map = build_states g in
        (* extract edges *)
        let* edges, alphabet = build_edges g state_translation_map in
        (* get initial state *)
        let$ t env sigma =
          Constrintern.interp_constr_evars env sigma init_term
        in
        let$ init_term env sigma = sigma, Reductionops.nf_all env sigma t in
        let init_state = Hashtbl.find state_translation_map init_term in
        (* return fsm and state_translation_map *)
        return
          ({ init = init_state; alphabet; states; edges }, state_translation_map)
  ;;
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  let* m = make_constr_tbl in
  let* s = make_constr_set in
  (* let* l = make_constr_set in *)
  let module G = MkGraph ((val m)) ((val s)) (* ((val l)) *) in
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
  (* prints all transitions -- the possible constructors.
     a term may take as part of its structure.
     these are dependant on the definition of a type. *)
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
  Feedback.msg_debug
    (str (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (Exa1)\n"));
  Feedback.msg_debug (str (Printf.sprintf "exa1.s: %s" (Fsm.pstr_fsm ~pp:() s)));
  Feedback.msg_debug (str (Printf.sprintf "exa1.t: %s" (Fsm.pstr_fsm ~pp:() t)));
  (* run algorithm *)
  let are_bisimilar, pi = RCP.KS90.run s t in
  (* print out results *)
  Feedback.msg_notice
    (str
       (Printf.sprintf
          "(s ~ t) = %b.%s"
          are_bisimilar
          (if Bool.not are_bisimilar
           then
             Printf.sprintf
               "\nnon-bisimilar partition: %s"
               (RCP.KS90.pstr_partition ~pp:() pi)
           else "")));
  Feedback.msg_info
    (str
       (Printf.sprintf
          "where s = %s\nand t = %s."
          (Fsm.pstr_fsm ~pp:() s)
          (Fsm.pstr_fsm ~pp:() t)));
  Feedback.msg_debug
    (str
       (Printf.sprintf "\n--------\npi: %s" (RCP.KS90.pstr_partition ~pp:() pi)));
  Feedback.msg_debug (str (Printf.sprintf "\n= = = = = = = = = =\n"));
  ()
;;

(** [bisim_exa2_ks90] *)
let bisim_exa2_ks90 : unit =
  let s, t = RCP.Examples.exa_2 in
  Feedback.msg_debug
    (str (Printf.sprintf "\n= = = = = = = = = =\nRCP.KS90 (Exa2)\n"));
  Feedback.msg_debug (str (Printf.sprintf "exa2.s: %s" (Fsm.pstr_fsm ~pp:() s)));
  Feedback.msg_debug (str (Printf.sprintf "exa2.t: %s" (Fsm.pstr_fsm ~pp:() t)));
  (* run algorithm *)
  let are_bisimilar, pi = RCP.KS90.run s t in
  (* print out results *)
  Feedback.msg_notice
    (str
       (Printf.sprintf
          "(s ~ t) = %b.%s"
          are_bisimilar
          (if Bool.not are_bisimilar
           then
             Printf.sprintf
               "\nnon-bisimilar partition: %s"
               (RCP.KS90.pstr_partition ~pp:() pi)
           else "")));
  Feedback.msg_info
    (str
       (Printf.sprintf
          "where s = %s\nand t = %s."
          (Fsm.pstr_fsm ~pp:() s)
          (Fsm.pstr_fsm ~pp:() t)));
  Feedback.msg_debug
    (str
       (Printf.sprintf "\n--------\npi: %s" (RCP.KS90.pstr_partition ~pp:() pi)));
  Feedback.msg_debug (str (Printf.sprintf "\n= = = = = = = = = =\n"));
  ()
;;
