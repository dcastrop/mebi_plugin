open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax
open Pp_ext

(* *)
open Fsm
open Utils

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

(** [get_lts_labels_and_terms mib mip] is the mapping of terms (states) and labels (outgoing edges) from [mip].

    @raise invalid_arity
      if lts terms and labels cannot be obtained from [mip]. [mib] is only used in case of error. *)
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
    @param [coq_lts] is the type constructor.
    @param [trm_type] is the type of terms for the LTS.
    @param [lbl_type] is the type of labels for the LTS.
    @param [coq_ctor_names]
      is the array of names for each constructor of the Coq term.
    @param [constructor_transitions]
      is the array of constructors of the Coq term (i.e., the transitions or outgoing edges). *)
type raw_lts =
  { coq_lts : EConstr.constr
  ; trm_type : EConstr.types
  ; lbl_type : EConstr.types
  ; coq_ctor_names : Names.Id.t array
  ; constructor_transitions : (Constr.rel_context * Constr.types) array
  }

(** [check_ref_lts gref] is the [raw_lts] of [gref].

    @raise invalid_ref if [gref] is not a reference to an inductive type. *)
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
let m_unify ?(show_debug : bool = false) (t0 : Evd.econstr) (t1 : Evd.econstr)
  : bool mm
  =
  let* _ =
    debug ~show_debug (fun (env : Environ.env) (sigma : Evd.evar_map) ->
      str "Unifying "
      ++ Printer.pr_econstr_env env sigma t0
      ++ strbrk "\n"
      ++ str "Unifying "
      ++ Printer.pr_econstr_env env sigma t1)
  in
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      if show_debug then Feedback.msg_debug (str "\t\tSuccess");
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify (m, n, e))
      ->
      if show_debug then Feedback.msg_debug (str "\t\tCould not unify");
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
    (* *)
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

(* let rec pstr_unif_problem (t : unif_problem) : string =
  match t with
  | { termL; termR; _ } ->
    Printf.sprintf
      "{ L: %s;\n  R: %s; }"
      (econstr_to_string termL)
      (econstr_to_string termR)
;; *)

type 'a tree = Node of 'a * 'a tree list

(* type unif_tree =
   | Tree of int tree
   | Start
   | Failed *)

let rec pstr_int_tree (t : int tree) : string =
  match t with
  | Node (lhs_int, rhs_int_tree_list) ->
    Printf.sprintf
      "(%d) [%s]"
      lhs_int
      (List.fold_left
         (fun (acc : string) (rhs_int_tree : int tree) ->
            pstr_int_tree rhs_int_tree)
         ""
         rhs_int_tree_list)
;;

(* change type to: *)
(* (int tree * unif_problem) list -> int tree list option t *)
(* *)
let rec unify_all
          ?(show_debug : bool = false)
          (i : (int tree * unif_problem) list)
  : int tree list option t
  =
  match i with
  | [] -> return (Some [])
  | (ctor_tree, u) :: t ->
    let* _ =
      debug ~show_debug (fun env sigma ->
        str "UNIFYALL:::::::::: "
        ++ Printer.pr_econstr_env env sigma u.termL
        ++ strbrk "\n::::::::::"
        ++ Printer.pr_econstr_env env sigma u.termR)
    in
    let* success = m_unify ~show_debug u.termL u.termR in
    if success
    then
      let* unified = unify_all ~show_debug t in
      match unified with
      | None -> return None
      | Some unified -> return (Some (List.append [ ctor_tree ] unified))
    else return None
;;

let sandboxed_unify
      ?(show_debug : bool = false)
      (tgt_term : EConstr.t)
      (u : (int tree * unif_problem) list)
  : (EConstr.t * int tree list) option mm
  =
  let* _ =
    debug ~show_debug (fun env sigma ->
      str "TGT:::::: " ++ Printer.pr_econstr_env env sigma tgt_term)
  in
  sandbox
    (let* success = unify_all ~show_debug u in
     match success with
     | None -> return None
     | Some unified ->
       let$+ term env sigma = Reductionops.nf_all env sigma tgt_term in
       let$+ is_undefined _ sigma = EConstr.isEvar sigma term in
       if is_undefined then return None else return (Some (term, unified)))
;;

(* [act] should probably come from the unification problems? *)
let rec retrieve_tgt_nodes
          ?(show_debug : bool = false)
          (acc : (EConstr.t * EConstr.t * int tree) list)
          (i : int)
          (act : EConstr.t)
          (tgt_term : EConstr.t)
  :  (int tree * unif_problem) list list
  -> (EConstr.t * EConstr.t * int tree) list t
  =
  (* :  (int tree * unif_problem) list list -> (EConstr.t * int tree list) list t *)
  function
  | [] -> return acc
  | u1 :: nctors ->
    let* success = sandboxed_unify ~show_debug tgt_term u1 in
    (match success with
     | None -> retrieve_tgt_nodes ~show_debug acc i act tgt_term nctors
     | Some (tgt, ctor_tree) ->
       let$+ act env sigma = Reductionops.nf_all env sigma act in
       retrieve_tgt_nodes
         ~show_debug
         ((act, tgt, Node (i, ctor_tree)) :: acc)
         i
         act
         tgt_term
         nctors)
;;

(* Should return a list of unification problems *)
let rec check_updated_ctx
          (acc : (int tree * unif_problem) list list)
          (lts : raw_lts)
  :  EConstr.t list * EConstr.rel_declaration list
  -> (int tree * unif_problem) list list option t
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
             List.map
               (fun (_, (tL : EConstr.t), (i : int tree)) ->
                  i, { termL = tL; termR = args.(2) })
               ctors
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
and check_valid_constructor
      ?(show_debug : bool = false)
      (lts : raw_lts)
      (t : EConstr.t)
  : (EConstr.t * EConstr.t * int tree) list t
  =
  (* : (EConstr.t * int tree list) list t *)
  let$+ t env sigma = Reductionops.nf_all env sigma t in
  (* let iter_body (i : int) (ctor_vals : (EConstr.t * int tree list) list) = *)
  let iter_body (i : int) (ctor_vals : (EConstr.t * EConstr.t * int tree) list) =
    let* _ =
      debug ~show_debug (fun env sigma ->
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
      let* (next_ctors : (int tree * unif_problem) list list option) =
        check_updated_ctx [ [] ] lts (substl, ctx_tys)
      in
      let (tgt_term : EConstr.t) = EConstr.Vars.substl substl termR in
      (match next_ctors with
       | None -> return ctor_vals
       | Some [] ->
         let* sg = get_sigma in
         if EConstr.isEvar sg tgt_term
         then return ctor_vals
         else
           let$+ act env sigma = Reductionops.nf_all env sigma act in
           return ((act, tgt_term, Node (i, [])) :: ctor_vals)
       | Some nctors ->
         let tgt_nodes =
           retrieve_tgt_nodes ~show_debug ctor_vals i act tgt_term nctors
         in
         tgt_nodes)
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
    ; index_tree : int tree
    }

  type lts_transition = (action, EConstr.constr) transition

  type lts_graph =
    { to_visit : EConstr.constr Queue.t
      (* Queue for BFS *)
      (* ; labels : L.t *)
    ; states : S.t
    ; transitions : lts_transition H.t
    }

  val build_graph
    :  ?show_debug:bool
    -> raw_lts
    -> Constrexpr.constr_expr_r CAst.t
    -> lts_graph mm

  type state_translation_table = (EConstr.t, state) Hashtbl.t

  val build_states
    :  ?show_debug:bool
    -> lts_graph
    -> (States.t * state_translation_table) mm

  val build_edges
    :  ?show_debug:bool
    -> lts_graph
    -> state_translation_table
    -> (States.t Actions.t Edges.t * Alphabet.t) mm

  val lts_to_fsm
    :  ?show_debug:bool
    -> lts_graph
    -> Constrexpr.constr_expr_r CAst.t
    -> (fsm * state_translation_table) mm

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
  (* val pp_fsm : ?long:unit -> fsm -> unit *)
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

  (** [('a, 'b) transition] is a 3-tuple comprised of:
      - [action] which is typically of type [Fsm.action].
      - [destination] of type ['b].
      - [index_tree] of type [int tree], which helps us handle recursion. *)
  type ('a, 'b) transition =
    { action : 'a
    ; destination : 'b
    ; index_tree : int tree
    }

  (** [lts_transition] is a type for describing outgoing transitions of a Coq-based LTS.
      - [action] is the constructor number.
      - [EConstr.constr] is the destination node. *)
  type lts_transition = (action, EConstr.constr) transition

  (** [lts_graph] is a type used when building an LTS (graph) from Coq-based terms.
      - [to_visit] is a queue of coq terms to explore in BFS.
      - [states] is the set of coq terms visited so far.
      - [transitions] is a hashtable mapping integers (of hashed constructors) to Coq terms. *)
  type lts_graph =
    { to_visit : EConstr.constr Queue.t (* ; labels : L.t *)
    ; states : S.t
    ; transitions : lts_transition H.t
    }

  (** [build_lts the_lts g] is an [lts_graph] [g] obtained by exploring [the_lts].
      @param the_lts describes the Coq-based term.
      @param g is an [lts_graph] accumulated while exploring [the_lts].
      @return an [lts_graph] constructed so long as the [bound] is not exceeded. *)
  let rec build_lts
            ?(show_debug : bool = false)
            (the_lts : raw_lts)
            (g : lts_graph)
    : lts_graph mm
    =
    if H.length g.transitions >= bound
    then return g (* FIXME: raise error *)
    else if Queue.is_empty g.to_visit
    then return g
    else
      let* t = return (Queue.pop g.to_visit) in
      let* (constrs : (EConstr.t * EConstr.t * int tree) list) =
        check_valid_constructor ~show_debug the_lts t
      in
      if show_debug
      then
        Feedback.msg_debug
          (str
             (Printf.sprintf
                "---- (returned from check_valid_constructor)\n\n\
                 build_lts: constrs: [%s] (length %d).\n"
                (List.fold_left
                   (fun (acc : string)
                     ((act, ctor, int_tree) : EConstr.t * EConstr.t * int tree) ->
                      Printf.sprintf
                        "%s   (%s ::\n    %s[%s])\n"
                        acc
                        (pstr_int_tree int_tree)
                        (econstr_to_string ctor)
                        (econstr_to_string act))
                   "\n"
                   constrs)
                (List.length constrs)));
      let* env = get_env in
      let* sigma = get_sigma in
      let new_states = ref (S.singleton t) in
      (* set up counter for transition ids *)
      let transition_id_counter = ref 0 in
      let get_transition_id () : int =
        let to_return = !transition_id_counter in
        transition_id_counter := to_return + 1;
        to_return
      in
      List.iter
        (fun ((act, tgt, int_tree) : EConstr.t * EConstr.t * int tree) ->
           if show_debug
           then
             Feedback.msg_debug
               (str "\n\nTransition to" ++ Printer.pr_econstr_env env sigma tgt);
           new_states := S.add tgt !new_states;
           H.add
             g.transitions
             t
             { action =
                 { id = get_transition_id (); label = econstr_to_string act }
             ; index_tree = int_tree
             ; destination = tgt
             };
           if H.mem g.transitions tgt || EConstr.eq_constr sigma tgt t
           then ()
           else Queue.push tgt g.to_visit;
           if show_debug
           then
             Feedback.msg_debug
               (str "\nVisiting next: " ++ int (Queue.length g.to_visit)))
        constrs;
      let g = { g with states = S.union g.states !new_states } in
      build_lts ~show_debug the_lts g
  ;;

  (** [build_graph the_lts t] is ...
      @param the_lts is ...
      @param t is the original Coq-term. *)
  let build_graph
        ?(show_debug : bool = false)
        (the_lts : raw_lts)
        (t : Constrexpr.constr_expr_r CAst.t)
    : lts_graph mm
    =
    let$ t env sigma = Constrintern.interp_constr_evars env sigma t in
    let$* u env sigma = Typing.check env sigma t the_lts.trm_type in
    let$ t env sigma = sigma, Reductionops.nf_all env sigma t in
    let q = Queue.create () in
    let* _ = return (Queue.push t q) in
    build_lts
      ~show_debug
      the_lts
      { to_visit = q (* ; labels = L.empty *)
      ; states = S.empty
      ; transitions = H.create bound
      }
  ;;

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
    | { action; index_tree; destination } ->
      Printf.sprintf
        "%s\n\t---{ %s\n\t}--> %s"
        (econstr_to_string from)
        (match long with
         | None -> Printf.sprintf "%d" action.id
         | Some () ->
           Printf.sprintf
             "\n\t\t(id: %d)\n\t\t(label: %s)\n\t\t(index_tree: %s)"
             action.id
             action.label
             (pstr_int_tree index_tree))
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
                "%s%s{%s}\n%s"
                acc
                (str_tabs indent)
                (match long with
                 | None -> pstr_lts_transition (from_node, outgoing_transition)
                 | Some () ->
                   pstr_lts_transition ~long:() (from_node, outgoing_transition))
                (match long with
                 | None -> ""
                 | Some () -> "\n"))
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

  (** [state_translation_table] is is a hashtable mapping [EConstr.t] of terms to [states]. *)
  type state_translation_table = (EConstr.t, state) Hashtbl.t

  (** [build_states g] returns the set of States and for each a mapping from EConstr.t. *)
  let build_states ?(show_debug : bool = false) (g : lts_graph)
    : (States.t * state_translation_table) mm
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
        (fun (s : EConstr.t) (acc : States.t) ->
           (* check if [map_of_states] has *)
           match Hashtbl.find_opt map_of_states s with
           | None ->
             (* add as new state *)
             let new_state =
               make_state ~pp:(econstr_to_string s) (get_state_id ())
             in
             Hashtbl.add map_of_states s new_state;
             States.add new_state acc
           | Some existing_state ->
             (* do not add new state, already translated *)
             acc)
        g.states
        States.empty
    in
    (* *)
    return (states, map_of_states)
  ;;

  (** [build_edges g s] is a hashtable mapping FSM states to
      outgoing edges comprised of labels and destination states.
      @param g is the LTS with transitions to build the FSM edges from.
      @param s is the translation map from Coq-terms to FSM states.
      @return
        a tuple containing the {b edges} [States.t Actions.t Edges.t] and corresponding {b alphabet} of labels. *)
  let build_edges
        ?(show_debug : bool = false)
        (g : lts_graph)
        (s : state_translation_table)
    : (States.t Actions.t Edges.t * Alphabet.t) mm
    =
    let* env = get_env in
    let* sigma = get_sigma in
    let keys = H.to_seq_keys g.transitions in
    let (edges : States.t Actions.t Edges.t) =
      Seq.length keys |> Edges.create
    in
    (* get actions first *)
    let alphabet =
      H.fold
        (fun (from : EConstr.t)
          (transition : (action, EConstr.constr) transition)
          (acc : Alphabet.t) ->
           (* only add if action with same label doesnt exist *)
           if
             Alphabet.exists
               (fun (a : action) ->
                  String.equal a.label transition.action.label)
               acc
           then acc
           else
             Alphabet.add
               (make_action
                  ~label:transition.action.label
                  (Alphabet.cardinal acc))
               acc)
        g.transitions
        Alphabet.empty
    in
    (* build edges *)
    H.iter
      (fun (from : EConstr.t)
        (transition : (action, EConstr.constr) transition) ->
         let edge_from = Hashtbl.find s from in
         let edge_dest = Hashtbl.find s transition.destination in
         let edge_action =
           List.nth
             (let pstr_actions =
                Alphabet.filter
                  (fun (a : action) ->
                     String.equal a.label transition.action.label)
                  alphabet
              in
              Alphabet.to_list pstr_actions)
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

  (** [lts_to_fsm g init_term term] translates LTS [g] to an FSM.
      @param g is an [lts_graph] to be translated.
      @param init_term
        is the original actual Coq-term used to build the LTS.
        (Used to determine the initial state of the FSM).
      @return
        a tuple containing the ocaml-based [Fsm.fsm] and a table for translating ocaml-fsm states to coq-terms.*)
  let lts_to_fsm
        ?(show_debug : bool = false)
        (g : lts_graph)
        (init_term : Constrexpr.constr_expr_r CAst.t)
    : (fsm * state_translation_table) mm
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
        let* states, state_translation_map = build_states ~show_debug g in
        (* extract edges *)
        let* edges, alphabet =
          build_edges ~show_debug g state_translation_map
        in
        (* get initial state *)
        let$ t env sigma =
          Constrintern.interp_constr_evars env sigma init_term
        in
        let$ init_term env sigma = sigma, Reductionops.nf_all env sigma t in
        let init_state = Hashtbl.find state_translation_map init_term in
        (* return fsm and state_translation_map *)
        return (make_fsm init_state alphabet states edges, state_translation_map)
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

let build_lts
      ?(show_details : bool = true)
      ?(show_debug : bool = false)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : raw_lts mm
  =
  let* raw_lts = check_ref_lts iref in
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  let* graph_lts = G.build_graph ~show_debug raw_lts tref in
  (* *)
  let* env = get_env in
  let* sigma = get_sigma in
  (* *)
  if show_debug
  then (
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
            (G.pstr_lts_transitions ~long:() graph_lts.transitions))));
  return raw_lts
;;

(**  *)
let build_fsm_from_lts
      ?(show_details : bool = true)
      ?(show_debug : bool = false)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : fsm mm
  =
  let* raw_lts = check_ref_lts iref in
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  let* graph_lts = G.build_graph ~show_debug raw_lts tref in
  (* *)
  let* env = get_env in
  let* sigma = get_sigma in
  (* *)
  if show_debug
  then (
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
            (G.pstr_lts_transitions ~long:() graph_lts.transitions))));
  (* *)
  let* the_fsm, translation = G.lts_to_fsm ~show_debug graph_lts tref in
  if show_debug
  then (
    Feedback.msg_debug
      (str
         (Printf.sprintf
            "(f) Fsm: %s.\n"
            (pstr (pp_wrap_as_supported (Fsm the_fsm)))));
    Feedback.msg_debug
      (str
         (Printf.sprintf
            "(g, with details) Fsm: %s.\n"
            (pstr ~options:(Debug ()) (pp_wrap_as_supported (Fsm the_fsm))))));
  (* *)
  if show_details
  then
    Feedback.msg_info
      (str
         (Printf.sprintf
            "generated fsm: %s.\n"
            (pstr
               ~options:(pstr_options show_details)
               (pp_wrap_as_supported (Fsm the_fsm)))));
  (* *)
  if show_debug
  then Feedback.msg_debug (str "\n= = = = = (end of build_fsm) = = = = = =\n");
  return the_fsm
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
let cmd_bounded_lts
      ?(show_details : bool = true)
      ?(show_debug : bool = false)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  let* _ = build_lts ~show_details ~show_debug iref tref in
  return ()
;;

let cmd_bounded_lts_to_fsm
      ?(show_details : bool = true)
      ?(show_debug : bool = false)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  let* _ = build_fsm_from_lts ~show_details ~show_debug iref tref in
  return ()
;;

let cmd_merge_fsm_from_lts
      ?(show_details : bool = true)
      ?(show_debug : bool = false)
      ((s_iref, s_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
      ((t_iref, t_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  let* (s : fsm) =
    build_fsm_from_lts ~show_details:show_debug ~show_debug s_iref s_tref
  in
  let* (t : fsm) =
    build_fsm_from_lts ~show_details:show_debug ~show_debug t_iref t_tref
  in
  (* *)
  let merged_fsm, _ = merge_fsm s t in
  Feedback.msg_info
    (str
       (Printf.sprintf
          "merged fsm's 's' and 't' :: %s.\n\n\
           where s = %s,\n\
           and t = %s.\n\n\
           = = = (end of cmd_merge_fsm_from_lts) = = = = = =\n\n"
          (pstr
             ~options:(pstr_options show_details)
             (pp_wrap_as_supported (Fsm merged_fsm)))
          (pstr
             ~options:(pstr_options show_details)
             (pp_wrap_as_supported (Fsm s)))
          (pstr
             ~options:(pstr_options show_details)
             (pp_wrap_as_supported (Fsm t)))));
  return ()
;;

(* *)
let cmd_bisim_ks90_using_fsm
      ?(show_details : bool = true)
      ?(show_debug : bool = false)
      (s : fsm)
      (t : fsm)
  : unit mm
  =
  let open Bisimilarity in
  (* *)
  let result =
    RCP.KS90.run ~show:false ~details:show_details ~debug:show_debug s t
  in
  match result with
  | { are_bisimilar; bisimilar_states; non_bisimilar_states; _ } ->
    Feedback.msg_info
      (str
         (Printf.sprintf
            "[KS90] Results: (s ~ t) = %b.\n\n\
             Bisimilar states: %s.\n\n\
             Non-bisimilar states: %s.\n\n\
             where s = %s\n\n\
             and t = %s.\n\n\
             = = = (end of cmd_bisim_ks90_using_fsm) = = = = = =\n\n"
            are_bisimilar
            (pstr
               ~options:(pstr_options show_details)
               ~tabs:1
               (pp_wrap_as_supported (Partition bisimilar_states)))
            (pstr
               ~options:(pstr_options show_details)
               ~tabs:1
               (pp_wrap_as_supported (Partition non_bisimilar_states)))
            (pstr
               ~options:(pstr_options show_details)
               ~tabs:1
               (pp_wrap_as_supported (Fsm s)))
            (pstr
               ~options:(pstr_options show_details)
               ~tabs:1
               (pp_wrap_as_supported (Fsm t)))));
    return ()
;;

(* *)
let cmd_bisim_ks90_using_lts_to_fsm
      ?(show_details : bool = true)
      ?(show_debug : bool = false)
      ((s_iref, s_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
      ((t_iref, t_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  let* (s : fsm) =
    build_fsm_from_lts ~show_details:show_debug ~show_debug s_iref s_tref
  in
  let* (t : fsm) =
    build_fsm_from_lts ~show_details:show_debug ~show_debug t_iref t_tref
  in
  (* *)
  cmd_bisim_ks90_using_fsm ~show_details ~show_debug s t
;;
