open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax
open Pp_ext

(* *)
open Fsm
open Utils

let default_params : logging_params = default_logging_params ~mode:(Coq ()) ()

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
let m_unify
      ?(params : logging_params = default_params)
      (t0 : Evd.econstr)
      (t1 : Evd.econstr)
  : bool mm
  =
  params.kind <- Debug ();
  let* _ =
    if is_output_kind_enabled params
    then
      feedback ~params (fun (env : Environ.env) (sigma : Evd.evar_map) ->
        str "Unifying (t0) :: "
        ++ Printer.pr_econstr_env env sigma t0
        ++ strbrk "\nUnifying (t1) :: "
        ++ Printer.pr_econstr_env env sigma t1)
    else return ()
  in
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      log ~params "\t\tSuccess";
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify (m, n, e))
      ->
      log ~params "\t\tCould not unify";
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
          ?(params : logging_params = default_params)
          (i : (int tree * unif_problem) list)
  : int tree list option t
  =
  params.kind <- Debug ();
  match i with
  | [] -> return (Some [])
  | (ctor_tree, u) :: t ->
    let* _ =
      if is_output_kind_enabled params
      then
        feedback ~params (fun env sigma ->
          str "UNIFYALL (termL) :::::::::: "
          ++ Printer.pr_econstr_env env sigma u.termL
          ++ strbrk "\nUNIFYALL (termR) :::::::::: "
          ++ Printer.pr_econstr_env env sigma u.termR)
      else return ()
    in
    let* success = m_unify ~params u.termL u.termR in
    if success
    then
      let* unified = unify_all ~params t in
      match unified with
      | None -> return None
      | Some unified -> return (Some (List.append [ ctor_tree ] unified))
    else return None
;;

let sandboxed_unify
      ?(params : logging_params = default_params)
      (tgt_term : EConstr.t)
      (u : (int tree * unif_problem) list)
  : (EConstr.t * int tree list) option mm
  =
  params.kind <- Debug ();
  let* _ =
    if is_output_kind_enabled params
    then
      feedback ~params (fun env sigma ->
        str "TGT:::::: " ++ Printer.pr_econstr_env env sigma tgt_term)
    else return ()
  in
  sandbox
    (let* success = unify_all ~params u in
     match success with
     | None -> return None
     | Some unified ->
       let$+ term env sigma = Reductionops.nf_all env sigma tgt_term in
       let$+ is_undefined _ sigma = EConstr.isEvar sigma term in
       if is_undefined then return None else return (Some (term, unified)))
;;

(* [act] should probably come from the unification problems? *)
let rec retrieve_tgt_nodes
          ?(params : logging_params = default_params)
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
    let* success = sandboxed_unify ~params tgt_term u1 in
    (match success with
     | None -> retrieve_tgt_nodes ~params acc i act tgt_term nctors
     | Some (tgt, ctor_tree) ->
       let$+ act env sigma = Reductionops.nf_all env sigma act in
       retrieve_tgt_nodes
         ~params
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
         let* ctors = check_valid_constructor lts nextT (Some args.(1)) in
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
      ?(params : logging_params = default_params)
      (lts : raw_lts)
      (t : EConstr.t)
      (ma : EConstr.t option)
  : (EConstr.t * EConstr.t * int tree) list t
  =
  params.kind <- Debug ();
  (* : (EConstr.t * int tree list) list t *)
  let$+ t env sigma = Reductionops.nf_all env sigma t in
  (* let iter_body (i : int) (ctor_vals : (EConstr.t * int tree list) list) = *)
  let iter_body (i : int) (ctor_vals : (EConstr.t * EConstr.t * int tree) list) =
    let* _ =
      if is_output_kind_enabled params
      then
        feedback ~params (fun env sigma ->
          str "CHECKING CONSTRUCTOR "
          ++ int i
          ++ str ". Term: "
          ++ Printer.pr_econstr_env env sigma t)
      else return ()
    in
    let ctx, tm = lts.constructor_transitions.(i) in
    let ctx_tys = List.map EConstr.of_rel_decl ctx in
    let* substl = mk_ctx_substl [] (List.rev ctx_tys) in
    let termL, act, termR = extract_args substl tm in
    let* success = m_unify t termL in
    if success
    then
      let* success = Option.cata (fun a -> m_unify a act) (return true) ma in
      if success
      then
        let* (next_ctors : (int tree * unif_problem) list list option) =
          check_updated_ctx [ [] ] lts (substl, ctx_tys)
        in
        let$+ act env sigma = Reductionops.nf_all env sigma act in
        let (tgt_term : EConstr.t) = EConstr.Vars.substl substl termR in
        match next_ctors with
        | None -> return ctor_vals
        | Some [] ->
          let* sg = get_sigma in
          if EConstr.isEvar sg tgt_term
          then return ctor_vals
          else return ((act, tgt_term, Node (i, [])) :: ctor_vals)
        | Some nctors ->
          let tgt_nodes =
            retrieve_tgt_nodes ~params ctor_vals i act tgt_term nctors
          in
          tgt_nodes
      else return ctor_vals
    else return ctor_vals
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
    :  ?params:logging_params
    -> raw_lts
    -> Constrexpr.constr_expr_r CAst.t
    -> lts_graph mm

  val pstr_lts_transition
    :  ?params:logging_params
    -> EConstr.constr * lts_transition
    -> string

  val pstr_lts_transitions
    :  ?params:logging_params
    -> ?indent:int
    -> lts_transition H.t
    -> string

  val pstr_lts : ?params:logging_params -> ?long:unit -> lts_graph -> string
  (* val pp_fsm : ?long:unit -> fsm -> unit *)

  module DeCoq : sig
    type coq_translation =
      { from_coq : (EConstr.t, string) Hashtbl.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    (* type coq_translation = coq_translation_record mm *)

    val translate_coq_terms
      :  ?params:logging_params
      -> S.t
      -> coq_translation mm

    val translate_coq_lts
      :  ?params:logging_params
      -> lts_transition H.t
      -> coq_translation
      -> Lts.raw_flat_lts mm

    val lts_graph_to_lts
      :  ?params:logging_params
      -> lts_graph
      -> Constrexpr.constr_expr_r CAst.t
      -> (Lts.lts * coq_translation) mm
  end
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

  (** [build_lts_graph the_lts g] is an [lts_graph] [g] obtained by exploring [the_lts].
      @param the_lts describes the Coq-based term.
      @param g is an [lts_graph] accumulated while exploring [the_lts].
      @return an [lts_graph] constructed so long as the [bound] is not exceeded. *)
  let rec build_lts_graph
            ?(params : logging_params = default_params)
            (the_lts : raw_lts)
            (g : lts_graph)
    : lts_graph mm
    =
    params.kind <- Debug ();
    if H.length g.transitions >= bound
    then return g (* FIXME: raise error *)
    else if Queue.is_empty g.to_visit
    then return g
    else
      let* t = return (Queue.pop g.to_visit) in
      let* (constrs : (EConstr.t * EConstr.t * int tree) list) =
        check_valid_constructor ~params the_lts t None
      in
      let* env = get_env in
      let* sigma = get_sigma in
      let* _ =
        if is_output_kind_enabled params
        then
          feedback ~params (fun env sigma ->
            str
              (Printf.sprintf
                 "---- (returned from check_valid_constructor)\n\n\
                  build_lts_graph: constrs: [%s] (length %d).\n"
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
                 (List.length constrs)))
        else return ()
      in
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
           log
             ~params
             (Printf.sprintf "\n\nTransition to: %s." (econstr_to_string tgt));
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
           log
             ~params
             (Printf.sprintf "\nVisiting next: %i." (Queue.length g.to_visit)))
        constrs;
      let g = { g with states = S.union g.states !new_states } in
      build_lts_graph ~params the_lts g
  ;;

  (** [build_graph the_lts t] is ...
      @param the_lts is ...
      @param t is the original Coq-term. *)
  let build_graph
        ?(params : logging_params = default_params)
        (the_lts : raw_lts)
        (t : Constrexpr.constr_expr_r CAst.t)
    : lts_graph mm
    =
    let$ t env sigma = Constrintern.interp_constr_evars env sigma t in
    let$* u env sigma = Typing.check env sigma t the_lts.trm_type in
    let$ t env sigma = sigma, Reductionops.nf_all env sigma t in
    let q = Queue.create () in
    let* _ = return (Queue.push t q) in
    build_lts_graph
      ~params
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
        ?(params : logging_params = default_params)
        ((from : EConstr.constr), (t : lts_transition))
    : string
    =
    match t with
    | { action; index_tree; destination } ->
      Printf.sprintf
        "%s\n\t---{ %s\n\t}--> %s"
        (econstr_to_string from)
        (Printf.sprintf
           "\n\t\t(id: %d)\n\t\t(label: %s)\n\t\t(index_tree: %s)"
           action.id
           action.label
           (pstr_int_tree index_tree))
        (econstr_to_string destination)
  ;;

  (** [pstr_lts_transitions transitions] is a string of [transitions]. *)
  let pstr_lts_transitions
        ?(params : logging_params = default_params)
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
                (pstr_lts_transition ~params (from_node, outgoing_transition)))
           transitions
           "\n")
  ;;

  (** [pstr_lts_to_visit ?indent nodes_to_visit] is a string of [nodes_to_visit]. *)
  let pstr_lts_to_visit
        ?(params : logging_params = default_params)
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
  let pstr_lts
        ?(params : logging_params = default_params)
        ?(long : unit option)
        (g : lts_graph)
    : string
    =
    Printf.sprintf
      "%s\n%s"
      (Printf.sprintf "to_visit: %s" (pstr_lts_to_visit g.to_visit))
      (Printf.sprintf "transitions: %s" (pstr_lts_transitions g.transitions))
  ;;

  (*
     TODO: refactor the above, using the new [Fsm.Make] and [Fsm.New] functions
    - this will ensure that there are no duplicate states or actions and such
    - first, just translate this graph_lts to a [Lts.lts] (composed of just strings)
    - maybe [MkGraph] provides a record for converting to [Lts.lts].
      Then it is simple to use [Translate.to_fsm]
  *)
  module DeCoq = struct
    type coq_translation =
      { from_coq : (EConstr.t, string) Hashtbl.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    let make_coq_translation : coq_translation =
      { from_coq = Hashtbl.create 0; to_coq = Hashtbl.create 0 }
    ;;

    (* ! ! ! ! ! !
    "double wrapping" using the monad. => Is this a good idea?

    My idea is that it would be good if the coq_translation is
    all wrapped up in the environment/sigma that it was created
    from. Though, I am wondering if this would make it easier
    for us to translate back if needed.

    But... maybe this just isnt necessary since we access all
    of this from the [G] module anyway.
    *)

    (* type coq_translation = coq_translation_record mm *)

    let translate_coq_terms
          ?(params : logging_params = default_params)
          (states : N.t)
      : coq_translation mm
      =
      let tbl : coq_translation = make_coq_translation in
      S.iter
        (fun (s : EConstr.t) ->
           match Hashtbl.find_opt tbl.from_coq s with
           | None ->
             (* add as new state *)
             let str : string = econstr_to_string s in
             Hashtbl.add tbl.from_coq s str;
             Hashtbl.add tbl.to_coq str s
           | Some _ -> (* ignore *) ())
        states;
      return tbl
    ;;

    let translate_coq_lts
          ?(params : logging_params = default_params)
          (transitions : lts_transition H.t)
          (tbl : coq_translation)
      : Lts.raw_flat_lts mm
      =
      return
        (H.fold
           (fun (from : EConstr.t)
             (transition : (action, EConstr.constr) transition)
             (acc : Lts.raw_flat_lts) ->
              List.append
                acc
                [ ( Hashtbl.find tbl.from_coq from
                  , transition.action.label
                  , Hashtbl.find tbl.from_coq transition.destination )
                ])
           transitions
           [])
    ;;

    let translate_init_term
          (init_term : Constrexpr.constr_expr_r CAst.t)
          (tbl : coq_translation)
      : string mm
      =
      let$ t env sigma = Constrintern.interp_constr_evars env sigma init_term in
      let$ init_term env sigma = sigma, Reductionops.nf_all env sigma t in
      let init_str : string = Hashtbl.find tbl.from_coq init_term in
      return init_str
    ;;

    (** Error when trying to translate an unfinished LTS to FSM. *)
    exception UnfinishedLTS of lts_graph

    let lts_graph_to_lts
          ?(params : logging_params = default_params)
          (g : lts_graph)
          (init_term : Constrexpr.constr_expr_r CAst.t)
      : (Lts.lts * coq_translation) mm
      =
      params.kind <- Debug ();
      params.options.show_debug_output <- true;
      (* abort if lts not complete *)
      if Bool.not (Queue.is_empty g.to_visit)
      then (
        params.kind <- Warning ();
        log
          ~params
          (Printf.sprintf
             "lts is not complete, still had at least (%d) terms to visit."
             (Queue.length g.to_visit));
        raise (UnfinishedLTS g));
      (* *)
      let* tbl : coq_translation = translate_coq_terms g.states in
      let* init : string = translate_init_term init_term tbl in
      let* raw_lts : Lts.raw_flat_lts = translate_coq_lts g.transitions tbl in
      let lts : Lts.lts = Lts.Make.lts ~init (Flat raw_lts) in
      return (lts, tbl)
    ;;
  end
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  let* m = make_constr_tbl in
  let* s = make_constr_set in
  (* let* l = make_constr_set in *)
  let module G = MkGraph ((val m)) ((val s)) (* ((val l)) *) in
  return (module G : GraphB)
;;

let build_lts_graph
      ?(params : logging_params = default_params)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : raw_lts mm
  =
  params.kind <- Debug ();
  (* *)
  let* raw_lts = check_ref_lts iref in
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  let* graph_lts = G.build_graph ~params raw_lts tref in
  (* *)
  let* env = get_env in
  let* sigma = get_sigma in
  (* *)
  log
    ~params
    (Printf.sprintf
       "(a) Types of terms: %s.\n"
       (econstr_to_string raw_lts.trm_type));
  log
    ~params
    (Printf.sprintf
       "(b) Types of labels: %s.\n"
       (econstr_to_string raw_lts.lbl_type));
  log
    ~params
    (Printf.sprintf
       "(c) Constructors: %s.\n"
       (Pp.string_of_ppcmds
          (Pp.prvect_with_sep
             (fun _ -> str ", ")
             Names.Id.print
             raw_lts.coq_ctor_names)));
  log
    ~params
    (Printf.sprintf
       "(d) Transitions: %s.\n"
       (Pp.string_of_ppcmds
          (pp_transitions env sigma raw_lts.constructor_transitions)));
  log
    ~params
    (Printf.sprintf
       "(e) Graph Edges: %s.\n"
       (G.pstr_lts_transitions ~params graph_lts.transitions));
  return raw_lts
;;

(**  *)
let build_fsm_from_lts
      ?(params : logging_params = default_params)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : fsm mm
  =
  params.kind <- Debug ();
  (* *)
  let* raw_lts = check_ref_lts iref in
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  let* graph_lts = G.build_graph ~params raw_lts tref in
  (* *)
  let* env = get_env in
  let* sigma = get_sigma in
  (* *)
  log
    ~params
    (Printf.sprintf
       "(a) Types of terms: %s.\n"
       (econstr_to_string raw_lts.trm_type));
  log
    ~params
    (Printf.sprintf
       "(b) Types of labels: %s.\n"
       (econstr_to_string raw_lts.lbl_type));
  log
    ~params
    (Printf.sprintf
       "(c) Constructors: %s.\n"
       (Pp.string_of_ppcmds
          (Pp.prvect_with_sep
             (fun _ -> str ", ")
             Names.Id.print
             raw_lts.coq_ctor_names)));
  log
    ~params
    (Printf.sprintf
       "(d) Transitions: %s.\n"
       (Pp.string_of_ppcmds
          (pp_transitions env sigma raw_lts.constructor_transitions)));
  log
    ~params
    (Printf.sprintf
       "(e) Graph Edges: %s.\n"
       (G.pstr_lts_transitions ~params graph_lts.transitions));
  (* *)
  let* the_pure_lts, coq_translation =
    G.DeCoq.lts_graph_to_lts ~params graph_lts tref
  in
  let the_fsm = Translate.to_fsm the_pure_lts in
  log
    ~params
    (Printf.sprintf
       "(f) Fsm: %s.\n"
       (PStr.fsm ~params:(Logging params) the_fsm));
  params.kind <- Debug ();
  log ~params "\n= = = = = (end of build_fsm) = = = = = =\n";
  (* *)
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
      ?(params : logging_params = default_params)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  params.kind <- Debug ();
  let* _ = build_lts_graph ~params iref tref in
  return ()
;;

let cmd_bounded_lts_to_fsm
      ?(params : logging_params = default_params)
      (iref : Names.GlobRef.t)
      (tref : Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  params.kind <- Debug ();
  let* _ = build_fsm_from_lts ~params iref tref in
  return ()
;;

let cmd_merge_fsm_from_lts
      ?(params : logging_params = default_params)
      ((s_iref, s_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
      ((t_iref, t_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  params.kind <- Debug ();
  let* (s : fsm) = build_fsm_from_lts ~params s_iref s_tref in
  let* (t : fsm) = build_fsm_from_lts ~params t_iref t_tref in
  (* *)
  let merged_fsm, _ = Merge.fsms ~params s t in
  params.kind <- Details ();
  log
    ~params
    (Printf.sprintf
       "Merged FSMs 's' and 't' :: %s.\n\n\
        where s = %s,\n\n\
        and t = %s.\n\n\
        = = = (end of cmd_merge_fsm_from_lts) = = = = = =\n\n"
       (PStr.fsm ~params:(Logging params) merged_fsm)
       (PStr.fsm ~params:(Logging params) s)
       (PStr.fsm ~params:(Logging params) t));
  return ()
;;

(* *)
let cmd_bisim_ks90_using_fsm
      ?(params : logging_params = default_params)
      (s : fsm)
      (t : fsm)
  : unit mm
  =
  let open Bisimilarity in
  (* *)
  params.kind <- Debug ();
  let result = RCP.KS90.run ~params s t in
  match result with
  | { are_bisimilar; merged_fsm; bisimilar_states; non_bisimilar_states; _ } ->
    params.kind <- Details ();
    params.options.show_normal_output <- true;
    log
      ~params:
        (params.kind <- Normal ();
         params)
      (Printf.sprintf
         "[KS90] Results: (s ~ t) = %b.\n\n\
          Bisimilar states: %s.\n\n\
          Non-bisimilar states: %s.\n\n\
          where s = %s\n\n\
          and t = %s\n\n\
          were merged into: %s.\n\n\
          = = = (end of cmd_bisim_ks90_using_fsm) = = = = = =\n\n"
         are_bisimilar
         (PStr.partition ~params:(Logging params) bisimilar_states)
         (PStr.partition ~params:(Logging params) non_bisimilar_states)
         (PStr.fsm ~params:(Logging params) s)
         (PStr.fsm ~params:(Logging params) t)
         (PStr.fsm ~params:(Logging params) merged_fsm));
    return ()
;;

(* *)
let cmd_bisim_ks90_using_lts_to_fsm
      ?(params : logging_params = default_params)
      ((s_iref, s_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
      ((t_iref, t_tref) : Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t)
  : unit mm
  =
  params.kind <- Debug ();
  let* (s : fsm) = build_fsm_from_lts ~params s_iref s_tref in
  let* (t : fsm) = build_fsm_from_lts ~params t_iref t_tref in
  (* *)
  cmd_bisim_ks90_using_fsm ~params s t
;;
