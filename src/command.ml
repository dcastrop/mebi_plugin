open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax

(* *)
open Fsm
open Utils.Logging
open Utils.Formatting
open Utils

let default_params : Params.log = Params.Default.log ~mode:(Coq ()) ()

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

type term_type_map =
  (* (EConstr.types, (Constr.rel_context * Constr.types) array) Hashtbl.t *)
  (EConstr.types, raw_lts) Hashtbl.t

(** essentially a list of [raw_lts], to allow the plugin to be provided (manually) the relevant inductive defeinitions. Useful in the case of a layered LTS. *)
(* type rlts_list = raw_lts list *)

(** [log_raw_lts] *)
let log_raw_lts ?(params : Params.log = default_params) (rlts : raw_lts)
  : unit mm
  =
  log
    ~params
    (Printf.sprintf
       "(a) Types of terms: %s.\n"
       (econstr_to_string rlts.trm_type));
  log
    ~params
    (Printf.sprintf
       "(b) Types of labels: %s.\n"
       (econstr_to_string rlts.lbl_type));
  log
    ~params
    (Printf.sprintf
       "(c) Constructors: %s.\n"
       (Pp.string_of_ppcmds
          (Pp.prvect_with_sep
             (fun _ -> str ", ")
             Names.Id.print
             rlts.coq_ctor_names)));
  log
    ~params
    (Printf.sprintf
       "(d) Types of coq_lts: %s.\n"
       (econstr_to_string rlts.coq_lts));
  (* log
     ~params
     (Printf.sprintf
     "(f) Transitions (do not be alarmed by any _UNBOUND_ below): %s.\n"
     (if Array.length rlts.constructor_transitions < 1
     then "[] (empty)"
     else
     Printf.sprintf
     "[%s]"
     (Array.fold_left
     (fun (acc : string) (tr : Constr.rel_context * Constr.t) ->
     Printf.sprintf "%s   %s\n" acc (constr_to_string (snd tr)))
     "\n"
     rlts.constructor_transitions))); *)
  return ()
;;

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
      ?(params : Params.log = default_params)
      (t0 : EConstr.t)
      (t1 : EConstr.t)
  : bool mm
  =
  params.kind <- Debug ();
  (* let* _ =
     if is_output_kind_enabled params
     then
     debug (fun (env : Environ.env) (sigma : Evd.evar_map) ->
     str "Unifying (t0) :: "
     ++ Printer.pr_econstr_env env sigma t0
     ++ strbrk "\nUnifying (t1) :: "
     ++ Printer.pr_econstr_env env sigma t1)
     else return ()
     in *)
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      (* log ~params "\t\tSuccess"; *)
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify (m, n, e))
      ->
      log ~params "\t\tCould not unify";
      sigma, false)
;;

(** [mk_ctx_substl] *)
let rec mk_ctx_substl (substl : EConstr.t list)
  : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list -> EConstr.t list mm
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

let _pstr_unif_problem (t : unif_problem) : string =
  match t with
  | { termL; termR; _ } ->
    Printf.sprintf
      "{ L: %s;\n  R: %s; }"
      (econstr_to_string termL)
      (econstr_to_string termR)
;;

type 'a tree = Node of 'a * 'a tree list

let rec tree_eq (t1 : int tree) (t2 : int tree) : bool =
  match t1, t2 with
  | Node (a1, b1), Node (a2, b2) -> a1 == a2 && tree_list_eq b1 b2

and tree_list_eq (l1 : int tree list) (l2 : int tree list) : bool =
  match l1, l2 with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 -> tree_eq h1 h2 && tree_list_eq t1 t2
  | _, _ -> false
;;

let rec _tree_contains (i : int) (t : int tree) : bool =
  match t with
  | Node (j, l) ->
    if i == j
    then true
    else (
      match l with
      | [] -> false
      | l -> List.for_all (fun (s : int tree) -> _tree_contains i s) l)
;;

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
          ?(params : Params.log = default_params)
          (i : (int tree * unif_problem) list)
  : int tree list option t
  =
  params.kind <- Debug ();
  match i with
  | [] -> return (Some [])
  | (ctor_tree, u) :: t ->
    (* let* _ =
       if is_output_kind_enabled params
       then
       debug (fun env sigma ->
       str "UNIFYALL (termL) :::::::::: "
       ++ Printer.pr_econstr_env env sigma u.termL
       ++ strbrk "\nUNIFYALL (termR) :::::::::: "
       ++ Printer.pr_econstr_env env sigma u.termR)
       else return ()
       in *)
    let* success = m_unify ~params u.termL u.termR in
    if success
    then
      let* unified = unify_all ~params t in
      match unified with
      | None -> return None
      | Some unified -> return (Some (ctor_tree :: unified))
    else return None
;;

let sandboxed_unify
      ?(params : Params.log = default_params)
      (tgt_term : EConstr.t)
      (u : (int tree * unif_problem) list)
  : (EConstr.t * int tree list) option mm
  =
  (* let* _ =
     if is_output_kind_enabled params
     then
     debug (fun env sigma ->
     str "TGT:::::: " ++ Printer.pr_econstr_env env sigma tgt_term)
     else return ()
     in *)
  sandbox
    (let* success = unify_all ~params u in
     match success with
     | None -> return None
     | Some unified ->
       let$+ term env sigma = Reductionops.nf_all env sigma tgt_term in
       let$+ is_undefined _ sigma = EConstr.isEvar sigma term in
       if is_undefined then return None else return (Some (term, unified)))
;;

(** [coq_ctor]
    - [EConstr.t] action
    - [EConstr.t] destination
    - [int tree] coq-constructor index *)
type coq_ctor = EConstr.t * EConstr.t * int tree

(* [act] should probably come from the unification problems? *)
let rec retrieve_tgt_nodes
          ?(params : Params.log = default_params)
          (acc : coq_ctor list)
          (i : int)
          (act : EConstr.t)
          (tgt_term : EConstr.t)
  : (int tree * unif_problem) list list -> coq_ctor list t
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
          ?(params : Params.log = default_params)
          (acc : (int tree * unif_problem) list list)
          (fn_rlts : term_type_map)
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
       (match Hashtbl.find_opt fn_rlts fn with
        | None -> check_updated_ctx acc fn_rlts (substl, tl)
        | Some rlts ->
          let$+ nextT env sigma = Reductionops.nf_evar sigma args.(0) in
          let* ctors =
            check_valid_constructor
              ~params
              rlts.constructor_transitions
              fn_rlts
              nextT
              (Some args.(1))
          in
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
            (* replace [rtls] in [rtls_ctx] *)
            (* let rtls_ctx':rlts_list =
               List.append [  ]
               in *)
            check_updated_ctx
              (List.concat_map (fun x -> List.map (fun y -> y :: x) ctors) acc)
              fn_rlts
              (substl, tl)))
     | _ -> check_updated_ctx acc fn_rlts (substl, tl))
  | _, _ -> assert false
(* Impossible! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

(** Checks possible transitions for this term: *)
and check_valid_constructor
      ?(params : Params.log = default_params)
      (ctor_transitions : (Constr.rel_context * Constr.types) array)
      (fn_rlts : term_type_map)
      (t' : EConstr.t)
      (ma : EConstr.t option)
  : coq_ctor list t
  =
  params.kind <- Debug ();
  let$+ t env sigma = Reductionops.nf_all env sigma t' in
  let iter_body (i : int) (ctor_vals : coq_ctor list) =
    (* let* _ =
       if is_output_kind_enabled params
       then
       debug (fun env sigma ->
       str "CHECKING CONSTRUCTOR "
       ++ int i
       ++ str ". Term: "
       ++ Printer.pr_econstr_env env sigma t)
       else return ()
       in *)
    let ctx, tm = ctor_transitions.(i) in
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
          check_updated_ctx ~params [ [] ] fn_rlts (substl, ctx_tys)
        in
        let$+ act env sigma = Reductionops.nf_all env sigma act in
        let tgt_term : EConstr.t = EConstr.Vars.substl substl termR in
        match next_ctors with
        | None -> return ctor_vals
        | Some [] ->
          let* sigma = get_sigma in
          if EConstr.isEvar sigma tgt_term
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
  iterate 0 (Array.length ctor_transitions - 1) [] iter_body
;;

(** [default_bound] is the total depth that will be explored of a given lts by [explore_lts]. *)
let default_bound : int = 10
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

  val build_lts_graph
    :  ?params:Params.log
    -> term_type_map
    -> lts_graph
    -> int
    -> lts_graph mm

  val build_graph
    :  ?params:Params.log
    -> term_type_map
    -> term_type_map
    -> Constrexpr.constr_expr
    -> int
    -> lts_graph mm

  module DeCoq : sig
    type coq_translation =
      { from_coq : (EConstr.t, string) Hashtbl.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    (* type coq_translation = coq_translation_record mm *)

    val translate_coq_terms : ?params:Params.log -> S.t -> coq_translation mm

    val translate_coq_lts
      :  ?params:Params.log
      -> lts_transition H.t
      -> coq_translation
      -> Lts.raw_flat_lts mm

    val lts_graph_to_lts
      :  ?params:Params.log
      -> ?bound:int
      -> ?name:string
      -> lts_graph
      -> Constrexpr.constr_expr
      -> (Lts.lts * coq_translation) mm
  end

  module PStr : sig
    val econstr : EConstr.t -> string mm
    val constructor : ?params:Params.pstr -> lts_transition -> string mm

    val transition
      :  ?params:Params.pstr
      -> EConstr.constr * lts_transition
      -> string mm

    val transitions : ?params:Params.pstr -> lts_transition H.t -> string mm
    val states : ?params:Params.pstr -> S.t -> string mm
    val queue : ?params:Params.pstr -> EConstr.t Queue.t -> string mm
    val lts : ?params:Params.pstr -> lts_graph -> string mm
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

  module PStr = struct
    let econstr (t : EConstr.t) : string mm =
      let* env = get_env in
      let* sigma = get_sigma in
      return (Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma t))
    ;;

    (* TODO: refactor pstr_lts and others *)

    let constructor
          ?(params : Params.pstr = Fmt (Params.Default.fmt ~mode:(Coq ()) ()))
          (t : lts_transition)
      : string mm
      =
      let _params : Params.fmt = Params.handle params in
      let normal_pstr : string = Printf.sprintf "(%s)" t.action.label
      and detail_pstr : string =
        Printf.sprintf "(%s | id:%d)" t.action.label t.action.id
      in
      match _params.params.kind with
      | Normal () -> return normal_pstr
      | Details () -> return detail_pstr
      | Debug () -> return detail_pstr
      | Warning () -> return detail_pstr
    ;;

    let transition
          ?(params : Params.pstr = Fmt (Params.Default.fmt ~mode:(Coq ()) ()))
          ((from, transition) : EConstr.constr * lts_transition)
      : string mm
      =
      let _params : Params.fmt = Params.handle params in
      let normal_pstr : string =
        Printf.sprintf
          "{ %s --<%s>--> %s }"
          (run (econstr from))
          transition.action.label
          (run (econstr transition.destination))
      and detail_pstr : string =
        Printf.sprintf
          "{ %s :: %s --<%s | id:%d>--> %s }"
          (pstr_int_tree transition.index_tree)
          (run (econstr from))
          transition.action.label
          transition.action.id
          (run (econstr transition.destination))
      in
      match _params.params.kind with
      | Normal () -> return normal_pstr
      | Details () -> return detail_pstr
      | Debug () -> return detail_pstr
      | Warning () -> return detail_pstr
    ;;

    let transitions
          ?(params : Params.pstr = Fmt (Params.Default.fmt ~mode:(Coq ()) ()))
          (transitions : lts_transition H.t)
      : string mm
      =
      let _params : Params.fmt = Params.handle params in
      let tabs : string = str_tabs _params.tabs
      and tabs' : string = str_tabs (_params.tabs + 1) in
      if H.length transitions < 1
      then return "{ } (empty)"
      else (
        let pstr : string =
          H.fold
            (fun (from : EConstr.t)
              (transition' : lts_transition)
              (acc : string) ->
               Printf.sprintf
                 "%s%s%s\n"
                 acc
                 tabs'
                 (run (transition (from, transition'))))
            transitions
            "\n"
        in
        return (Printf.sprintf "{%s%s}" pstr tabs))
    ;;

    let states
          ?(params : Params.pstr = Fmt (Params.Default.fmt ~mode:(Coq ()) ()))
          (states : N.t)
      : string mm
      =
      let _params : Params.fmt = Params.handle params in
      let tabs : string = str_tabs _params.tabs
      and tabs' : string = str_tabs (_params.tabs + 1) in
      if N.is_empty states
      then return "[ ] (empty)"
      else (
        let pstr : string =
          N.fold
            (fun (s : EConstr.t) (acc : string) ->
               Printf.sprintf "%s%s%s\n" acc tabs' (run (econstr s)))
            states
            "\n"
        in
        return (Printf.sprintf "[%s%s]" pstr tabs))
    ;;

    let queue
          ?(params : Params.pstr = Fmt (Params.Default.fmt ~mode:(Coq ()) ()))
          (q : EConstr.t Queue.t)
      : string mm
      =
      let _params : Params.fmt = Params.handle params in
      let tabs : string = str_tabs _params.tabs
      and tabs' : string = str_tabs (_params.tabs + 1) in
      if Queue.is_empty q
      then return "[ ] (empty)"
      else (
        let pstr : string =
          Queue.fold
            (fun (acc : string) (to_visit : EConstr.t) ->
               Printf.sprintf "%s%s%s\n" acc tabs' (run (econstr to_visit)))
            "\n"
            q
        in
        return (Printf.sprintf "[%s%s]" pstr tabs))
    ;;

    let lts
          ?(params : Params.pstr = Fmt (Params.Default.fmt ~mode:(Coq ()) ()))
          (g : lts_graph)
      : string mm
      =
      let _params : Params.fmt = Params.handle params in
      let _params' : Params.fmt = inc_tab ~by:2 _params
      and tabs : string = str_tabs _params.tabs
      and tabs' : string = str_tabs (_params.tabs + 1) in
      let pstr_queue : string =
        Printf.sprintf
          "\n%sterms to visit: %s"
          tabs'
          (run (queue ~params:(Fmt _params') g.to_visit))
      and pstr_states : string =
        Printf.sprintf
          "\n%sstates: %s"
          tabs'
          (run (states ~params:(Fmt _params') g.states))
      and pstr_transitions : string =
        Printf.sprintf
          "\n%stransitions: %s"
          tabs'
          (run (transitions ~params:(Fmt _params') g.transitions))
      in
      return
        (Printf.sprintf
           "{ %s; %s; %s \n%s}"
           pstr_queue
           pstr_states
           pstr_transitions
           tabs)
    ;;
  end

  (** handles adding new transition to [lts_graph] *)
  (* let _handle_new_lts_graph_transition
    (g : lts_graph ref)
    (t : EConstr.t)
    (to_add : action)
    (int_tree : int tree)
    (tgt : EConstr.t)
    : lts_graph mm
    =
    let* sigma = get_sigma in
    (match H.find_opt !g.transitions t with
     | None ->
       H.add
         !g.transitions
         t
         { action = to_add; index_tree = int_tree; destination = tgt }
     | Some lt ->
       (match lt with
        | { action; destination; index_tree } ->
          if EConstr.eq_constr sigma tgt destination
          then
            H.replace
              !g.transitions
              t
              { action
              ; index_tree = merge_tree index_tree int_tree
              ; destination
              }
          else
            H.add
              !g.transitions
              t
              { action = to_add; index_tree = int_tree; destination = tgt }));
    return !g
  ;; *)

  let _debug_output_constrs (t : EConstr.t) (constrs : coq_ctor list) : unit mm =
    debug (fun env sigma ->
      str
        (Printf.sprintf
           "---- (returned from check_valid_constructor)\n\
            build_lts_graph, t: %s\n\
            constrs of t: [%s\n\
            ] (length %d).\n"
           (econstr_to_string t)
           (List.fold_left
              (fun (acc : string) ((act, ctor, int_tree) : coq_ctor) ->
                 Printf.sprintf
                   "%s\n- ctor_index_tree: %s\n  act:%s\n  ctors: %s\n"
                   acc
                   (pstr_int_tree int_tree)
                   (econstr_to_string act)
                   (econstr_to_string ctor))
              ""
              constrs)
           (List.length constrs)))
  ;;

  (** returns [None] if [ctor] is not in [ctors], by matching only on the first two fields (i.e, [act] and [tgt] -- ignoring [int tree]). *)
  let find_opt_ctor (ctor : coq_ctor) (ctors : coq_ctor list)
    : coq_ctor option mm
    =
    let* sigma = get_sigma in
    match ctor with
    | act, tgt, int_tree ->
      (match
         List.find_opt
           (fun (ctor' : coq_ctor) ->
              match ctor' with
              | act', tgt', int_tree' ->
                EConstr.eq_constr sigma act act'
                && EConstr.eq_constr sigma tgt tgt')
           ctors
       with
       | None -> return None
       | Some ctor -> return (Some ctor))
  ;;

  (** merges [ctors] into [acc] by:
      - if already in [acc], then skip.
      - if another in [acc] has matching [act] and [tgt] fields, then merge [int tree].
      - else just add to [acc]. *)
  let merge_constrs_tree_lists
        ?(params : Params.log = default_params)
        (t : EConstr.t)
        (acc : coq_ctor list)
        (ctors : coq_ctor list)
    : coq_ctor list
    =
    List.fold_left
      (fun (acc' : coq_ctor list) (ctor : coq_ctor) ->
         (* skip if already in [acc'] *)
         if List.mem ctor acc'
         then acc'
         else (
           let (opt_ctor : coq_ctor option) = run (find_opt_ctor ctor acc') in
           match opt_ctor with
           (* merge [int tree] if already exists *)
           | Some ctor' ->
             (match ctor, ctor' with
              | (act, tgt, int_tree), (_act', _tgt', int_tree') ->
                if Bool.not (tree_eq int_tree int_tree')
                then (
                  Log.warning
                    ~params
                    (Printf.sprintf
                       "merge_constrs_tree_lists, different int tree found for \
                        t: %s\n\
                        existing: %s\n\
                        new: %s.\n\
                        kept the original (i.e., %s)"
                       (econstr_to_string t)
                       (pstr_int_tree int_tree)
                       (pstr_int_tree int_tree')
                       (pstr_int_tree int_tree));
                  params.kind <- Normal ());
                (act, tgt, int_tree) :: acc')
             (* add if new *)
           | None -> ctor :: acc'))
      acc
      ctors
  ;;

  (** *)
  let build_constrs_tree_list
        ?(params : Params.log = default_params)
        (t : EConstr.t)
        (fn_rlts : term_type_map)
    : coq_ctor list mm
    =
    Hashtbl.fold
      (fun (_k : EConstr.t) (v : raw_lts) (acc : coq_ctor list mm) ->
         let* (ctors : coq_ctor list) =
           check_valid_constructor
             ~params
             v.constructor_transitions
             fn_rlts
             t
             None
         in
         if List.is_empty ctors
         then acc
         else return (merge_constrs_tree_lists t (run acc) ctors))
      fn_rlts
      (return [])
  ;;

  let get_new_states
        ?(params : Params.log = default_params)
        (t : EConstr.t)
        (g : lts_graph)
        (ctors : coq_ctor list)
    : S.t mm
    =
    let get_transition_id : unit -> int = new_int_counter () in
    let iter_body (i : int) (new_states : S.t) =
      let (act, tgt, int_tree) : coq_ctor = List.nth ctors i in
      let new_states : S.t = S.add tgt new_states in
      let* sigma = get_sigma in
      (* TODO: detect tau transitions and then defer to [Fsm.tau] instead. *)
      let to_add : action =
        Fsm.Create.action
          ~is_tau:false
          ~annotation:[]
          (Of (get_transition_id (), econstr_to_string act))
      in
      (match H.find_opt g.transitions t with
       | None ->
         H.add
           g.transitions
           t
           { action = to_add; index_tree = int_tree; destination = tgt };
         (* if [tgt] is new state, add to [new_states] *)
         if H.mem g.transitions tgt || EConstr.eq_constr sigma tgt t
         then ()
         else Queue.push tgt g.to_visit
       | Some lt ->
         (* () *)
         (match lt with
          | { action; destination; index_tree } ->
            if Bool.not (tree_eq int_tree index_tree)
            then (
              (* if [tgt] is new state, add to [new_states] *)
              if H.mem g.transitions tgt || EConstr.eq_constr sigma tgt t
              then ()
              else Queue.push tgt g.to_visit;
              H.add
                g.transitions
                t
                { action = to_add; index_tree = int_tree; destination = tgt })));
      return new_states
    in
    (* ) *)
    (* ctors; *)
    (* return !new_states *)
    (* let new_states : S.t ref = ref (S.singleton t) in *)
    iterate 0 (List.length ctors - 1) (S.singleton t) iter_body
  ;;

  let _get_new_states
        ?(params : Params.log = default_params)
        (t : EConstr.t)
        (g : lts_graph)
        (ctors : coq_ctor list)
    : S.t mm
    =
    let new_states : S.t ref = ref (S.singleton t) in
    let get_transition_id : unit -> int = new_int_counter () in
    let* sigma = get_sigma in
    List.iter
      (fun ((act, tgt, int_tree) : coq_ctor) ->
         (* let old_states = !new_states in *)
         new_states := S.add tgt !new_states;
         (* log
           ~params
           (Printf.sprintf
           "get_new_states, t: %s\nold new states: %s\nnew new states: %s"
           (econstr_to_string t)
           (run (PStr.states ~params:(Log params) old_states))
           (run (PStr.states ~params:(Log params) old_states))); *)
         (* TODO: detect tau transitions and then defer to [Fsm.tau] instead. *)
         let to_add : action =
           Fsm.Create.action
             ~is_tau:false
             ~annotation:[]
             (Of (get_transition_id (), econstr_to_string act))
         in
         match H.find_opt g.transitions t with
         | None ->
           H.add
             g.transitions
             t
             { action = to_add; index_tree = int_tree; destination = tgt };
           (* if [tgt] is new state, add to [new_states] *)
           if H.mem g.transitions tgt || EConstr.eq_constr sigma tgt t
           then ()
           else Queue.push tgt g.to_visit
         | Some lt ->
           (* () *)
           (match lt with
            | { action; destination; index_tree } ->
              if Bool.not (tree_eq int_tree index_tree)
              then (
                (* if [tgt] is new state, add to [new_states] *)
                if H.mem g.transitions tgt || EConstr.eq_constr sigma tgt t
                then ()
                else Queue.push tgt g.to_visit;
                H.add
                  g.transitions
                  t
                  { action = to_add; index_tree = int_tree; destination = tgt }))
         (*
            if Bool.not (EConstr.eq_constr sigma tgt destination)
              then
                H.add
                  g.transitions
                  t
                  { action = to_add; index_tree = int_tree; destination = tgt }) *))
      ctors;
    return !new_states
  ;;

  (** [build_lts_graph fn_rlts g bound] is an [lts_graph] [g] obtained by exploring [fn_rlts].
      @param fn_rlts maps coq-term names to [raw_lts].
      @param g is an [lts_graph] accumulated while exploring [rlts].
      @param bound is the number of states to explore until.
      @return an [lts_graph] with a maximum of [bound] many states. *)
  let rec build_lts_graph
            ?(params : Params.log = default_params)
            (fn_rlts : term_type_map)
            (g : lts_graph)
            (bound : int)
    : lts_graph mm
    =
    params.kind <- Debug ();
    if Queue.is_empty g.to_visit
    then return g (* finished if no more to visit*)
    else if S.cardinal g.states > bound
    then return g (* exit if bound reached *)
    else
      let* (t : EConstr.t) = return (Queue.pop g.to_visit) in
      let* (constrs : coq_ctor list) =
        build_constrs_tree_list ~params t fn_rlts
      in
      (* let* _ =
         if is_output_kind_enabled params
         then debug_output_constrs t constrs
         else return ()
         in *)
      let* (new_states : S.t) = get_new_states ~params t g constrs in
      let g = { g with states = S.union g.states new_states } in
      build_lts_graph ~params fn_rlts g bound
  ;;

  exception
    CoqTermDoesNotMatchSemantics of
      (Constrexpr.constr_expr * EConstr.t * term_type_map * term_type_map)

  (** [build_graph tr_rlts fn_rlts tref bound] is the entry point for [build_lts_graph].
      @param tr_rlts maps coq-term types to [raw_lts].
      @param fn_rlts
        is passed to [build_lts_graph] and maps coq-term names to [raw_lts].
      @param tref is the original coq-term.
      @param bound is the number of states to explore until. *)
  let build_graph
        ?(params : Params.log = default_params)
        (tr_rlts : term_type_map)
        (fn_rlts : term_type_map)
        (tref : Constrexpr.constr_expr)
        (bound : int)
    : lts_graph mm
    =
    let$ t env sigma = Constrintern.interp_constr_evars env sigma tref in
    match Hashtbl.find_opt tr_rlts (run (Mebi_utils.type_of_tref tref)) with
    | None ->
      Log.warning
        ~params
        (Printf.sprintf
           "build_graph, ERROR: could not find rlts matching:\n\
            \tt of tref: %s\n\
            where tr_lts (keys): %s\n\
            and fn_rlts (keys): %s."
           (econstr_to_string t)
           (Utils.pstr_keys (Utils.OfEConstr (Hashtbl.to_seq_keys tr_rlts)))
           (Utils.pstr_keys (Utils.OfEConstr (Hashtbl.to_seq_keys fn_rlts))));
      raise (CoqTermDoesNotMatchSemantics (tref, t, tr_rlts, fn_rlts))
    | Some rlts ->
      (* update environment by typechecking *)
      let$* u env sigma = Typing.check env sigma t rlts.trm_type in
      let$ t env sigma = sigma, Reductionops.nf_all env sigma t in
      let q = Queue.create () in
      let* _ = return (Queue.push t q) in
      build_lts_graph
        ~params
        fn_rlts
        { to_visit = q; states = S.empty; transitions = H.create bound }
        bound
  ;;

  module DeCoq = struct
    type coq_translation =
      { from_coq : (EConstr.t, string) Hashtbl.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    let make_coq_translation : coq_translation =
      { from_coq = Hashtbl.create 0; to_coq = Hashtbl.create 0 }
    ;;

    (* type coq_translation = coq_translation_record mm *)

    let translate_coq_terms
          ?(params : Params.log = default_params)
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
          ?(params : Params.log = default_params)
          (transitions : lts_transition H.t)
          (tbl : coq_translation)
      : Lts.raw_flat_lts mm
      =
      return
        (H.fold
           (fun (from : EConstr.t)
             (transition : (action, EConstr.constr) transition)
             (acc : Lts.raw_flat_lts) ->
              ( Hashtbl.find tbl.from_coq from
              , transition.action.label
              , Hashtbl.find tbl.from_coq transition.destination
              , Some (pstr_int_tree transition.index_tree) )
              :: acc)
           transitions
           [])
    ;;

    let translate_init_term
          (init_term : Constrexpr.constr_expr)
          (tbl : coq_translation)
      : string mm
      =
      let$ t env sigma = Constrintern.interp_constr_evars env sigma init_term in
      let$ init_term env sigma = sigma, Reductionops.nf_all env sigma t in
      let init_str : string = Hashtbl.find tbl.from_coq init_term in
      return init_str
    ;;

    (** Error when trying to translate an unfinished LTS to FSM. *)
    (* exception UnfinishedLTS of lts_graph *)

    let lts_graph_to_lts
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "unknown")
          (g : lts_graph)
          (init_term : Constrexpr.constr_expr)
      : (Lts.lts * coq_translation) mm
      =
      params.kind <- Debug ();
      if List.mem name [ "e3"; "e4" ]
      then params.override <- Some ()
      else params.override <- None;
      (* abort if lts not complete *)
      (* if Bool.not (Queue.is_empty g.to_visit)
         then (
         params.kind <- Warning ();
         log
         ~params
         (Printf.sprintf
         "lts is not complete using bound (%d), still had at least (%d) \
         terms to visit."
         bound
         (Queue.length g.to_visit));
         (* dump lts to *)
         raise (UnfinishedLTS g)); *)
      let* (tbl : coq_translation) = translate_coq_terms g.states in
      let* (init : string) = translate_init_term init_term tbl in
      let* (flat_rlts : Lts.raw_flat_lts) =
        translate_coq_lts g.transitions tbl
      in
      let is_complete : bool = Queue.is_empty g.to_visit in
      let num_states : int = S.cardinal g.states in
      let num_edges : int = H.length g.transitions in
      let info : Utils.model_info =
        { is_complete; bound; num_states; num_edges }
      in
      let lts : Lts.lts = Lts.Create.lts ~init ~info (Flat flat_rlts) in
      if Bool.not is_complete
      then (
        let to_visit : int = Queue.length g.to_visit in
        params.kind <- Warning ();
        Log.warning
          ~params
          (Printf.sprintf
             "LTS (%s) is not complete using bound (%d), still had at least \
              (%d) terms to visit."
             name
             bound
             to_visit);
        (* dump lts to *)
        let dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (LTS (Printf.sprintf "%s (%d rem %d)" name bound to_visit))
            (JSON ())
            (LTS lts)
        in
        Log.warning
          ~params
          (Printf.sprintf "saved incomplete LTS to: %s\n" dump_filepath)
        (* raise (UnfinishedLTS g) *));
      return (lts, tbl)
    ;;
  end

  (* TODO: from [LTS.lts] and [coq_translation] create a coq term and save to file *)
  (* module ReCoq = struct
     (* let from_lts *)
     end *)
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  let* m = make_constr_tbl in
  let* s = make_constr_set in
  (* let* l = make_constr_set in *)
  let module G = MkGraph ((val m)) ((val s)) (* ((val l)) *) in
  return (module G : GraphB)
;;

let build_rlts_map
      ?(params : Params.log = default_params)
      (grefs : Names.GlobRef.t list)
  : term_type_map * term_type_map
  =
  let trmap : term_type_map = Hashtbl.create (List.length grefs)
  and fnmap : term_type_map = Hashtbl.create (List.length grefs)
  and map_id : unit -> int = new_int_counter () in
  params.override <- Some ();
  List.iter
    (fun (gref : Names.GlobRef.t) ->
       let rlts : raw_lts = run (check_ref_lts gref) in
       log
         ~params
         (Printf.sprintf "= = = = = = = = =\n\trlts (#%d):" (map_id ()));
       let _ = log_raw_lts ~params rlts in
       (* FIXME: avoid two keys mapping to same rlts *)
       (* - [build_graph] requires [tref -> raw_lts] *)
       (* - [check_updated_ctx] requires [fn -> raw_lts] *)
       Hashtbl.add trmap rlts.trm_type rlts;
       Hashtbl.add fnmap rlts.coq_lts rlts)
    grefs;
  params.override <- None;
  trmap, fnmap
;;

(** *)
let build_bounded_lts
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unknown")
      (tr_rlts : term_type_map)
      (fn_rlts : term_type_map)
      (tref : Constrexpr.constr_expr)
      (module G : GraphB)
  : Lts.lts mm
  =
  (* graph lts *)
  let* graph_lts = G.build_graph ~params tr_rlts fn_rlts tref bound in
  log ~params (Printf.sprintf "= = = = = = = = =\n");
  if G.S.cardinal graph_lts.states > bound
  then
    log
      ~params
      (Printf.sprintf
         "Warning: LTS graph is incomplete, exceeded bound: %i.\n"
         bound);
  (* show edges *)
  (* log
     ~params
     (Printf.sprintf
     "(e) Graph Edges: %s.\n"
     (run (G.PStr.transitions ~params:(Log params) graph_lts.transitions))); *)
  (* show if normal output allowed from outside call *)
  if params.options.show_debug_output
  then params.kind <- Details ()
  else params.kind <- Normal ();
  (* log
     ~params
     (Printf.sprintf
     "Constructed LTS: %s.\n"
     (run (G.PStr.lts ~params:(Log params) graph_lts))); *)
  (* pure lts *)
  let* the_pure_lts, coq_translation =
    G.DeCoq.lts_graph_to_lts ~params ~bound ~name graph_lts tref
  in
  return the_pure_lts
;;

(** *)
let build_fsm_from_bounded_lts
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unknown")
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
  : Fsm.fsm mm
  =
  (* disable detailed printouts *)
  params.options.show_normal_output <- false;
  (* list of raw coq lts *)
  let (tr_rlts, fn_rlts) : term_type_map * term_type_map =
    build_rlts_map ~params grefs
  in
  (* print out the type of the term *)
  params.override <- Some ();
  log
    ~params
    (Printf.sprintf
       "(A) type of tref: %s"
       (econstr_to_string (run (Mebi_utils.type_of_tref tref))));
  params.override <- None;
  (* graph module *)
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  (* get pure lts *)
  let* (the_lts : Lts.lts) =
    build_bounded_lts ~params ~bound ~name tr_rlts fn_rlts tref (module G)
  in
  (* translate to fsm *)
  params.options.show_normal_output <- true;
  return (Translate.to_fsm the_lts)
;;

module Vernac = struct
  module LTS = struct
    let build
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "unknown")
          ?(equiv : Names.GlobRef.t option)
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : Lts.lts mm
      =
      (* list of raw coq lts *)
      let (tr_rlts, fn_rlts) : term_type_map * term_type_map =
        build_rlts_map ~params grefs
      in
      (* print out the type of the term *)
      params.override <- Some ();
      log
        ~params
        (Printf.sprintf
           "(B) type of tref: %s"
           (econstr_to_string (run (Mebi_utils.type_of_tref tref))));
      if Bool.not (List.mem name [ "e3"; "e4" ]) then params.override <- None;
      (* graph module *)
      let* graphM = make_graph_builder in
      let module G = (val graphM) in
      (* get pure lts *)
      build_bounded_lts ~params ~bound ~name tr_rlts fn_rlts tref (module G)
    ;;

    let show
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(equiv : Names.GlobRef.t option)
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      let* (the_lts : Lts.lts) = build ~params ~bound tref grefs in
      params.kind <- Normal ();
      params.override <- Some ();
      log
        ~params
        (Printf.sprintf "LTS: %s" (Lts.PStr.lts ~params:(Log params) the_lts));
      return ()
    ;;

    let dump
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "dump")
          ?(equiv : Names.GlobRef.t option)
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      params.options.output_enabled <- false;
      let* (the_lts : Lts.lts) = build ~params ~bound ~name tref grefs in
      let make_dump : bool =
        match the_lts.info with
        | None -> true
        | Some i -> i.is_complete
      in
      if make_dump
      then (
        let dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (LTS name)
            (JSON ())
            (LTS the_lts)
        in
        params.options.output_enabled <- true;
        params.kind <- Normal ();
        params.override <- Some ();
        Log.normal
          ~params
          (Printf.sprintf "Dumped LTS into: %s.\n" dump_filepath))
      else
        Log.normal
          ~params
          (Printf.sprintf
             "Incomplete, LTS (%s) should already be dumped.\n"
             name);
      return ()
    ;;
  end

  module FSM = struct
    (** *)
    let build
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "unknown")
          ?(equiv : Names.GlobRef.t option)
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : Fsm.fsm mm
      =
      build_fsm_from_bounded_lts ~params ~bound ~name tref grefs
    ;;

    let show
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(equiv : Names.GlobRef.t option)
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm = build ~params ~bound tref grefs in
      (* show if normal output allowed from outside call *)
      if params.options.show_debug_output
      then params.kind <- Details ()
      else params.kind <- Normal ();
      log
        ~params
        (Printf.sprintf "FSM: %s" (Fsm.PStr.fsm ~params:(Log params) the_fsm));
      return ()
    ;;

    let dump
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "dump")
          ?(equiv : Names.GlobRef.t option)
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm = build ~params ~bound ~name tref grefs in
      let dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM name)
          (JSON ())
          (FSM the_fsm)
      in
      log ~params (Printf.sprintf "Dumped FSM into: %s.\n" dump_filepath);
      return ()
    ;;
  end

  module Minim = struct
    (** *)
    let build
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "unknown")
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : (Fsm.fsm * Fsm.fsm) mm
      =
      (* get translated fsm *)
      let* the_fsm = FSM.build ~params ~bound ~name tref grefs in
      let (minimized_fsm, _map_of_states) : fsm * (state, state) Hashtbl.t =
        Minimize.run ~params the_fsm
      in
      return (the_fsm, minimized_fsm)
    ;;

    (** *)
    let show
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm, minimized_fsm = build ~params ~bound tref grefs in
      (* show if normal output allowed from outside call *)
      if params.options.show_debug_output
      then params.kind <- Details ()
      else params.kind <- Normal ();
      log
        ~params
        (Printf.sprintf
           "Minimized: %s\n\nfrom: %s."
           (Fsm.PStr.fsm ~params:(Log params) minimized_fsm)
           (Fsm.PStr.fsm ~params:(Log params) the_fsm));
      return ()
    ;;

    let dump
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "dump")
          (tref : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm, minimized_fsm = build ~params ~bound ~name tref grefs in
      let fsm_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s tobe minim" name))
          (JSON ())
          (FSM the_fsm)
      and minim_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s post minim" name))
          (JSON ())
          (FSM minimized_fsm)
      in
      log
        ~params
        (Printf.sprintf
           "Dumped FSMs into: %s\nand %s.\n"
           fsm_dump_filepath
           minim_dump_filepath);
      return ()
    ;;
  end

  module Merged = struct
    (** *)
    let build
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "unknown")
          (trefA : Constrexpr.constr_expr)
          (trefB : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : (Fsm.fsm * Fsm.fsm * Fsm.fsm) mm
      =
      (* get translated fsm *)
      let* fsm_A =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s A" name) trefA grefs
      in
      let* fsm_B =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s B" name) trefB grefs
      in
      (* merge fsm *)
      let (merged_fsm, _translation_table) : Fsm.fsm * (state, state) Hashtbl.t =
        Fsm.Merge.fsms ~params fsm_A fsm_B
      in
      return (merged_fsm, fsm_A, fsm_B)
    ;;

    (** *)
    let show
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          (trefA : Constrexpr.constr_expr)
          (trefB : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get merged fsm *)
      let* merged_fsm, fsm_A, fsm_B = build ~params ~bound trefA trefB grefs in
      (* show if normal output allowed from outside call *)
      if params.options.show_debug_output
      then params.kind <- Details ()
      else params.kind <- Normal ();
      log
        ~params
        (Printf.sprintf
           "Merged FSM A with B :: %s.\n\nwhere A: %s,\n\nand B: %s.\n"
           (Fsm.PStr.fsm ~params:(Log params) merged_fsm)
           (Fsm.PStr.fsm ~params:(Log params) fsm_A)
           (Fsm.PStr.fsm ~params:(Log params) fsm_B));
      return ()
    ;;

    let dump
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "dump")
          (trefA : Constrexpr.constr_expr)
          (trefB : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get merged fsm *)
      let* merged_fsm, fsm_A, fsm_B =
        build ~params ~bound ~name trefA trefB grefs
      in
      let merged_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s merged fsm" name))
          (JSON ())
          (FSM merged_fsm)
      and fsm_a_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s to merge A" name))
          (JSON ())
          (FSM fsm_A)
      and fsm_b_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s to merge B" name))
          (JSON ())
          (FSM fsm_B)
      in
      log
        ~params
        (Printf.sprintf
           "Dumped FSMs into: %s\nand %s\nand %s.\n"
           merged_dump_filepath
           fsm_a_dump_filepath
           fsm_b_dump_filepath);
      return ()
    ;;
  end

  module Bisim = struct
    open Bisimilarity

    exception UnexpectedResultKind of Bisimilarity.result

    (** *)
    let build
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "unknown")
          (trefA : Constrexpr.constr_expr)
          (trefB : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : (Fsm.fsm * Fsm.fsm * Bisimilarity.result) mm
      =
      (* get translated fsm *)
      let* fsm_A =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s A" name) trefA grefs
      in
      let* fsm_B =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s B" name) trefB grefs
      in
      (* run bisimilarity algorithm *)
      let raw_result = RCP.KS90.run ~params (ToMerge (fsm_A, fsm_B)) () in
      let result = RCP.KS90.result ~params raw_result in
      return (fsm_A, fsm_B, result)
    ;;

    (** *)
    let show
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          (trefA : Constrexpr.constr_expr)
          (trefB : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      let* fsm_A, fsm_B, result = build ~params ~bound trefA trefB grefs in
      match result with
      | BisimResult result ->
        (* show if normal output allowed from outside call *)
        if params.options.show_debug_output
        then params.kind <- Details ()
        else params.kind <- Normal ();
        log
          ~params
          (Bisimilarity.PStr.bisim_result
             ~params
             ~merged_from:(fsm_A, fsm_B)
             result);
        return ()
      | _ -> raise (UnexpectedResultKind result)
    ;;

    let dump
          ?(params : Params.log = default_params)
          ?(bound : int = default_bound)
          ?(name : string = "dump")
          (trefA : Constrexpr.constr_expr)
          (trefB : Constrexpr.constr_expr)
          (grefs : Names.GlobRef.t list)
      : unit mm
      =
      let* fsm_A, fsm_B, result =
        build ~params ~bound ~name trefA trefB grefs
      in
      match result with
      | BisimResult result ->
        let merged_dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (FSM (Printf.sprintf "%s bisim merged fsm" name))
            (JSON ())
            (FSM result.merged_fsm)
        and fsm_a_dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (FSM (Printf.sprintf "%s bisim to merge A" name))
            (JSON ())
            (FSM fsm_A)
        and fsm_b_dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (FSM (Printf.sprintf "%s bisim to merge B" name))
            (JSON ())
            (FSM fsm_B)
        in
        log
          ~params
          (Printf.sprintf
             "Dumped FSMs into: %s\nand %s\nand %s.\n"
             merged_dump_filepath
             fsm_a_dump_filepath
             fsm_b_dump_filepath);
        return ()
      | _ -> raise (UnexpectedResultKind result)
    ;;
  end
end

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
