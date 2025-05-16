open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax

(* *)
open Utils.Logging
open Utils.Formatting
open Utils
open Fsm

let default_params : Params.log = Params.Default.log ~mode:(Coq ()) ()

(** [default_bound] is the total depth that will be explored of a given lts by [explore_lts]. *)
let default_bound : int = 10

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
  { coq_lts : EConstr.t
  ; trm_type : EConstr.types
  ; lbl_type : EConstr.types
  ; coq_ctor_names : Names.Id.t array
  ; constructor_transitions : (Constr.rel_context * Constr.types) array
  }

type term_type_map =
  (* (EConstr.types, (Constr.rel_context * Constr.types) array) Hashtbl.t *)
  (EConstr.types, raw_lts) Hashtbl.t

(** [_log_raw_lts] *)
let _log_raw_lts ?(params : Params.log = default_params) (rlts : raw_lts)
  : unit mm
  =
  Log.override
    ~params
    (Printf.sprintf
       "(a) Types of terms: %s.\n"
       (econstr_to_string rlts.trm_type));
  Log.override
    ~params
    (Printf.sprintf
       "(b) Types of labels: %s.\n"
       (econstr_to_string rlts.lbl_type));
  Log.override
    ~params
    (Printf.sprintf
       "(c) Constructors: %s.\n"
       (Pp.string_of_ppcmds
          (Pp.prvect_with_sep
             (fun _ -> str ", ")
             Names.Id.print
             rlts.coq_ctor_names)));
  Log.override
    ~params
    (Printf.sprintf
       "(d) Types of coq_lts: %s.\n"
       (econstr_to_string rlts.coq_lts));
  (* log ~params (Printf.sprintf "(f) Transitions (do not be alarmed by any
     _UNBOUND_ below): %s.\n" (if Array.length rlts.constructor_transitions < 1
     then "[] (empty)" else Printf.sprintf "[%s]" (Array.fold_left (fun (acc :
     string) (tr : Constr.rel_context * Constr.t) -> Printf.sprintf "%s %s\n"
     acc (constr_to_string (snd tr))) "\n" rlts.constructor_transitions))); *)
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
  (* let* _ = if is_output_kind_enabled params then debug (fun (env :
     Environ.env) (sigma : Evd.evar_map) -> str "Unifying (t0) :: " ++
     Printer.pr_econstr_env env sigma t0 ++ strbrk "\nUnifying (t1) :: " ++
     Printer.pr_econstr_env env sigma t1) else return () in *)
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

let rec unify_all
  ?(params : Params.log = default_params)
  (i : (Constr_tree.t * unif_problem) list)
  : Constr_tree.t list option t
  =
  params.kind <- Debug ();
  match i with
  | [] -> return (Some [])
  | (ctor_tree, u) :: t ->
    (* let* _ = if is_output_kind_enabled params then debug (fun env sigma ->
       str "UNIFYALL (termL) :::::::::: " ++ Printer.pr_econstr_env env sigma
       u.termL ++ strbrk "\nUNIFYALL (termR) :::::::::: " ++
       Printer.pr_econstr_env env sigma u.termR) else return () in *)
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
  (u : (Constr_tree.t * unif_problem) list)
  : (EConstr.t * Constr_tree.t list) option mm
  =
  (* let* _ = if is_output_kind_enabled params then debug (fun env sigma -> str
     "TGT:::::: " ++ Printer.pr_econstr_env env sigma tgt_term) else return ()
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
    - [Constr_tree.t] coq-constructor index *)
type coq_ctor = EConstr.t * EConstr.t * Constr_tree.t

(* [act] should probably come from the unification problems? *)
let rec retrieve_tgt_nodes
  ?(params : Params.log = default_params)
  (acc : coq_ctor list)
  (i : int)
  (act : EConstr.t)
  (tgt_term : EConstr.t)
  : (Constr_tree.t * unif_problem) list list -> coq_ctor list mm
  = function
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
  (acc : (Constr_tree.t * unif_problem) list list)
  (fn_rlts : term_type_map)
  :  EConstr.t list * EConstr.rel_declaration list
  -> (Constr_tree.t * unif_problem) list list option mm
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
        | None ->
          Log.warning
            ~params
            (Printf.sprintf
               "check_updated_ctx, fn_rlts does not have corresponding fn: %s."
               (econstr_to_string fn));
          check_updated_ctx acc fn_rlts (substl, tl)
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
                (fun (_, (tL : EConstr.t), (i : Constr_tree.t)) ->
                  i, { termL = tL; termR = args.(2) })
                ctors
            in
            (* We need to cross-product all possible unifications. This is in
               case we have a constructor of the form LTS t11 a1 t12 -> LTS t21
               a2 t22 -> ... -> LTS tn an t2n. Repetition may occur. It is not
               unavoidable, but we should make sure we understand well the
               problem before removing the source of repetition. *)
            (* FIXME: Test this *)
            (* replace [rtls] in [rtls_ctx] *)
            (* let rtls_ctx':rlts_list = List.append [ ] in *)
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
  : coq_ctor list mm
  =
  params.kind <- Debug ();
  (* let$+ t env sigma = Reductionops.nf_all env sigma t' in *)
  let* (t : EConstr.t) = normalize_econstr t' in
  let iter_body (i : int) (ctor_vals : coq_ctor list) =
    (* let* _ = if is_output_kind_enabled params then debug (fun env sigma ->
       str "CHECKING CONSTRUCTOR " ++ int i ++ str ". Term: " ++
       Printer.pr_econstr_env env sigma t) else return () in *)
    let (ctx, tm) : Constr.rel_context * Constr.t = ctor_transitions.(i) in
    let ctx_tys : EConstr.rel_declaration list =
      List.map EConstr.of_rel_decl ctx
    in
    let* substl = mk_ctx_substl [] (List.rev ctx_tys) in
    let (termL, act, termR) : Evd.econstr * Evd.econstr * Evd.econstr =
      extract_args substl tm
    in
    let* success = m_unify t termL in
    if success
    then
      let* success = Option.cata (fun a -> m_unify a act) (return true) ma in
      if success
      then
        let* (next_ctors : (Constr_tree.t * unif_problem) list list option) =
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
          else return ((act, tgt_term, Constr_tree.Node (i, [])) :: ctor_vals)
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

(** [GraphB] is ...
    (Essentially acts as a `.mli` for the [MkGraph] module.) *)
module type GraphB = sig
  module H : Hashtbl.S with type key = EConstr.t
  module S : Set.S with type elt = EConstr.t
  module D : Set.S with type elt = EConstr.t * Constr_tree.t
  (* module Q : Queue_intf.S with type t = EConstr.t *)

  type constr_transitions = (Mebi_action.action, D.t) Hashtbl.t

  type lts_graph =
    { to_visit : EConstr.t Queue.t
    ; init : EConstr.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    }

  val insert_constr_transition
    :  constr_transitions
    -> Mebi_action.action
    -> EConstr.t
    -> Constr_tree.t
    -> unit mm

  val add_new_term_constr_transition
    :  lts_graph
    -> EConstr.t
    -> Mebi_action.action
    -> EConstr.t
    -> Constr_tree.t
    -> unit mm

  val build_lts_graph
    :  ?params:Params.log
    -> raw_lts H.t (* -> raw_lts H.t -> raw_lts H.t *)
    -> lts_graph
    -> int
    -> lts_graph mm

  val build_graph
    :  ?params:
         Params.log
         (* -> term_type_map  *)
         (*  -> term_type_map *)
    -> Constrexpr.constr_expr
    -> Names.GlobRef.t list
    -> int
    -> lts_graph mm

  module DeCoq : sig
    type coq_translation =
      { from_coq : string H.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    (* val translate_coq_terms : ?params:Params.log -> S.t -> coq_translation mm

       val translate_coq_lts : ?params:Params.log -> constr_transitions H.t ->
       coq_translation (* -> string list *) -> Lts.raw_flat_lts mm *)

    val lts_graph_to_lts
      :  ?params:Params.log
      -> ?bound:int
      -> ?name:string
      -> lts_graph
      -> (Lts.lts * coq_translation) mm
  end
end

(** [MkGraph M] is ...
    [M] is a ... *)
module MkGraph
    (M : Hashtbl.S with type key = EConstr.t)
    (N : Set.S with type elt = EConstr.t)
    (P : Set.S with type elt = EConstr.t * Constr_tree.t) : GraphB =
(* (O : Queue_intf.S with type t = EConstr.t) *)
struct
  (* [H] is the hashtbl of outgoing transitions, from some [EConstr.t] and also
     is used for mapping term types to [raw_lts]. *)
  module H = M

  (* [S] is the set of states, of [EConstr.t]. *)
  module S = N

  (* [D] is the set of destination tuples, each comprised of a term [EConstr.t]
     and the corresponding [Constr_tree.t]. *)
  module D = P

  (* [Q] is the queue of [EConstr.t] *)
  (* module Q = O *)

  (** [constr_transitions] is a hashtbl mapping [action]s to terms of [EConstr.t] and [Constr_tree.t]. *)
  type constr_transitions = (Mebi_action.action, D.t) Hashtbl.t

  let num_transitions (ts : constr_transitions H.t) : int =
    H.fold
      (fun (_from : EConstr.t) (transitions : constr_transitions) (acc : int) ->
        Hashtbl.fold
          (fun (_a : Mebi_action.action) (destinations : D.t) (acc' : int) ->
            acc' + D.cardinal destinations)
          transitions
          acc)
      ts
      0
  ;;

  (** [lts_graph] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Constr_tree.t]. *)
  type lts_graph =
    { to_visit : EConstr.t Queue.t
    ; init : EConstr.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    }

  let build_rlts_map (grefs : Names.GlobRef.t list) : raw_lts H.t mm =
    let num_grefs : int = List.length grefs in
    let trmap : raw_lts H.t = H.create num_grefs in
    let iter_body (i : int) (acc : raw_lts H.t) =
      let gref : Names.GlobRef.t = List.nth grefs i in
      let* (rlts : raw_lts) = check_ref_lts gref in
      (* normalize term type before adding to map *)
      let* (ty : EConstr.t) = normalize_econstr rlts.trm_type in
      H.add acc ty rlts;
      (* add name of inductive prop *)
      H.add acc rlts.coq_lts rlts;
      return acc
    in
    iterate 0 (num_grefs - 1) trmap iter_body
  ;;

  let get_rlts (t' : EConstr.t) (rlts_map : raw_lts H.t) : raw_lts mm =
    let* (t : EConstr.t) = normalize_econstr t' in
    let* (ty : EConstr.t) = type_of_econstr t in
    match H.find_opt rlts_map ty with
    | None -> unknown_term_type (t, ty, List.of_seq (H.to_seq_keys rlts_map))
    | Some rlts -> return rlts
  ;;

  (**********************************************************************)
  (******** below is a sanity check *************************************)
  (******** cannot be moved since it depends on this module *************)
  (******** (i.e., H, D, S )*********************************************)
  (**********************************************************************)

  let pstr_econstr_set (states : S.t) : string =
    if S.is_empty states
    then "[ ] (empty)"
    else
      Printf.sprintf
        "[%s\n]"
        (S.fold
           (fun (s : S.elt) (acc : string) ->
             Printf.sprintf "%s\n  %s\n" acc (econstr_to_string s))
           states
           "")
  ;;

  let _pstr_constr_transition
    (f : EConstr.t)
    (a : Mebi_action.action)
    ((d, c) : EConstr.t * Constr_tree.t)
    : string
    =
    Printf.sprintf
      "{|from: %s; label: %s; dest: %s; tree: %s|}"
      (econstr_to_string f)
      a.label
      (econstr_to_string d)
      (Constr_tree.pstr c)
  ;;

  (* let _pstr_list_constr_transitions (f : EConstr.t) (a : Mebi_action.action)
     (ds : D.t) : string list = let l = D.to_list ds in let hd = List.hd l and
     tl = List.tl l in List.fold_left (fun (acc : string list) d ->
     _pstr_constr_transition f a d :: acc) [ _pstr_constr_transition f a hd ] tl
     ;;

     let _pstr_list_term_transitions (f : EConstr.t) (cs : constr_transitions) :
     string list = let keys = List.of_seq (Hashtbl.to_seq_keys cs) in let hd =
     List.hd keys and tl = List.tl keys in let hd_ds = Hashtbl.find cs hd in
     List.fold_left (fun (acc : string list) (a : Mebi_action.action) ->
     List.append (_pstr_list_constr_transitions f a (Hashtbl.find cs a)) acc)
     (_pstr_list_constr_transitions f hd hd_ds) tl ;;

     let _pstr_list_transitions (ts : constr_transitions H.t) : string list list
     mm = let keys = List.of_seq (H.to_seq_keys ts) in let hd = List.hd keys and
     tl = List.tl keys in let hd_ts = H.find ts hd in return (List.fold_left
     (fun (acc : string list list) (f : EConstr.t) ->
     _pstr_list_term_transitions f (H.find ts f) :: acc) [
     _pstr_list_term_transitions hd hd_ts ] tl) ;; *)

  let _check_for_duplicate_states ?(prefix : string = "") (g : lts_graph)
    : unit mm
    =
    let state_list : EConstr.t list = S.to_list g.states in
    let iter_body (i : int) (dupes1 : (string, int * int) Hashtbl.t) =
      let s1 : EConstr.t = List.nth state_list i in
      let pstr1 : string = econstr_to_string s1 in
      let iter_inner (j : int) (dupes2 : (string, int * int) Hashtbl.t) =
        if Int.equal i j
        then return dupes2
        else (
          let s2 : EConstr.t = List.nth state_list j in
          let pstr2 : string = econstr_to_string s2 in
          let* sigma = get_sigma in
          let are_eq : bool = EConstr.eq_constr sigma s1 s2 in
          let b_val : int = if are_eq then 1 else 0 in
          if String.equal pstr1 pstr2
          then (
            match Hashtbl.find_opt dupes2 pstr1 with
            | None -> Hashtbl.add dupes2 pstr1 (1, b_val)
            | Some (sum, b) -> Hashtbl.replace dupes2 pstr1 (sum + 1, b + b_val));
          return dupes2)
      in
      iterate 0 (List.length state_list - 1) dupes1 iter_inner
    in
    let* dupes =
      iterate 0 (List.length state_list - 1) (Hashtbl.create 0) iter_body
    in
    let num_dupes = Hashtbl.length dupes in
    if num_dupes > 0
    then
      Log.warning
        ~params:default_params
        (Printf.sprintf
           "%sfound (%i) duplicate states: [%s]"
           prefix
           num_dupes
           (Hashtbl.fold
              (fun (k : string) ((v, b) : int * int) (acc : string) ->
                Printf.sprintf
                  "%s\nduplicates (%i), eq_constr (%i):\n  %s\n"
                  acc
                  v
                  b
                  k)
              dupes
              ""));
    return ()
  ;;

  let _check_for_duplicate_transitions
    ?(prefix : string = "")
    ?(none : string option)
    (g : lts_graph)
    : unit mm
    =
    (* Log.override "_check_for_duplicate_transitions, begin."; *)
    if Bool.not (H.length g.transitions > 1)
    then return ()
    else (
      let raw_list : (EConstr.t * constr_transitions) list =
        List.of_seq (H.to_seq g.transitions)
      in
      let from_body
        (i : int)
        (new_transitions :
          (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
        =
        let (from, actions) : EConstr.t * constr_transitions =
          List.nth raw_list i
        in
        let raw_actions : (Mebi_action.action * D.t) list =
          List.of_seq (Hashtbl.to_seq actions)
        in
        let action_body
          (j : int)
          (new_transitions :
            (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
          =
          let (a, destinations) : Mebi_action.action * D.t =
            List.nth raw_actions j
          in
          let raw_destinations : (EConstr.t * Constr_tree.t) list =
            D.elements destinations
          in
          let destination_body
            (k : int)
            (new_transitions :
              (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
            =
            let (destination, constr_tree) : EConstr.t * Constr_tree.t =
              List.nth raw_destinations k
            in
            return ((from, a, destination, constr_tree) :: new_transitions)
          in
          let* new_transitions' =
            iterate 0 (List.length raw_destinations - 1) [] destination_body
          in
          return (List.append new_transitions' new_transitions)
        in
        let* new_transitions' =
          iterate 0 (List.length raw_actions - 1) [] action_body
        in
        return (List.append new_transitions' new_transitions)
      in
      let* flattened_transitions =
        iterate 0 (List.length raw_list - 1) [] from_body
      in
      let iter_dupe
        (i : int)
        ((acc, cache) :
          string list
          * (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
        =
        let t1 = List.nth flattened_transitions i in
        let from1, a1, dest1, tree1 = t1 in
        let pstr_from1 = econstr_to_string from1 in
        let pstr_dest1 = econstr_to_string dest1 in
        let iter_dupe2 (j : int) dupe_list =
          if Int.equal i j
          then return dupe_list
          else (
            let t2 = List.nth flattened_transitions j in
            let from2, a2, dest2, tree2 = t2 in
            let pstr_from2 = econstr_to_string from2 in
            let pstr_dest2 = econstr_to_string dest2 in
            let from_eq = String.equal pstr_from1 pstr_from2 in
            let dest_eq = String.equal pstr_dest1 pstr_dest2 in
            let a_eq = String.equal a1.label a2.label in
            let tree_eq = Constr_tree.eq tree1 tree2 in
            if from_eq && dest_eq && a_eq && tree_eq
            then
              return
                (Printf.sprintf
                   "from: %s\nact:  %s\ndest: %s\ntree: %s"
                   pstr_from2
                   a2.label
                   pstr_dest2
                   (Constr_tree.pstr tree2)
                 :: dupe_list)
            else return dupe_list)
        in
        let* dupe_list =
          iterate 0 (List.length flattened_transitions - 1) [] iter_dupe2
        in
        if List.is_empty dupe_list
        then return (acc, t1 :: cache)
        else (
          let dupe_str =
            Printf.sprintf
              "for transition:\n\
               from: %s\n\
               act:  %s\n\
               dest: %s\n\
               tree: %s\n\n\
               found duplicates: [%s]"
              pstr_from1
              a1.label
              pstr_dest1
              (Constr_tree.pstr tree1)
              (List.fold_left
                 (fun (acc : string) (d : string) ->
                   Printf.sprintf "%s\n%s\n" acc d)
                 ""
                 dupe_list)
          in
          return (dupe_str :: acc, cache))
      in
      let* pstr_dupes, _ =
        iterate 0 (List.length flattened_transitions - 1) ([], []) iter_dupe
      in
      if List.is_empty pstr_dupes
      then (
        match none with
        | None -> ()
        | Some s ->
          Log.override ~params:default_params (Printf.sprintf "%s%s" prefix s))
      else
        Log.warning
          ~params:default_params
          (Printf.sprintf
             "%sduplicates found: [%s]"
             prefix
             (List.fold_left
                (fun (acc : string) (d : string) ->
                  Printf.sprintf "%s\n  %s\n" acc d)
                ""
                pstr_dupes));
      return ())
  ;;

  let _check_transition_states ?(prefix : string = "") (g : lts_graph) : unit mm
    =
    Log.override
      ~params:default_params
      (Printf.sprintf "%s_check_transition_states, begin." prefix);
    H.iter
      (fun (from : EConstr.t) (actions : constr_transitions) ->
        (* check [from] is in [g.states] *)
        Hashtbl.iter
          (fun (a : Mebi_action.action) (destinations : D.t) ->
            D.iter
              (fun ((destination, _constr_tree) : D.elt) ->
                let is_from_missing : bool = Bool.not (S.mem from g.states) in
                let is_dest_missing : bool =
                  Bool.not (S.mem destination g.states)
                in
                if Bool.( || ) is_from_missing is_dest_missing
                then
                  Log.warning
                    ~params:default_params
                    (Printf.sprintf
                       "%s_check_transition_states, in transition:\n\
                        %s\n\n\
                        did not find states: %s%s\n\
                        in states: %s\n\n\
                        %s"
                       prefix
                       (Printf.sprintf
                          "from: %s\nlabel: %s\ndest: %s\nctor_tree: %s"
                          (econstr_to_string from)
                          a.label
                          (econstr_to_string destination)
                          (Constr_tree.pstr _constr_tree))
                       (if is_from_missing then Printf.sprintf "'from'" else "")
                       (if is_dest_missing then Printf.sprintf "'dest'" else "")
                       (pstr_econstr_set g.states)
                       (let singleton_matches : S.t =
                          S.filter
                            (fun (s : S.elt) ->
                              Bool.( || )
                                (is_from_missing && S.mem from (S.singleton s))
                                (is_dest_missing
                                 && S.mem destination (S.singleton s)))
                            g.states
                        in
                        Printf.sprintf
                          "found matches (%i) : %s\n"
                          (S.cardinal singleton_matches)
                          (pstr_econstr_set singleton_matches))))
              destinations)
          actions)
      g.transitions;
    Log.override ~params:default_params "_check_transition_states, end.";
    return ()
  ;;

  (**********************************************************************)
  (******** above is a sanity check *************************************)
  (**********************************************************************)

  (** [insert_constr_transition] handles adding the mapping of action [a] to tuple [(term * Constr_tree.t)] in a given [constr_transitions]. *)
  let insert_constr_transition
    (constrs : constr_transitions)
    (a : Mebi_action.action)
    (d : EConstr.t)
    (c : Constr_tree.t)
    : unit mm
    =
    (match Hashtbl.find_opt constrs a with
     | None -> Hashtbl.add constrs a (D.singleton (d, c))
     | Some ds -> Hashtbl.replace constrs a (D.add (d, c) ds));
    return ()
  ;;

  let add_new_term_constr_transition
    (g : lts_graph)
    (t : EConstr.t)
    (a : Mebi_action.action)
    (d : EConstr.t)
    (c : Constr_tree.t)
    : unit mm
    =
    H.add
      g.transitions
      t
      (Hashtbl.of_seq (List.to_seq [ a, D.singleton (d, c) ]));
    return ()
  ;;

  let get_new_states
    ?(params : Params.log = default_params)
    (t : EConstr.t)
    (g : lts_graph)
    (ctors : coq_ctor list)
    : S.t mm
    =
    let iter_body (i : int) (new_states : S.t) =
      let (act, tgt, int_tree) : coq_ctor = List.nth ctors i in
      let* sigma = get_sigma in
      (* TODO: detect tau transitions and then defer to [Fsm.tau] instead. *)
      let is_tau : bool = false in
      let* (label : string) = econstr_to_string_mm act in
      let to_add : Mebi_action.action = { label; is_tau } in
      (match H.find_opt g.transitions t with
       | None ->
         let _ = add_new_term_constr_transition g t to_add tgt int_tree in
         ()
       | Some actions ->
         let _ = insert_constr_transition actions to_add tgt int_tree in
         ());
      (* if [tgt] has not been explored then add [to_visit] *)
      if H.mem g.transitions tgt
         || EConstr.eq_constr sigma tgt t
         || S.mem tgt g.states
      then ()
      else Queue.push tgt g.to_visit;
      (* add [tgt] to [new_states] *)
      (* if Bool.( || ) (S.mem tgt new_states) (S.mem tgt g.states) then return
         new_states else return (S.add tgt new_states) *)
      return (S.add tgt new_states)
    in
    iterate 0 (List.length ctors - 1) (S.singleton t) iter_body
  ;;

  (** [get_new_constrs t tr_rlts fn_rlts] returns the list of constructors applicable to term [t], using those provided in [tr_rlts] (and [fn_rlts]).
      If no immediate constructor is found matching [t] in [tr_rlts] (likely due to unification problems), then each constructor in [tr_rlts] is tried sequentially, until one of them returns some valid constructors.
      @raise CannotFindTypeOfTermToVisit
        if none of the constructors provided in [tr_rlts] yield constructors from [check_valid_constructors]. *)
  let get_new_constrs
    ?(params : Params.log = default_params)
    (t : EConstr.t)
    (tr_rlts : raw_lts H.t)
    : coq_ctor list mm
    =
    let* (the_rlts : raw_lts) = get_rlts t tr_rlts in
    let temp_map = Hashtbl.of_seq (H.to_seq tr_rlts) in
    check_valid_constructor
      ~params
      the_rlts.constructor_transitions
      temp_map
      t
      None
  ;;

  (** [build_lts_graph fn_rlts g bound] is an [lts_graph] [g] obtained by exploring [fn_rlts].
      @param fn_rlts maps coq-term names to [raw_lts].
      @param g is an [lts_graph] accumulated while exploring [rlts].
      @param bound is the number of states to explore until.
      @return an [lts_graph] with a maximum of [bound] many states. *)
  let rec build_lts_graph
    ?(params : Params.log = default_params)
    (tr_rlts : raw_lts H.t)
    (g : lts_graph)
    (bound : int)
    : lts_graph mm
    =
    params.kind <- Debug ();
    if Queue.is_empty g.to_visit
    then return g (* finished if no more to visit*)
    else if S.cardinal g.states > bound
    then return g (* exit if bound reached *)
    else (
      let t : EConstr.t = Queue.pop g.to_visit in
      let* (new_constrs : coq_ctor list) = get_new_constrs ~params t tr_rlts in
      (* [get_new_states] also updates [g.to_visit] *)
      let* (new_states : S.t) = get_new_states ~params t g new_constrs in
      (* let* _ = _check_for_duplicate_states ~prefix:"A build_lts_graph, " g in
         Log.override ~params (Printf.sprintf "old states: %s\n\nnew states:
         %s\n\nunion: %s" (pstr_econstr_set g.states) (pstr_econstr_set
         new_states) (pstr_econstr_set (S.union g.states new_states))); *)
      (* let g : lts_graph = { g with states = S.union g.states new_states }
         in *)
      let g : lts_graph = { g with states = S.union g.states new_states } in
      (* let* _ = _check_for_duplicate_transitions ~prefix:"build_lts_graph, "
         ~none:"No duplicates found" g in let* _ = _check_for_duplicate_states
         ~prefix:"build_lts_graph, " g in let* _ = _check_transition_states
         ~prefix:"build_lts_graph, " g in *)
      build_lts_graph ~params tr_rlts (* fn_rlts *) g bound)
  ;;

  (** [build_graph tr_rlts fn_rlts tref bound] is the entry point for [build_lts_graph].
      @param tr_rlts maps coq-term types to [raw_lts].
      @param fn_rlts
        is passed to [build_lts_graph] and maps coq-term names to [raw_lts].
      @param tref is the original coq-term.
      @param bound is the number of states to explore until. *)
  let build_graph
    ?(params : Params.log = default_params)
    (tref : Constrexpr.constr_expr)
    (grefs : Names.GlobRef.t list)
    (bound : int)
    : lts_graph mm
    =
    (* make map of term types *)
    let* (tr_rlts : raw_lts H.t) = build_rlts_map grefs in
    (* should be able to get the type now -- fail otherwise *)
    let* (t : EConstr.t) = tref_to_econstr tref in
    let* (the_lts : raw_lts) = get_rlts t tr_rlts in
    (* update environment by typechecking *)
    let$* u env sigma = Typing.check env sigma t the_lts.trm_type in
    let$ init env sigma = sigma, Reductionops.nf_all env sigma t in
    let q = Queue.create () in
    let* _ = return (Queue.push init q) in
    let* g =
      build_lts_graph
        ~params
        tr_rlts
        { to_visit = q; states = S.empty; init; transitions = H.create bound }
        bound
    in
    let* _ =
      _check_for_duplicate_transitions
        ~prefix:"build_graph, "
        ~none:"No duplicates found"
        g
    in
    let* _ = _check_for_duplicate_states ~prefix:"build_graph, " g in
    let* _ = _check_transition_states ~prefix:"build_graph, " g in
    return g
  ;;

  module DeCoq = struct
    type coq_translation =
      { from_coq : string H.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    let create_translation_tbl
      ?(params : Params.log = default_params)
      (states : S.t)
      : coq_translation mm
      =
      let list_states : EConstr.t list = S.elements states in
      let iter_body
        (i : int)
        ((from_coq_list, to_coq_list) :
          (EConstr.t * string) list * (string * EConstr.t) list)
        =
        let s : EConstr.t = List.nth list_states i in
        let* (str : string) = econstr_to_string_mm s in
        (* since [list_states] is from a set, we assume no duplicates. *)
        return ((s, str) :: from_coq_list, (str, s) :: to_coq_list)
      in
      let* from_coq_list, to_coq_list =
        iterate 0 (List.length list_states - 1) ([], []) iter_body
      in
      let from_coq : string H.t = H.of_seq (List.to_seq from_coq_list) in
      let to_coq : (string, EConstr.t) Hashtbl.t =
        Hashtbl.of_seq (List.to_seq to_coq_list)
      in
      let translation_tbl : coq_translation = { from_coq; to_coq } in
      return translation_tbl
    ;;

    let create_transitions_list
      ?(params : Params.log = default_params)
      (transitions : constr_transitions H.t)
      : (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list mm
      =
      let raw_list : (EConstr.t * constr_transitions) list =
        List.of_seq (H.to_seq transitions)
      in
      let from_body
        (i : int)
        (new_transitions :
          (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
        =
        let (from, actions) : EConstr.t * constr_transitions =
          List.nth raw_list i
        in
        let raw_actions : (Mebi_action.action * D.t) list =
          List.of_seq (Hashtbl.to_seq actions)
        in
        let action_body
          (j : int)
          (new_transitions :
            (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
          =
          let (a, destinations) : Mebi_action.action * D.t =
            List.nth raw_actions j
          in
          let raw_destinations : (EConstr.t * Constr_tree.t) list =
            D.elements destinations
          in
          let destination_body
            (k : int)
            (new_transitions :
              (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
            =
            let (destination, constr_tree) : EConstr.t * Constr_tree.t =
              List.nth raw_destinations k
            in
            return ((from, a, destination, constr_tree) :: new_transitions)
          in
          let* new_transitions' =
            iterate 0 (List.length raw_destinations - 1) [] destination_body
          in
          return (List.append new_transitions' new_transitions)
        in
        let* new_transitions' =
          iterate 0 (List.length raw_actions - 1) [] action_body
        in
        return (List.append new_transitions' new_transitions)
      in
      iterate 0 (List.length raw_list - 1) [] from_body
    ;;

    let _check_all_states_translated
      (translation_tbl : coq_translation)
      (g : lts_graph)
      : unit mm
      =
      S.iter
        (fun (s : EConstr.t) ->
          match H.find_opt translation_tbl.from_coq s with
          | None ->
            let (s_str : string) = econstr_to_string s in
            Log.warning
              ~params:default_params
              (Printf.sprintf
                 "translate_transitions, state not translated: %s"
                 s_str)
          | Some _ -> ())
        g.states;
      return ()
    ;;

    let get_state_translation
      ?(prefix : string = "?")
      (state : EConstr.t)
      (translation_tbl : coq_translation)
      (g : lts_graph)
      : string mm
      =
      let* (_str : string) = econstr_to_string_mm state in
      match H.find_opt translation_tbl.from_coq state with
      | None ->
        let missing_state : string =
          Printf.sprintf "UNKNOWN_%s_STATE:\n%s" prefix _str
        in
        Log.warning
          ~params:default_params
          (Printf.sprintf
             "%s\n\
              is in states: %s\n\n\
              is term in tbl: %b %b\n\
              is str in tbl: %b %b"
             missing_state
             (if S.mem state g.states
              then (
                let matches : S.t =
                  S.filter
                    (fun (s : S.elt) -> S.mem state (S.singleton s))
                    g.states
                in
                Printf.sprintf
                  "true\nmatches (%i): %s\n"
                  (S.cardinal matches)
                  (pstr_econstr_set matches))
              else "false")
             (H.mem translation_tbl.from_coq state)
             (List.mem
                state
                (List.of_seq (Hashtbl.to_seq_values translation_tbl.to_coq)))
             (Hashtbl.mem translation_tbl.to_coq _str)
             (List.mem
                _str
                (List.of_seq (H.to_seq_values translation_tbl.from_coq))));
        return missing_state
      | Some s -> return s
    ;;

    let translate_transitions
      (transitions_list :
        (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
      (translation_tbl : coq_translation)
      (g : lts_graph)
      : Lts.raw_flat_lts mm
      =
      (* sanity check: all states should be translated *)
      let* _ = _check_all_states_translated translation_tbl g in
      let iter_body (i : int) (acc : Lts.raw_flat_lts) =
        let (from, a, destination, constr_tree)
          : S.elt * Mebi_action.action * S.elt * Constr_tree.t
          =
          List.nth transitions_list i
        in
        let* (from_str : string) =
          get_state_translation ~prefix:"FROM" from translation_tbl g
        in
        let a_str : string = a.label in
        let* (dest_str : string) =
          get_state_translation ~prefix:"DEST" destination translation_tbl g
        in
        let constr_tree_str : string = Constr_tree.pstr constr_tree in
        return ((from_str, a_str, dest_str, Some constr_tree_str) :: acc)
      in
      iterate 0 (List.length transitions_list - 1) [] iter_body
    ;;

    let lts_graph_to_lts
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      (g : lts_graph)
      : (Lts.lts * coq_translation) mm
      =
      let* (transitions_list :
             (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
        =
        create_transitions_list ~params g.transitions
      in
      (* bidirectional mapping between [EConstr.t] and [string] *)
      let* translation_tbl = create_translation_tbl g.states in
      let* (flat_rlts : Lts.raw_flat_lts) =
        translate_transitions transitions_list translation_tbl g
      in
      let init : string = H.find translation_tbl.from_coq g.init in
      let is_complete : bool = Queue.is_empty g.to_visit in
      let num_states : int = S.cardinal g.states in
      let num_edges : int = num_transitions g.transitions in
      let info : Utils.model_info =
        { is_complete; bound; num_states; num_edges }
      in
      let (string_states : string list) =
        List.of_seq (H.to_seq_values translation_tbl.from_coq)
      in
      let lts : Lts.lts =
        Lts.Create.lts ~init ~info (Flat (flat_rlts, Some string_states))
      in
      return (lts, translation_tbl)
    ;;
  end
  (* TODO: from [LTS.lts] and [coq_translation] create a coq term and save to
     file *)
  (* module ReCoq = struct (* let from_lts *) end *)
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  let* h = make_constr_tbl in
  (* hashtabl of terms to (edges) or (raw_lts) *)
  let* s = make_constr_set in
  (* set of states (econstr term) *)
  let* d = make_constr_tree_set in
  (* hashtabl mapping term type or raw_lts *)
  let module G = MkGraph ((val h)) ((val s)) ((val d)) in
  return (module G : GraphB)
;;

(** *)
let build_bounded_lts
  ?(params : Params.log = default_params)
  ?(bound : int = default_bound)
  ?(name : string = "unnamed")
  (tref : Constrexpr.constr_expr)
  (grefs : Names.GlobRef.t list)
  (module G : GraphB)
  : Lts.lts mm
  =
  (* graph lts *)
  let* graph_lts = G.build_graph ~params tref grefs bound in
  Log.override ~params (Printf.sprintf "- - - - -");
  if G.S.cardinal graph_lts.states > bound
  then
    Log.warning
      ~params
      (Printf.sprintf "LTS graph is incomplete, exceeded bound: %i.\n" bound);
  (* pure lts *)
  let* the_pure_lts, coq_translation =
    G.DeCoq.lts_graph_to_lts ~params ~bound ~name graph_lts
  in
  return the_pure_lts
;;

(** *)
let build_fsm_from_bounded_lts
  ?(params : Params.log = default_params)
  ?(bound : int = default_bound)
  ?(name : string = "unnamed")
  (tref : Constrexpr.constr_expr)
  (grefs : Names.GlobRef.t list)
  : Fsm.fsm mm
  =
  (* disable detailed printouts *)
  params.options.show_normal_output <- false;
  (* Log.override ~params (Printf.sprintf "(A) type of tref: %s"
     (econstr_to_string (run (Mebi_utils.type_of_tref tref)))); *)
  (* graph module *)
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  (* get pure lts *)
  let* (the_lts : Lts.lts) =
    build_bounded_lts ~params ~bound ~name tref grefs (module G)
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
      ?(name : string = "unnamed")
      ?(equiv : Names.GlobRef.t option)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : Lts.lts mm
      =
      (* Log.override ~params (Printf.sprintf "(B) type of tref: %s"
         (econstr_to_string (run (Mebi_utils.type_of_tref tref)))); *)
      (* graph module *)
      let* graphM = make_graph_builder in
      let module G = (val graphM) in
      (* get pure lts *)
      build_bounded_lts ~params ~bound ~name tref grefs (module G)
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
      Log.normal
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
        match the_lts.info with None -> true | Some i -> i.is_complete
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
      ?(name : string = "unnamed")
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
      ?(name : string = "unnamed")
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
      ?(name : string = "unnamed")
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
      ?(name : string = "unnamed")
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
