(* open Pp *)

(* open Mebi_utils *)
(* open Mebi_monad *)
(* open Mebi_monad.Monad_syntax *)

(* open the wrapper *)
open Mebi_wrapper
open Mebi_wrapper.Syntax

(* *)
open Utils.Logging

(* open Utils.Formatting *)
open Utils
(* open Fsm *)

let default_params : Params.log = Params.Default.log ~mode:(Coq ()) ()

(** [default_bound] is the total depth that will be explored of a given lts by [explore_lts].
*)
let default_bound : int = 10

let arity_is_type (mip : Declarations.one_inductive_body) : unit mm =
  let open Declarations in
  match mip.mind_arity with
  | RegularArity s ->
    (match s.mind_sort with
     | Type _ -> return ()
     | _ -> invalid_sort (Sorts.family s.mind_sort))
  | TemplateArity t -> invalid_sort (Sorts.family t.template_level)
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
      if lts terms and labels cannot be obtained from [mip]. [mib] is only used in case of error.
*)
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

(** *)
(* type raw_lts =
  { index : int
  ; coq_lts : EConstr.t
  ; trm_type : EConstr.types
  ; lbl_type : EConstr.types
  ; coq_ctor_names : Names.Id.t array
  ; constructor_transitions : (Constr.rel_context * Constr.types) array
  } *)

(* type term_type_map = (EConstr.types, raw_lts) Hashtbl.t *)

(** coq_inductive_lts *)
type cind_lts =
  { trm_type : EConstr.t
  ; lbl_type : EConstr.t
  ; constr_transitions : (Constr.rel_context * Constr.t) array
  }

type coq_def_kind =
  | Type of EConstr.t option
  | LTS of cind_lts

(** coq_inductive_def_info *)
type cindef_info =
  { name : EConstr.t
  ; constr_names : Names.variable array
  }

(** coq_inductive_def *)
type cindef =
  { index : int
  ; info : cindef_info
  ; kind : coq_def_kind
  }

let lts_cindef_trm_type (c : cindef) : EConstr.t mm =
  match c.kind with LTS l -> return l.trm_type | _ -> invalid_cindef_kind ()
;;

let _lts_cindef_lbl_type (c : cindef) : EConstr.t mm =
  match c.kind with LTS l -> return l.lbl_type | _ -> invalid_cindef_kind ()
;;

let lts_cindef_constr_transitions (c : cindef)
  : (Constr.rel_context * Constr.t) array mm
  =
  match c.kind with
  | LTS l -> return l.constr_transitions
  | _ -> invalid_cindef_kind ()
;;

(** [_log_cindef] *)
let _log_cindef
      ?(params : Params.log = default_params)
      ?(key : E.t option)
      (c : cindef)
  : unit mm
  =
  let key_str =
    match key with
    | None -> ""
    | Some k -> Printf.sprintf "(key %s) " (E.to_string k)
  in
  let name = econstr_to_string c.info.name in
  let constr_names =
    Pp.string_of_ppcmds
      (Pp.prvect_with_sep
         (fun _ -> Pp.str ", ")
         Names.Id.print
         c.info.constr_names)
  in
  (match c.kind with
   | Type v ->
     let opt_val_str =
       match v with None -> "(no value given)" | Some v -> econstr_to_string v
     in
     Log.normal
       (Printf.sprintf
          "cindef %sType: %s\nof value: %s\nconstrs: %s\n"
          key_str
          name
          opt_val_str
          constr_names)
   | LTS l ->
     let term_type = econstr_to_string l.trm_type in
     let label_type = econstr_to_string l.lbl_type in
     let constr_transitions =
       "..."
       (* if Array.length l.constr_transitions < 1
          then "[] (empty)"
          else
          Printf.sprintf
          "[%s]"
          (Array.fold_left
          (fun (acc : string) (tr : Constr.rel_context * Constr.t) ->
          Printf.sprintf "%s %s\n" acc (constr_to_string (snd tr)))
          "\n"
          l.constr_transitions) *)
     in
     Log.normal
       (Printf.sprintf
          "cindef %sLTS: %s\n\
           constrs: %s\n\
           term type: %s\n\
           label type: %s\n\
           constr transitions: %s"
          key_str
          name
          constr_names
          term_type
          label_type
          constr_transitions));
  return ()
;;

let _log_cindef_map
      ?(params : Params.log = default_params)
      (primary_enc : E.t)
      (weak_enc : E.t option)
      (cindef_map : cindef B.t)
  : unit mm
  =
  Log.normal "\n/////////// log cindef map";
  (* output cindef map*)
  let cindef_list = List.of_seq (B.to_seq cindef_map) in
  let iter_body (i : int) () =
    let key, c = List.nth cindef_list i in
    _log_cindef ~params ~key c
  in
  let* _ = iterate 0 (List.length cindef_list - 1) () iter_body in
  (* output primary lts *)
  Log.normal
    (Printf.sprintf "cindef, primary: (enc => %s)" (E.to_string primary_enc));
  (* output weak type *)
  match weak_enc with
  | None ->
    Log.normal (Printf.sprintf "cindef, no weak enc.");
    Log.normal "///////////\n";
    return ()
  | Some w ->
    Log.normal (Printf.sprintf "cindef, weak: (enc => %s)" (E.to_string w));
    let* _ = _log_cindef ~params ~key:w (B.find cindef_map w) in
    Log.normal "///////////\n";
    return ()
;;

let check_ref_type (gref : Names.GlobRef.t) : cindef_info mm =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env i in
    let* _ = arity_is_type mip in
    let univ = mib.mind_univ_hyps in
    let type_term = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    return { name = type_term; constr_names = mip.mind_consnames }
  (* raise error if [gref] is not an inductive type *)
  | _ -> invalid_ref gref
;;

(** [check_ref_lts gref] is the [cindef] of [gref].

    @raise invalid_ref if [gref] is not a reference to an inductive type. *)
let check_ref_lts (gref : Names.GlobRef.t) : (cindef_info * cind_lts) mm =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env i in
    let* _ = arity_is_prop mip in
    let* lbl, term = get_lts_labels_and_terms mib mip in
    let univ = mib.mind_univ_hyps in
    (* lts of inductive type *)
    let lts_term = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    return
      ( { name = lts_term; constr_names = mip.mind_consnames }
      , { trm_type = EConstr.of_constr (Context.Rel.Declaration.get_type term)
        ; lbl_type = EConstr.of_constr (Context.Rel.Declaration.get_type lbl)
        ; constr_transitions = mip.mind_nf_lc
        } )
  (* raise error if [gref] is not an inductive type *)
  | _ -> invalid_ref gref
;;

let get_lts_cindef (i : int) (gref : Names.GlobRef.t) : cindef mm =
  let* ((c_info, c_lts) : cindef_info * cind_lts) = check_ref_lts gref in
  return { index = i; info = c_info; kind = LTS c_lts }
;;

let get_type_cindef (i : int) (gref : Names.GlobRef.t) (v : EConstr.t option)
  : cindef mm
  =
  let* (c_info : cindef_info) = check_ref_type gref in
  return { index = i; info = c_info; kind = Type v }
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
  : Constr_tree.t list option mm
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
  : int * (Constr_tree.t * unif_problem) list list -> coq_ctor list mm
  = function
  | _, [] -> return acc
  | lts_index, u1 :: nctors ->
    let* success = sandboxed_unify ~params tgt_term u1 in
    (match success with
     | None -> retrieve_tgt_nodes ~params acc i act tgt_term (lts_index, nctors)
     | Some (tgt, ctor_tree) ->
       let$+ act env sigma = Reductionops.nf_all env sigma act in
       retrieve_tgt_nodes
         ~params
         ((act, tgt, Node ((E.of_int lts_index, i), ctor_tree)) :: acc)
         i
         act
         tgt_term
         (lts_index, nctors))
;;

(* Should return a list of unification problems *)
let rec check_updated_ctx
          ?(params : Params.log = default_params)
          (acc : int * (Constr_tree.t * unif_problem) list list)
          (fn_cindef : cindef F.t)
  :  EConstr.t list * EConstr.rel_declaration list
  -> (int * (Constr_tree.t * unif_problem) list list) option mm
  = function
  | [], [] -> return (Some acc)
  | _ :: substl, t :: tl ->
    let$+ upd_t env sigma =
      EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t)
    in
    let* sigma = get_sigma in
    (match EConstr.kind sigma upd_t with
     | App (fn, args) ->
       (match F.find_opt fn_cindef fn with
        | None ->
          Log.warning
            ~params
            (Printf.sprintf
               "check_updated_ctx, fn_cindef does not have corresponding fn: \
                %s."
               (econstr_to_string fn));
          check_updated_ctx acc fn_cindef (substl, tl)
        | Some c ->
          let$+ nextT env sigma = Reductionops.nf_evar sigma args.(0) in
          let* c_constr_transitions = lts_cindef_constr_transitions c in
          let* ctors =
            check_valid_constructor
              ~params
              c_constr_transitions
              fn_cindef
              nextT
              (Some args.(1))
              c.index
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
              ( fst acc
              , List.concat_map
                  (fun x -> List.map (fun y -> y :: x) ctors)
                  (snd acc) )
              fn_cindef
              (substl, tl)))
     | _ -> check_updated_ctx acc fn_cindef (substl, tl))
  | _, _ -> assert false
(* Impossible! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

(** Checks possible transitions for this term: *)
and check_valid_constructor
      ?(params : Params.log = default_params)
      (ctor_transitions : (Constr.rel_context * Constr.types) array)
      (fn_cindef : cindef F.t)
      (t' : EConstr.t)
      (ma : EConstr.t option)
      (lts_index : int)
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
        let* (next_ctors :
               (int * (Constr_tree.t * unif_problem) list list) option)
          =
          check_updated_ctx
            ~params
            (lts_index, [ [] ])
            fn_cindef
            (substl, ctx_tys)
        in
        let$+ act env sigma = Reductionops.nf_all env sigma act in
        let tgt_term : EConstr.t = EConstr.Vars.substl substl termR in
        match next_ctors with
        | None -> return ctor_vals
        | Some index_ctor_pair ->
          (match snd index_ctor_pair with
           | [] ->
             let* sigma = get_sigma in
             if EConstr.isEvar sigma tgt_term
             then return ctor_vals
             else
               return
                 (( act
                  , tgt_term
                  , Constr_tree.Node ((E.of_int (fst index_ctor_pair), i), [])
                  )
                  :: ctor_vals)
           | nctors ->
             let tgt_nodes =
               retrieve_tgt_nodes
                 ~params
                 ctor_vals
                 i
                 act
                 tgt_term
                 index_ctor_pair
             in
             tgt_nodes)
      else return ctor_vals
    else return ctor_vals
  in
  iterate 0 (Array.length ctor_transitions - 1) [] iter_body
;;

(** [GraphB] is ...
    (Essentially acts as a `.mli` for the [MkGraph] module.) *)
module type GraphB = sig
  module H : Hashtbl.S with type key = E.t
  module S : Set.S with type elt = E.t
  module D : Set.S with type elt = E.t * Constr_tree.t

  type constr_transitions = (Model_action.t, D.t) Hashtbl.t

  type lts_graph =
    { to_visit : E.t Queue.t
    ; init : E.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    }

  val insert_constr_transition
    :  constr_transitions
    -> Model_action.t
    -> E.t
    -> Constr_tree.t
    -> unit mm

  val add_new_term_constr_transition
    :  lts_graph
    -> E.t
    -> Model_action.t
    -> E.t
    -> Constr_tree.t
    -> unit mm

  val build_lts_graph
    :  ?params:Params.log
    -> ?weak:E.t option
    -> cindef
    -> cindef B.t
    -> lts_graph
    -> int
    -> lts_graph mm

  val build_graph
    :  ?params:Params.log
    -> ?primary_lts:Libnames.qualid option
    -> ?weak:Vernac.weak_params option
    -> int
    -> Constrexpr.constr_expr
    -> Names.GlobRef.t list
    -> lts_graph mm

  val decoq_lts : ?name:string -> int -> lts_graph -> Lts.t mm
end

(** [MkGraph M] is ...
    [M] is a ... *)
module MkGraph
    (M : Hashtbl.S with type key = E.t)
    (N : Set.S with type elt = E.t)
    (P : Set.S with type elt = E.t * Constr_tree.t) : GraphB = struct
  (* [H] is the hashtbl of outgoing transitions, from some [EConstr.t] and also
     is used for mapping term types to [cindef]. *)
  module H = M

  (* [S] is the set of states, of [EConstr.t]. *)
  module S = N

  (* [D] is the set of destination tuples, each comprised of a term [EConstr.t]
     and the corresponding [Constr_tree.t]. *)
  module D = P

  (** [constr_transitions] is a hashtbl mapping [action]s to terms of [EConstr.t] and [Constr_tree.t].
  *)
  type constr_transitions = (Model_action.t, D.t) Hashtbl.t

  let num_transitions (ts : constr_transitions H.t) : int =
    H.fold
      (fun (_from : E.t) (transitions : constr_transitions) (acc : int) ->
        Hashtbl.fold
          (fun (_a : Model_action.t) (destinations : D.t) (acc' : int) ->
            acc' + D.cardinal destinations)
          transitions
          acc)
      ts
      0
  ;;

  let _flatten_transitions (ts : constr_transitions H.t)
    : (S.elt * Model_action.t * S.elt * Constr_tree.t) list mm
    =
    let raw_list : (E.t * constr_transitions) list =
      List.of_seq (H.to_seq ts)
    in
    let from_body
          (i : int)
          (new_transitions :
            (S.elt * Model_action.t * S.elt * Constr_tree.t) list)
      =
      let (from, actions) : E.t * constr_transitions = List.nth raw_list i in
      let raw_actions : (Model_action.t * D.t) list =
        List.of_seq (Hashtbl.to_seq actions)
      in
      let action_body
            (j : int)
            (new_transitions :
              (S.elt * Model_action.t * S.elt * Constr_tree.t) list)
        =
        let (a, destinations) : Model_action.t * D.t = List.nth raw_actions j in
        let raw_destinations : (E.t * Constr_tree.t) list =
          D.elements destinations
        in
        let destination_body
              (k : int)
              (new_transitions :
                (S.elt * Model_action.t * S.elt * Constr_tree.t) list)
          =
          let (destination, constr_tree) : E.t * Constr_tree.t =
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

  (** [lts_graph] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Constr_tree.t].
  *)
  type lts_graph =
    { to_visit : E.t Queue.t
    ; init : E.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    }

  (**********************************************************************)
  (******** below are debugging printouts *******************************)
  (**********************************************************************)

  let _print_constr_names (cindef_map : cindef H.t) : unit mm =
    Log.override
      ~params:default_params
      (Printf.sprintf
         "- - - - (constrs: %s)"
         (List.fold_left
            (fun (acc : string) (l : string) -> Printf.sprintf "%s'%s' " acc l)
            ""
            (H.fold
               (fun _k v (acc : string list) ->
                 let l : string = econstr_to_string v.info.name in
                 if List.mem l acc then acc else l :: acc)
               cindef_map
               [])));
    return ()
  ;;

  let _print_finished_build_graph
        ?(params : Params.log = default_params)
        (g : lts_graph)
        (primary_enc : E.t)
    : unit mm
    =
    let is_complete = Queue.is_empty g.to_visit in
    let* primary_decoding = decode primary_enc in
    let primary_lts = econstr_to_string primary_decoding in
    if Bool.not is_complete
    then
      Log.override
        ~params
        (Printf.sprintf
           "build_graph, finished (%s) -- %s\n\
            num states: %i\n\
            num transitions: %i"
           (if is_complete then "complete" else "incomplete")
           primary_lts
           (S.cardinal g.states)
           (num_transitions g.transitions));
    return ()
  ;;

  (**********************************************************************)
  (******** below is a sanity check *************************************)
  (******** cannot be moved since it depends on this module *************)
  (******** (i.e., H, D, S )*********************************************)
  (**********************************************************************)

  let _pstr_econstr_set (states : S.t) : string =
    if S.is_empty states
    then "[ ] (empty)"
    else
      Printf.sprintf
        "[%s\n]"
        (S.fold
           (fun (s : S.elt) (acc : string) ->
             Printf.sprintf "%s\n %s\n" acc (E.to_string s))
           states
           "")
  ;;

  (* let _pstr_constr_transition (f : EConstr.t) (a : Model_action.t) ((d,
     c) : EConstr.t * Constr_tree.t) : string = Printf.sprintf "{|from: %s;
     label: %s; dest: %s; tree: %s|}" (econstr_to_string f) a.label
     (econstr_to_string d) (Constr_tree.pstr c) ;; *)

  (* let _check_for_duplicate_states ?(prefix : string = "") (g : lts_graph) :
     unit mm = let state_list : E.t list = S.to_list g.states in let iter_body
     (i : int) (dupes1 : (string, int * int) Hashtbl.t) = let s1 : E.t =
     List.nth state_list i in let pstr1 : string = econstr_to_string s1 in let
     iter_inner (j : int) (dupes2 : (string, int * int) Hashtbl.t) = if
     Int.equal i j then return dupes2 else ( let s2 : E.t = List.nth state_list
     j in let pstr2 : string = E.to_string s2 in let* sigma = get_sigma in let
     are_eq : bool = EConstr.eq_constr sigma s1 s2 in let b_val : int = if
     are_eq then 1 else 0 in if String.equal pstr1 pstr2 then ( match
     Hashtbl.find_opt dupes2 pstr1 with | None -> Hashtbl.add dupes2 pstr1 (1,
     b_val) | Some (sum, b) -> Hashtbl.replace dupes2 pstr1 (sum + 1, b +
     b_val)); return dupes2) in iterate 0 (List.length state_list - 1) dupes1
     iter_inner in let* dupes = iterate 0 (List.length state_list - 1)
     (Hashtbl.create 0) iter_body in let num_dupes = Hashtbl.length dupes in if
     num_dupes > 0 then Log.warning ~params:default_params (Printf.sprintf
     "%sfound (%i) duplicate states: [%s]" prefix num_dupes (Hashtbl.fold (fun
     (k : string) ((v, b) : int * int) (acc : string) -> Printf.sprintf
     "%s\nduplicates (%i), eq_constr (%i):\n %s\n" acc v b k) dupes "")); return
     () ;; *)

  (* let _check_for_duplicate_transitions ?(prefix : string = "") ?(none :
     string option) (g : lts_graph) : unit mm = Log.override
     ~params:default_params "_check_for_duplicate_transitions, begin."; let* _ =
     if H.length g.transitions > 1 then ( let* flattened_transitions =
     flatten_transitions g.transitions in let _num_transitions = List.length
     flattened_transitions in Log.override ~params:default_params
     (Printf.sprintf "_check_for_duplicate_transitions, found (%i) to check."
     _num_transitions); let iter_dupe (i : int) ((acc, cache) : string list *
     (S.elt * Model_action.t * S.elt * Constr_tree.t) list) = if Int.equal
     ((i + 1) mod 25) 0 then Log.override ~params:default_params (Printf.sprintf
     "_check_for_duplicate_transitions, checking i: %i / %i." (i + 1)
     _num_transitions); let t1 = List.nth flattened_transitions i in let from1,
     a1, dest1, tree1 = t1 in let pstr_from1 = econstr_to_string from1 in let
     pstr_dest1 = econstr_to_string dest1 in let iter_dupe2 (j : int) (dupe_list
     : string list) = let t2 = List.nth flattened_transitions j in let from2,
     a2, dest2, tree2 = t2 in let pstr_from2 = econstr_to_string from2 in let
     pstr_dest2 = econstr_to_string dest2 in let from_eq = String.equal
     pstr_from1 pstr_from2 in let dest_eq = String.equal pstr_dest1 pstr_dest2
     in let a_eq = String.equal a1.label a2.label in let tree_eq =
     Constr_tree.eq tree1 tree2 in if from_eq && dest_eq && a_eq && tree_eq then
     return (Printf.sprintf "from: %s\nact: %s\ndest: %s\ntree: %s" pstr_from2
     a2.label pstr_dest2 (Constr_tree.pstr tree2) :: dupe_list) else return
     dupe_list in let* dupe_list = iterate (i + 1) (List.length
     flattened_transitions - 1) [] iter_dupe2 in if List.is_empty dupe_list then
     return (acc, t1 :: cache) else ( let dupe_str = Printf.sprintf "for
     transition:\n\ from:\n\ %s\n\ act:\n\ %s\n\ dest:\n\ %s\n\ tree: %s\n\n\
     found (%i) duplicates: [%s]" pstr_from1 a1.label pstr_dest1
     (Constr_tree.pstr tree1) (List.length dupe_list) (List.fold_left (fun (acc
     : string) (d : string) -> Printf.sprintf "%s\n%s\n" acc d) "" dupe_list) in
     return (dupe_str :: acc, cache)) in let* pstr_dupes, _ = iterate 0
     (List.length flattened_transitions - 1) ([], []) iter_dupe in (*
     Log.override ~params:default_params "_check_for_duplicate_transitions, post
     dupes."; *) if List.is_empty pstr_dupes then ( match none with | None -> ()
     | Some s -> Log.override ~params:default_params (Printf.sprintf "%s%s"
     prefix s)) else Log.warning ~params:default_params (Printf.sprintf
     "%sduplicates found: [%s]" prefix (List.fold_left (fun (acc : string) (d :
     string) -> Printf.sprintf "%s\n %s\n" acc d) "" pstr_dupes)); return ())
     else return () in Log.override ~params:default_params
     "_check_for_duplicate_transitions, end."; return () ;; *)

  (* let _check_transition_states ?(prefix : string = "") (g : lts_graph) : unit
     mm = Log.override ~params:default_params (Printf.sprintf
     "%s_check_transition_states, begin." prefix); H.iter (fun (from :
     EConstr.t) (actions : constr_transitions) -> (* check [from] is in
     [g.states] *) Hashtbl.iter (fun (a : Model_action.t) (destinations :
     D.t) -> D.iter (fun ((destination, _constr_tree) : D.elt) -> let
     is_from_missing : bool = Bool.not (S.mem from g.states) in let
     is_dest_missing : bool = Bool.not (S.mem destination g.states) in if Bool.(
     || ) is_from_missing is_dest_missing then Log.warning
     ~params:default_params (Printf.sprintf "%s_check_transition_states, in
     transition:\n\ %s\n\n\ did not find states: %s%s\n\ in states: %s\n\n\ %s"
     prefix (Printf.sprintf "from: %s\nlabel: %s\ndest: %s\nctor_tree: %s"
     (econstr_to_string from) a.label (econstr_to_string destination)
     (Constr_tree.pstr _constr_tree)) (if is_from_missing then Printf.sprintf
     "'from'" else "") (if is_dest_missing then Printf.sprintf "'dest'" else "")
     (_pstr_econstr_set g.states) (let singleton_matches : S.t = S.filter (fun
     (s : S.elt) -> Bool.( || ) (is_from_missing && S.mem from (S.singleton s))
     (is_dest_missing && S.mem destination (S.singleton s))) g.states in
     Printf.sprintf "found matches (%i) : %s\n" (S.cardinal singleton_matches)
     (_pstr_econstr_set singleton_matches)))) destinations) actions)
     g.transitions; Log.override ~params:default_params
     "_check_transition_states, end."; return () ;; *)

  (* let _run_all_checks ?(prefix : string = "") (g : lts_graph) : unit mm =
     let* _ = _check_for_duplicate_transitions ~prefix:"build_graph, " ~none:"No
     duplicates found" g in let* _ = _check_for_duplicate_states
     ~prefix:"build_graph, " g in let* _ = _check_transition_states
     ~prefix:"build_graph, " g in return () ;; *)

  (**********************************************************************)
  (******** above is a sanity check *************************************)
  (**********************************************************************)

  (** [insert_constr_transition] handles adding the mapping of action [a] to tuple [(term * Constr_tree.t)] in a given [constr_transitions].
  *)
  let insert_constr_transition
        (constrs : constr_transitions)
        (a : Model_action.t)
        (d : E.t)
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
        (t : E.t)
        (a : Model_action.t)
        (d : E.t)
        (c : Constr_tree.t)
    : unit mm
    =
    H.add
      g.transitions
      t
      (Hashtbl.of_seq (List.to_seq [ a, D.singleton (d, c) ]));
    return ()
  ;;

  let is_silent_transition (weak : E.t option) (act : EConstr.t)
    : bool option mm
    =
    match weak with
    | None ->
      (* Log.warning ~params:default_params "NO SILENT"; *)
      return None
    | Some w ->
      let* act_encoding = encode_opt act in
      (match act_encoding with
       | None ->
         (* let* w_term = decode w in
            let w_label = econstr_to_string w_term in
            let a_label = econstr_to_string act in
            Log.warning
            ~params:default_params
            (Printf.sprintf
            "is_silent_transition: (no decode) (str => %b)"
            (String.equal a_label w_label)); *)
         return (Some false)
       | Some e ->
         (* let* w_term = decode w in
            let w_label = econstr_to_string w_term in
            let a_label = econstr_to_string act in
            Log.warning
            ~params:default_params
            (Printf.sprintf
            "is_silent_transition: (enc => %b) (str => %b)"
            (E.eq w e)
            (String.equal a_label w_label)); *)
         return (Some (E.eq w e)))
  ;;

  let get_new_states
        ?(params : Params.log = default_params)
        ?(weak : E.t option = None)
        (t : E.t)
        (g : lts_graph)
        (ctors : coq_ctor list)
    : S.t mm
    =
    let iter_body (i : int) (new_states : S.t) =
      let (act, tgt, int_tree) : coq_ctor = List.nth ctors i in
      let* (tgt_enc : E.t) = encode tgt in
      let* (act_enc : E.t) = encode act in
      let* is_silent : bool option = is_silent_transition weak act in
      let to_add : Model_action.t =
        { label = act_enc, None; is_silent; info = None; annos = [] }
      in
      let* _ =
        match H.find_opt g.transitions t with
        | None -> add_new_term_constr_transition g t to_add tgt_enc int_tree
        | Some actions ->
          insert_constr_transition actions to_add tgt_enc int_tree
      in
      (* if [tgt] has not been explored then add [to_visit] *)
      if
        H.mem g.transitions tgt_enc
        (* || EConstr.eq_constr sigma tgt t *)
        || S.mem tgt_enc g.states
      then ()
      else Queue.push tgt_enc g.to_visit;
      (* add [tgt] to [new_states] *)
      return (S.add tgt_enc new_states)
    in
    iterate 0 (List.length ctors - 1) (S.singleton t) iter_body
  ;;

  (** [get_new_constrs t rlts_map] returns the list of constructors applicable to term [t], using those provided in [rlts_map].
      If no immediate constructor is found matching [t] in [rlts_map] (likely due to unification problems), then each constructor in [rlts_map] is tried sequentially, until one of them returns some valid constructors.
      @raise CannotFindTypeOfTermToVisit
        if none of the constructors provided in [rlts_map] yield constructors from [check_valid_constructors].
  *)
  let get_new_constrs
        ?(params : Params.log = default_params)
        (encoded_t : E.t)
        (primary : cindef)
        (rlts_map : cindef B.t)
    : coq_ctor list mm
    =
    let* (t : EConstr.t) = decode encoded_t in
    let* (decoded_map : cindef F.t) = decode_map rlts_map in
    let* primary_constr_transitions = lts_cindef_constr_transitions primary in
    check_valid_constructor
      ~params
      primary_constr_transitions
      decoded_map
      t
      None
      primary.index
  ;;

  (** [build_lts_graph fn_rlts g bound] is an [lts_graph] [g] obtained by exploring [fn_rlts].
      @param fn_rlts maps coq-term names to [cindef].
      @param g is an [lts_graph] accumulated while exploring [rlts].
      @param bound is the number of states to explore until.
      @return an [lts_graph] with a maximum of [bound] many states. *)
  let rec build_lts_graph
            ?(params : Params.log = default_params)
            ?(weak : E.t option = None)
            (the_primary_lts : cindef)
            (rlts_map : cindef B.t)
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
      let encoded_t : E.t = Queue.pop g.to_visit in
      let* (new_constrs : coq_ctor list) =
        get_new_constrs ~params encoded_t the_primary_lts rlts_map
      in
      (* [get_new_states] also updates [g.to_visit] *)
      let* (new_states : S.t) =
        get_new_states ~params ~weak encoded_t g new_constrs
      in
      let g : lts_graph = { g with states = S.union g.states new_states } in
      (* let* _ = _run_all_checks ~prefix:"build_graph, " g in *)
      build_lts_graph ~params ~weak the_primary_lts rlts_map g bound)
  ;;

  let check_for_primary_lts
        (c : cindef)
        (ty : EConstr.t)
        ((fn_opt, cindef_map) : E.t option * cindef B.t)
    : (E.t option * cindef B.t) mm
    =
    (* normalize term type and check if primary *)
    let* term_type = lts_cindef_trm_type c in
    let* (trmty : EConstr.t) = normalize_econstr term_type in
    let* sigma = get_sigma in
    match fn_opt, EConstr.eq_constr sigma ty trmty with
    | None, true ->
      let* (encoding : E.t) = encode c.info.name in
      return (Some encoding, cindef_map)
    | Some _, true ->
      Log.warning
        ~params:default_params
        (Printf.sprintf
           "Found inductive definition that could be primary, when primary has \
            already been selected.\n\
            Please make sure that the primary LTS is provided first when using \
            the command.");
      return (fn_opt, cindef_map)
    | _, false -> return (fn_opt, cindef_map)
  ;;

  let handle_weak_lts
        (weak : Vernac.weak_params option)
        (cindef_map : cindef B.t)
        (i : int)
    : E.t option mm
    =
    match weak with
    | None ->
      (* Log.override "handle_weak_lts NO WEAK"; *)
      return None
    | Some (a_ref, constr_ref) ->
      let* (a : EConstr.t) = tref_to_econstr a_ref in
      let constr = Mebi_utils.ref_to_glob constr_ref in
      let* (c : cindef) = get_type_cindef i constr (Some a) in
      (* encode silent action type *)
      let* (constr_enc : E.t) = encode c.info.name in
      B.add cindef_map constr_enc c;
      (* return encode silent action *)
      let* (a_enc : E.t) = encode a in
      B.add cindef_map a_enc c;
      return (Some a_enc)
  ;;

  let resolve_primary_lts
        ?(primary_lts : Libnames.qualid option = None)
        (primary_enc_opt : E.t option)
        (ty : EConstr.t)
        (cindef_map : cindef B.t)
        (i : int)
    : E.t mm
    =
    match primary_lts with
    | None ->
      (* if no primary explicitly provided, check we found one. *)
      (match primary_enc_opt with
       | None ->
         let* decoded_map = decode_map cindef_map in
         primary_lts_not_found (ty, List.of_seq (F.to_seq_keys decoded_map))
       | Some p -> return p)
    | Some primary_lts ->
      (* obtain the encoding for the explicitly provide primary lts *)
      let* (c : cindef) =
        get_lts_cindef i (Mebi_utils.ref_to_glob primary_lts)
      in
      let* (encoding : E.t) = encode c.info.name in
      return encoding
  ;;

  (** @return
        the key for the primary lts and hashtable mapping the name of the coq definition to the rlts.
  *)
  let build_cindef_map
        ?(primary_lts : Libnames.qualid option = None)
        ?(weak : Vernac.weak_params option = None)
        (t' : EConstr.t)
        (grefs : Names.GlobRef.t list)
    : (E.t * E.t option * cindef B.t) mm
    =
    (* normalize the initial term *)
    let* (t : EConstr.t) = normalize_econstr t' in
    let* (ty : EConstr.t) = type_of_econstr t in
    (* prepare for iterating through [grefs] *)
    let num_grefs : int = List.length grefs in
    let trmap : cindef B.t = B.create num_grefs in
    let iter_body (i : int) ((fn_opt, acc_map) : E.t option * cindef B.t) =
      let gref : Names.GlobRef.t = List.nth grefs i in
      let* (c : cindef) = get_lts_cindef i gref in
      (* add name of inductive prop *)
      let* (encoding : E.t) = encode c.info.name in
      B.add acc_map encoding c;
      (* check if primary lts *)
      if Option.has_some primary_lts
      then return (fn_opt, acc_map)
      else check_for_primary_lts c ty (fn_opt, acc_map)
    in
    let* (primary_enc_opt, cindef_map) : E.t option * cindef B.t =
      iterate 0 (num_grefs - 1) (None, trmap) iter_body
    in
    let* (the_primary_enc : E.t) =
      resolve_primary_lts ~primary_lts primary_enc_opt ty cindef_map num_grefs
    in
    let* weak_enc_opt : E.t option =
      handle_weak_lts weak cindef_map (num_grefs + 1)
    in
    return (the_primary_enc, weak_enc_opt, cindef_map)
  ;;

  (** [build_graph rlts_map fn_rlts tref bound] is the entry point for [build_lts_graph].
      @param rlts_map maps coq-term types to [cindef].
      @param tref is the original coq-term.
      @param bound is the number of states to explore until. *)
  let build_graph
        ?(params : Params.log = default_params)
        ?(primary_lts : Libnames.qualid option = None)
        ?(weak : Vernac.weak_params option = None)
        (bound : int)
        (tref : Constrexpr.constr_expr)
        (grefs : Names.GlobRef.t list)
    : lts_graph mm
    =
    (* make map of term types *)
    let* (t : EConstr.t) = tref_to_econstr tref in
    let* (the_primary_enc, the_weak_opt_enc, cindef_map)
      : E.t * E.t option * cindef B.t
      =
      build_cindef_map ~primary_lts ~weak t grefs
    in
    (* let* _ =
      _log_cindef_map ~params the_primary_enc the_weak_opt_enc cindef_map
    in *)
    (* update environment by typechecking *)
    let (the_primary_lts : cindef) = B.find cindef_map the_primary_enc in
    let* primary_trm_type = lts_cindef_trm_type the_primary_lts in
    let$* u env sigma = Typing.check env sigma t primary_trm_type in
    let$ init env sigma = sigma, Reductionops.nf_all env sigma t in
    let* encoded_init = encode init in
    let q = Queue.create () in
    let* _ = return (Queue.push encoded_init q) in
    let* g =
      build_lts_graph
        ~params
        ~weak:the_weak_opt_enc
        the_primary_lts
        cindef_map
        { to_visit = q
        ; states = S.empty
        ; init = encoded_init
        ; transitions = H.create 0
        }
        bound
    in
    let* _ = _print_finished_build_graph ~params g the_primary_enc in
    (* let* _ = _run_all_checks ~prefix:"build_graph, " g in *)
    return g
  ;;

  let decoq_lts ?(name : string = "unnamed") (bound : int) (g : lts_graph)
    : Lts.t mm
    =
    let info : Utils.model_info option =
      Some
        { is_complete = Queue.is_empty g.to_visit
        ; bound
        ; num_states = S.cardinal g.states
        ; num_edges = num_transitions g.transitions
        }
    in
    let* init_decoding = decode g.init in
    let init : Model_state.t option =
      Some (g.init, Some (econstr_to_string init_decoding))
    in
    let* states : Model.States.t =
      S.fold
        (fun (s : E.t) (acc : Model.States.t mm) ->
          let* acc = acc in
          let* s_decoding = decode s in
          return (Model.States.add (s, Some (econstr_to_string s_decoding)) acc))
        g.states
        (return Model.States.empty)
    in
    let* transitions : Model.Transitions.t =
      H.fold
        (fun (from : E.t)
          (ts : constr_transitions)
          (acc0 : Model.Transitions.t mm) ->
          let* from_decoding = decode from in
          let from_str = econstr_to_string from_decoding in
          Hashtbl.fold
            (fun (a : Model_action.t)
              (dests : D.t)
              (acc1 : Model.Transitions.t mm) ->
              let* a_decoding = decode (fst a.label) in
              let a_str = econstr_to_string a_decoding in
              D.fold
                (fun ((dest, tree) : E.t * Constr_tree.t)
                  (acc2 : Model.Transitions.t mm) ->
                  let* acc2 = acc2 in
                  let* dest_decoding = decode dest in
                  let from = from, Some from_str in
                  let dest = dest, Some (econstr_to_string dest_decoding) in
                  let act = fst a.label, Some a_str in
                  return
                    (Model.Transitions.add
                       ( from
                       , act
                       , dest
                       , Some
                           { is_silent = a.is_silent
                           ; info = Some (Constr_tree.pstr tree)
                           } )
                       acc2))
                dests
                acc1)
            ts
            acc0)
        g.transitions
        (return Model.Transitions.empty)
    in
    let alphabet = Model.alphabet_from_transitions transitions in
    return (Lts.create init alphabet states transitions info)
  ;;
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  let* h = make_transition_tbl in
  (* hashtabl of terms to (edges) or (cindef) *)
  let* s = make_state_set in
  (* set of states (econstr term) *)
  let* d = make_state_tree_pair_set in
  (* hashtabl mapping term type or cindef *)
  let module G : GraphB = MkGraph ((val h)) ((val s)) ((val d)) in
  return (module G : GraphB)
;;

let _print_incomplete_lts_warning
      ?(params : Params.log = default_params)
      (name : string)
      (bound : int)
  : unit
  =
  Log.warning
    ~params
    (Printf.sprintf
       "LTS graph (%s) is incomplete, exceeded bound: %i.\n"
       name
       bound)
;;

let _print_complete_lts_notice
      ?(params : Params.log = default_params)
      (name : string)
  : unit
  =
  Log.override
    ~params
    (Printf.sprintf "- - - - (finished build lts of: %s)" name)
;;

(**********************)
(** Entry point *******)
(**********************)

open Vernac

let result_to_string (r : result_kind) : string =
  match r with
  | LTS the_lts -> Printf.sprintf "LTS: %s" (Lts.pstr the_lts)
  | FSM the_fsm -> Printf.sprintf "FSM: %s" (Fsm.pstr the_fsm)
  | Bisim
      ( ((the_fsm_1, the_fsm_2), the_merged_fsm)
      , (bisim_states, non_bisim_states) ) ->
    Printf.sprintf
      "Bisimilar: %b\n\n\
       Bisimilar States (%i): %s\n\
       Non-Bisimilar States (%i): %s\n\n\
       FSM merged: %s\n\n\
       FSM A: %s\n\n\
       FSM B: %s"
      (Model.Partition.is_empty non_bisim_states)
      (Model.Partition.cardinal bisim_states)
      (Model.pstr_partition bisim_states)
      (Model.Partition.cardinal non_bisim_states)
      (Model.pstr_partition non_bisim_states)
      (Fsm.pstr the_merged_fsm)
      (Fsm.pstr the_fsm_1)
      (Fsm.pstr the_fsm_2)
  | _ -> "TODO: finish handle output"
;;

let handle_output (o : output_kind) (r : result_kind) : unit mm =
  match o with
  | Check () -> return ()
  | Show () ->
    let s : string = result_to_string r in
    Log.normal ~params:default_params s;
    return ()
  | Dump name_opt ->
    let output_path : string =
      Dump_to_file.write_to_file (Default ()) (name_opt, Auto ()) (JSON ()) r
    in
    Log.normal
      ~params:default_params
      (Printf.sprintf "dumped file to: '%s'" output_path);
    return ()
;;

(* exception ExceededBoundBeforeLTSCompleted of int *)
exception WeakBisimilarityRequiresSilentConstrForEachTerm of unit

let vernac (o : output_kind) (r : run_params) : unit mm =
  let* (graphM : (module GraphB)) = make_graph_builder in
  let module G = (val graphM) in
  let build_lts_graph
        ?(primary_lts : Libnames.qualid option = None)
        ?(weak : weak_params option = None)
        (fail_if_incomplete : bool)
        (bound : int)
        (t : Constrexpr.constr_expr)
        (l : Libnames.qualid list)
    : Lts.t mm
    =
    let lts_grefs : Names.GlobRef.t list = Mebi_utils.ref_list_to_glob_list l in
    let* graph_lts = G.build_graph ~primary_lts ~weak bound t lts_grefs in
    let the_lts = G.decoq_lts bound graph_lts in
    the_lts
    (* let* the_lts_translation = G.DeCoq.lts_graph bound graph_lts in
       match
       fail_if_incomplete, Utils.is_complete (fst the_lts_translation).info
       with
       | true, Some false -> raise (ExceededBoundBeforeLTSCompleted bound)
       | _, _ -> return the_lts_translation *)
  in
  match r with
  | LTS ((f, b, t), w), l ->
    let* the_lts = build_lts_graph ~weak:w f b t l in
    handle_output o (LTS the_lts)
  | FSM ((f, b, t), w), l ->
    let* the_lts = build_lts_graph ~weak:w f b t l in
    let the_fsm = Translate.lts_to_fsm the_lts in
    handle_output o (FSM the_fsm)
  | Minim ((f, b, t), w), l -> return ()
  | Merge ((((f1, b1, t1), w1), p1), (((f2, b2, t2), w2), p2)), l ->
    let* the_lts_1 = build_lts_graph ~primary_lts:(Some p1) f1 b1 t1 l in
    let the_fsm_1 = Translate.lts_to_fsm the_lts_1 in
    let* the_lts_2 = build_lts_graph ~primary_lts:(Some p2) f2 b2 t2 l in
    let the_fsm_2 = Translate.lts_to_fsm the_lts_2 in
    let the_merged_fsm = Fsm.merge (the_fsm_1, the_fsm_2) in
    handle_output o (Merge ((the_fsm_1, the_fsm_2), the_merged_fsm))
  | Bisim ((((f1, b1, t1), w1), p1), (((f2, b2, t2), w2), p2)), l ->
    let weak =
      match w1, w2 with
      | None, None -> false
      | Some _, Some _ -> true
      | _, _ -> raise (WeakBisimilarityRequiresSilentConstrForEachTerm ())
    in
    let* the_lts_1 =
      build_lts_graph ~primary_lts:(Some p1) ~weak:w1 f1 b1 t1 l
    in
    let the_fsm_1 = Translate.lts_to_fsm the_lts_1 in
    let* the_lts_2 =
      build_lts_graph ~primary_lts:(Some p2) ~weak:w2 f2 b2 t2 l
    in
    let the_fsm_2 = Translate.lts_to_fsm the_lts_2 in
    let the_bisim_result =
      Bisimilarity.run ~weak ((the_fsm_1, the_fsm_2), None)
    in
    handle_output o (Bisim the_bisim_result)
;;
