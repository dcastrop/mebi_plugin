(* open Pp *)

(* open Mebi_utils *)
(* open Mebi_monad *)
(* open Mebi_monad.Monad_syntax *)

open Mebi_wrapper
open Logging

(** [bound] is the total number of states to be allowed when building an LTS. *)
let default_bound : int = 10

let bound : int ref = ref default_bound

let get_bound () : unit mm =
  Log.override
    (Printf.sprintf
       "Bound set to: %i.%s"
       !bound
       (if !bound = default_bound then " (default)" else ""));
  return ()
;;

let set_bound (b : int) : unit mm =
  bound := b;
  Log.override
    (Printf.sprintf
       "Set bound to: %i.%s"
       !bound
       (if !bound = default_bound then " (default)" else ""));
  return ()
;;

(** *)
let dump_to_file_flag : bool ref = ref true

let get_dump_to_file_flag () : unit mm =
  Log.notice
    (if !dump_to_file_flag
     then "File dumps enabled."
     else "File dumps disabled.");
  return ()
;;

let set_dump_to_file_flag (b : bool) : unit mm =
  dump_to_file_flag := b;
  Log.notice
    (if !dump_to_file_flag
     then "Enabled File dumps."
     else "Disabled File dumps.");
  return ()
;;

(** *)
let get_show_debug_flag () : unit mm = return Logging.get_show_debug_messages

let set_show_debug_flag (b : bool) : unit mm =
  return (Logging.set_show_debug_messages b)
;;

(** *)
let weak_mode : bool ref = ref true

let get_weak_mode () : unit mm =
  Log.notice
    (Printf.sprintf
       "Currently in %s mode."
       (if !weak_mode then "weak" else "strong"));
  return ()
;;

let set_weak_mode (b : bool) : unit mm =
  weak_mode := b;
  Log.notice
    (Printf.sprintf "Now in %s mode." (if !weak_mode then "weak" else "strong"));
  return ()
;;

(** *)
type weak_action_kinds =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

let weak_type : weak_action_kinds option ref = ref None

let get_weak_type () : unit mm =
  (match !weak_type with
   | None -> Log.notice "Weak type not set. (Is None)"
   | Some o ->
     (match o with
      | Option c ->
        Log.notice
          "Weak type is an Option of \"\" (where silent actions are None)"
      | Custom (x, y) ->
        Log.notice
          "Weak type is a Custom type of \"TODO\" where silent actions are \
           \"TODO\""));
  return ()
;;

let set_weak_type (k : weak_action_kinds) : unit mm =
  weak_type := Some k;
  (match k with
   | Option c ->
     Log.notice
       "Set weak type to an Option of \"\" (where silent actions are None)"
   | Custom (x, y) ->
     Log.notice
       "Set weak type to a Custom type of \"TODO\" where silent actions are \
        \"TODO\"");
  return ()
;;

open Mebi_wrapper.Syntax
open Model

(* *)
open Logging

(* open Utils.Formatting *)
open Utils
(* open Fsm *)

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

(** coq_inductive_lts *)
type cind_lts =
  { trm_type : EConstr.t
  ; _lbl_type : EConstr.t
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

let lts_cindef_constr_transitions (c : cindef)
  : (Constr.rel_context * Constr.t) array mm
  =
  match c.kind with
  | LTS l -> return l.constr_transitions
  | _ -> invalid_cindef_kind ()
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
  | _ -> invalid_ref_type gref
;;

(** [check_ref_lts gref] is the [cindef] of [gref].

    @raise invalid_ref_lts if [gref] is not a reference to an inductive type. *)
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
        ; _lbl_type = EConstr.of_constr (Context.Rel.Declaration.get_type lbl)
        ; constr_transitions = mip.mind_nf_lc
        } )
  (* raise error if [gref] is not an inductive type *)
  | _ -> invalid_ref_lts gref
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
let m_unify (t0 : EConstr.t) (t1 : EConstr.t) : bool mm =
  (* let* _ = if is_output_kind_enabled params then debug (fun (env :
     Environ.env) (sigma : Evd.evar_map) -> str "Unifying (t0) :: " ++
     Printer.pr_econstr_env env sigma t0 ++ strbrk "\nUnifying (t1) :: " ++
     Printer.pr_econstr_env env sigma t1) else return () in *)
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      (* log  "\t\tSuccess"; *)
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify (m, n, e))
      ->
      Log.warning "\t\tCould not unify";
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
let extract_args (substl : EConstr.Vars.substl) (tm : Constr.t)
  : (term * term * term) mm
  =
  match Constr.kind tm with
  | App (_, args) ->
    if Array.length args == 3
    then (
      (* assert (Array.length args == 3); *)
      let args = EConstr.of_constr_array args in
      let args = Array.map (EConstr.Vars.substl substl) args in
      return (args.(0), args.(1), args.(2)))
    else invalid_lts_args_length (Array.length args)
  | _ -> (* assert false *) invalid_lts_term_kind tm
;;

type unif_problem =
  { termL : EConstr.t
  ; termR : EConstr.t
  }

let rec unify_all (i : (Constr_tree.t * unif_problem) list)
  : Constr_tree.t list option mm
  =
  match i with
  | [] -> return (Some [])
  | (ctor_tree, u) :: t ->
    (* let* _ = if is_output_kind_enabled params then debug (fun env sigma ->
       str "UNIFYALL (termL) :::::::::: " ++ Printer.pr_econstr_env env sigma
       u.termL ++ strbrk "\nUNIFYALL (termR) :::::::::: " ++
       Printer.pr_econstr_env env sigma u.termR) else return () in *)
    let* success = m_unify u.termL u.termR in
    if success
    then
      let* unified = unify_all t in
      match unified with
      | None -> return None
      | Some unified -> return (Some (ctor_tree :: unified))
    else return None
;;

let sandboxed_unify
      (tgt_term : EConstr.t)
      (u : (Constr_tree.t * unif_problem) list)
  : (EConstr.t * Constr_tree.t list) option mm
  =
  (* let* _ = if is_output_kind_enabled params then debug (fun env sigma -> str
     "TGT:::::: " ++ Printer.pr_econstr_env env sigma tgt_term) else return ()
     in *)
  sandbox
    (let* success = unify_all u in
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
          (acc : coq_ctor list)
          (i : int)
          (act : EConstr.t)
          (tgt_term : EConstr.t)
  : int * (Constr_tree.t * unif_problem) list list -> coq_ctor list mm
  = function
  | _, [] -> return acc
  | lts_index, u1 :: nctors ->
    let* success = sandboxed_unify tgt_term u1 in
    (match success with
     | None -> retrieve_tgt_nodes acc i act tgt_term (lts_index, nctors)
     | Some (tgt, ctor_tree) ->
       let$+ act env sigma = Reductionops.nf_all env sigma act in
       retrieve_tgt_nodes
         ((act, tgt, Node ((E.of_int lts_index, i), ctor_tree)) :: acc)
         i
         act
         tgt_term
         (lts_index, nctors))
;;

(* Should return a list of unification problems *)
let rec check_updated_ctx
          (acc : int * (Constr_tree.t * unif_problem) list list)
          (fn_cindef : cindef F.t)
  :  EConstr.t list * EConstr.rel_declaration list
  -> (int * (Constr_tree.t * unif_problem) list list) option mm
  = function
  | [], [] -> return (Some acc)
  | _hsubstl :: substl, t :: tl ->
    (* Log.warning
      
      (Printf.sprintf
         "G: _hsubstl :: substl,\n_hsubstl = %s\nsubstl = %s"
         (econstr_to_string _hsubstl)
         (econstr_list_to_string substl)); *)
    (* Log.warning
      
      (Printf.sprintf
         "H: t :: tl,\nt = %s\ntl = %s"
         (econstr_rel_decl_to_string t)
         (econstr_rel_decl_list_to_string tl)); *)
    let$+ upd_t env sigma =
      EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t)
    in
    (* Log.warning

       (Printf.sprintf "I: upd_t = %s" (econstr_to_string upd_t)); *)
    let* sigma = get_sigma in
    (match EConstr.kind sigma upd_t with
     | App (fn, args) ->
       (match F.find_opt fn_cindef fn with
        | None ->
          (* NOTE: testing handling the [@eq] premises *)
          (match econstr_to_string fn with
           | "option" ->
             Log.warning
               (Printf.sprintf
                  "check_updated_ctx, fn_cindef \"option\" has args: %s"
                  (econstr_list_to_string (Array.to_list args)));
             check_updated_ctx acc fn_cindef (substl, tl)
           | "@eq" ->
             (* Log.warning
               
               (Printf.sprintf
                  "N: acc (%i): [%s]"
                  (fst acc)
                  (List.fold_left
                     (fun (acc : string)
                       (nctor : (Constr_tree.t * unif_problem) list) ->
                       if List.is_empty nctor
                       then "[]"
                       else
                         List.fold_left
                           (fun (acc2 : string)
                             (ctor : Constr_tree.t * unif_problem) ->
                             Printf.sprintf
                               "%s\n( tree: %s\n; {| termL: %s\n   ; termR: %s)"
                               acc2
                               (Constr_tree.pstr (fst ctor))
                               (econstr_to_string (snd ctor).termL)
                               (econstr_to_string (snd ctor).termR))
                           acc
                           nctor)
                     ""
                     (snd acc))); *)
             let* (lhs : EConstr.t) = normalize_econstr args.(1) in
             let* (rhs : EConstr.t) = normalize_econstr args.(2) in
             Log.warning
               (Printf.sprintf
                  "TODO: check_updated_ctx, handle premise (normalized):\n\
                   fn: %s\n\
                   lhs: %s\n\
                   rhs: %s"
                  (econstr_to_string fn)
                  (econstr_to_string lhs)
                  (econstr_to_string rhs));
             (* TODO: find way to propagate this *)
             let* (to_subst :
                    (int * (Constr_tree.t * unif_problem) list list) option)
               =
               check_updated_ctx acc fn_cindef (substl, tl)
             in
             return to_subst
             (* NOTE: below trying *)
             (* (match to_subst with
              | None -> return to_subst
              | Some to_subst' ->
                return
                  (Some
                     ( fst to_subst'
                     , List.concat_map
                         (fun (x : (Constr_tree.t * unif_problem) list) ->
                           (* x :: [] *)
                           List.map
                             (fun (y : Constr_tree.t * unif_problem) ->
                               (fst y, { termL = lhs; termR = rhs }) :: x)
                             x)
                         (snd to_subst') ))) *)
           | _ ->
             Log.warning
               (Printf.sprintf
                  "check_updated_ctx, fn_cindef does not have corresponding fn \
                   \"%s\" with args: %s."
                  (econstr_to_string fn)
                  (econstr_list_to_string (Array.to_list args)));
             check_updated_ctx acc fn_cindef (substl, tl))
        | Some c ->
          let$+ nextT env sigma = Reductionops.nf_evar sigma args.(0) in
          let* c_constr_transitions = lts_cindef_constr_transitions c in
          let* (ctors : coq_ctor list) =
            check_valid_constructor
              c_constr_transitions
              fn_cindef
              nextT
              (Some args.(1))
              c.index
          in
          if List.is_empty ctors
          then return None
          else (
            let ctree_unif_probs : (Constr_tree.t * unif_problem) list =
              List.map
                (fun (_, (tL : EConstr.t), (i : Constr_tree.t)) ->
                  i, { termL = tL; termR = args.(2) })
                ctors
            in
            (* Log.warning
              
              (Printf.sprintf
                 "K: acc: [%s]"
                 (List.fold_left
                    (fun (acc : string)
                      (nctor : (Constr_tree.t * unif_problem) list) ->
                      if List.is_empty nctor
                      then "[]"
                      else
                        List.fold_left
                          (fun (acc2 : string)
                            (ctor : Constr_tree.t * unif_problem) ->
                            Printf.sprintf
                              "%s\n( tree: %s\n; {| termL: %s\n   ; termR: %s)"
                              acc2
                              (Constr_tree.pstr (fst ctor))
                              (econstr_to_string (snd ctor).termL)
                              (econstr_to_string (snd ctor).termR))
                          acc
                          nctor)
                    ""
                    (snd acc))); *)
            (* Log.warning
              
              (Printf.sprintf
                 "L: ctree_unif_probs: [%s]"
                 (List.fold_left
                    (fun (acc2 : string)
                      (ctor : Constr_tree.t * unif_problem) ->
                      Printf.sprintf
                        "%s\n( tree: %s\n; {| termL: %s\n   ; termR: %s)"
                        acc2
                        (Constr_tree.pstr (fst ctor))
                        (econstr_to_string (snd ctor).termL)
                        (econstr_to_string (snd ctor).termR))
                    ""
                    ctree_unif_probs)); *)
            (* We need to cross-product all possible unifications. This is in
               case we have a constructor of the form LTS t11 a1 t12 -> LTS t21
               a2 t22 -> ... -> LTS tn an t2n. Repetition may occur. It is not
               unavoidable, but we should make sure we understand well the
               problem before removing the source of repetition. *)
            (* FIXME: Test this *)
            (* replace [rtls] in [rtls_ctx] *)
            (* let rtls_ctx':rlts_list = List.append [ ] in *)
            let acc' : (Constr_tree.t * unif_problem) list list =
              List.concat_map
                (fun (x : (Constr_tree.t * unif_problem) list) ->
                  List.map
                    (fun (y : Constr_tree.t * unif_problem) -> y :: x)
                    ctree_unif_probs)
                (snd acc)
            in
            (* Log.warning
              
              (Printf.sprintf
                 "M: acc': [%s]"
                 (List.fold_left
                    (fun (acc : string)
                      (nctor : (Constr_tree.t * unif_problem) list) ->
                      if List.is_empty nctor
                      then "[]"
                      else
                        List.fold_left
                          (fun (acc2 : string)
                            (ctor : Constr_tree.t * unif_problem) ->
                            Printf.sprintf
                              "%s\n( tree: %s\n; {| termL: %s\n   ; termR: %s)"
                              acc2
                              (Constr_tree.pstr (fst ctor))
                              (econstr_to_string (snd ctor).termL)
                              (econstr_to_string (snd ctor).termR))
                          acc
                          nctor)
                    ""
                    acc')); *)
            check_updated_ctx (fst acc, acc') fn_cindef (substl, tl)))
     | _ -> check_updated_ctx acc fn_cindef (substl, tl))
  | _substl, _ctxl -> invalid_check_updated_ctx _substl _ctxl
(* Impossible! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

(** Checks possible transitions for this term: *)
and check_valid_constructor
      (ctor_transitions : (Constr.rel_context * Constr.types) array)
      (fn_cindef : cindef F.t)
      (t' : EConstr.t)
      (ma : EConstr.t option)
      (lts_index : int)
  : coq_ctor list mm
  =
  (* let$+ t env sigma = Reductionops.nf_all env sigma t' in *)
  let* (t : EConstr.t) = normalize_econstr t' in
  (* Log.warning  (Printf.sprintf "A: %s" (econstr_to_string t)); *)
  let iter_body (i : int) (ctor_vals : coq_ctor list) =
    (* Log.warning  (Printf.sprintf "B (%i): %s" i (econstr_to_string t)); *)
    let (ctx, tm) : Constr.rel_context * Constr.t = ctor_transitions.(i) in
    let ctx_tys : EConstr.rel_declaration list =
      List.map EConstr.of_rel_decl ctx
    in
    let* substl = mk_ctx_substl [] (List.rev ctx_tys) in
    let* (termL, act, termR) : Evd.econstr * Evd.econstr * Evd.econstr =
      extract_args substl tm
    in
    (* Log.warning
      
      (Printf.sprintf "C (%i): substl = %s" i (econstr_list_to_string substl)); *)
    (* Log.warning
      
      (Printf.sprintf
         "D (%i): tl = %s"
         i
         (econstr_rel_decl_list_to_string ctx_tys)); *)
    let* success = m_unify t termL in
    if success
    then
      (* Log.warning

         (Printf.sprintf
         "E (%i): (%s) U (%s)"
         i
         (econstr_to_string t)
         (econstr_to_string termL)); *)
      let* success = Option.cata (fun a -> m_unify a act) (return true) ma in
      if success
      then
        (* Log.warning  (Printf.sprintf "F (%i): successs" i); *)
        let* (act : EConstr.t) = normalize_econstr act in
        let tgt_term : EConstr.t = EConstr.Vars.substl substl termR in
        let* (next_ctors :
               (int * (Constr_tree.t * unif_problem) list list) option)
          =
          check_updated_ctx (lts_index, [ [] ]) fn_cindef (substl, ctx_tys)
        in
        match next_ctors with
        | None -> return ctor_vals
        | Some index_ctor_pair ->
          (match snd index_ctor_pair with
           | [] ->
             (* Log.warning

                (Printf.sprintf "J (%i): snd next_ctors is empty" i); *)
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
             (* Log.warning
               
               (Printf.sprintf
                  "J (%i): snd next_ctors is non-empty: [%s]"
                  i
                  (List.fold_left
                     (fun (acc : string)
                       (nctor : (Constr_tree.t * unif_problem) list) ->
                       if List.is_empty nctor
                       then "[]"
                       else
                         List.fold_left
                           (fun (acc2 : string)
                             (ctor : Constr_tree.t * unif_problem) ->
                             Printf.sprintf
                               "%s\n( tree: %s\n; {| termL: %s\n   ; termR: %s)"
                               acc2
                               (Constr_tree.pstr (fst ctor))
                               (econstr_to_string (snd ctor).termL)
                               (econstr_to_string (snd ctor).termR))
                           acc
                           nctor)
                     ""
                     nctors)); *)
             let tgt_nodes =
               retrieve_tgt_nodes ctor_vals i act tgt_term index_ctor_pair
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

  type constr_transitions = (Action.t, D.t) Hashtbl.t

  type lts_graph =
    { to_visit : E.t Queue.t
    ; init : E.t
    ; terminals : S.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    ; cindefs : (E.t * cindef) list
    }

  val insert_constr_transition
    :  constr_transitions
    -> Action.t
    -> E.t
    -> Constr_tree.t
    -> unit mm

  val add_new_term_constr_transition
    :  lts_graph
    -> E.t
    -> Action.t
    -> E.t
    -> Constr_tree.t
    -> unit mm

  val build_lts_graph
    :  ?weak:E.t option
    -> cindef
    -> cindef B.t
    -> lts_graph
    -> int
    -> lts_graph mm

  val build_graph
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Names.GlobRef.t list
    -> lts_graph mm

  val decoq_lts
    :  ?cache_decoding:bool
    -> ?name:string
    -> lts_graph (* -> cindef * cindef B.t *)
    -> Lts.t mm
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
  type constr_transitions = (Action.t, D.t) Hashtbl.t

  let num_transitions (ts : constr_transitions H.t) : int =
    H.fold
      (fun (_from : E.t) (transitions : constr_transitions) (acc : int) ->
        Hashtbl.fold
          (fun (_a : Action.t) (destinations : D.t) (acc' : int) ->
            acc' + D.cardinal destinations)
          transitions
          acc)
      ts
      0
  ;;

  (** [lts_graph] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Constr_tree.t].
  *)
  type lts_graph =
    { to_visit : E.t Queue.t
    ; init : E.t
    ; terminals : S.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    ; cindefs : (E.t * cindef) list
    }

  (** [insert_constr_transition] handles adding the mapping of action [a] to tuple [(term * Constr_tree.t)] in a given [constr_transitions].
  *)
  let insert_constr_transition
        (constrs : constr_transitions)
        (a : Action.t)
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
        (a : Action.t)
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
    | None -> return None
    | Some w ->
      let* act_encoding = encode_opt act in
      (match act_encoding with
       | None -> return (Some false)
       | Some e -> return (Some (E.eq w e)))
  ;;

  let get_new_states
        ?(weak : E.t option = None)
        (from : E.t)
        (g : lts_graph)
        (ctors : coq_ctor list)
    : S.t mm
    =
    let iter_body (i : int) (new_states : S.t) =
      let (act, tgt, int_tree) : coq_ctor = List.nth ctors i in
      let* (tgt_enc : E.t) = encode tgt in
      let* (act_enc : E.t) = encode act in
      let* is_silent : bool option = is_silent_transition weak act in
      let meta : Action.MetaData.t = [ Constr_tree.pstr int_tree ] in
      let to_add : Action.t =
        { label = act_enc, (None, is_silent); meta; annos = [] }
      in
      let* _ =
        match H.find_opt g.transitions from with
        | None -> add_new_term_constr_transition g from to_add tgt_enc int_tree
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
    iterate 0 (List.length ctors - 1) (S.singleton from) iter_body
  ;;

  (** [get_new_constrs t rlts_map] returns the list of constructors applicable to term [t], using those provided in [rlts_map].
      If no immediate constructor is found matching [t] in [rlts_map] (likely due to unification problems), then each constructor in [rlts_map] is tried sequentially, until one of them returns some valid constructors.
      @raise CannotFindTypeOfTermToVisit
        if none of the constructors provided in [rlts_map] yield constructors from [check_valid_constructors].
  *)
  let get_new_constrs (from : E.t) (primary : cindef) (rlts_map : cindef B.t)
    : coq_ctor list mm
    =
    let* (from_dec : EConstr.t) = decode from in
    let* (decoded_map : cindef F.t) = decode_map rlts_map in
    let* primary_constr_transitions = lts_cindef_constr_transitions primary in
    check_valid_constructor
      primary_constr_transitions
      decoded_map
      from_dec
      None
      primary.index
  ;;

  (** [build_lts_graph fn_rlts g bound] is an [lts_graph] [g] obtained by exploring [fn_rlts].
      @param fn_rlts maps coq-term names to [cindef].
      @param g is an [lts_graph] accumulated while exploring [rlts].
      @param bound is the number of states to explore until.
      @return an [lts_graph] with a maximum of [bound] many states. *)
  let rec build_lts_graph
            ?(weak : E.t option = None)
            (the_primary_lts : cindef)
            (rlts_map : cindef B.t)
            (g : lts_graph)
            (bound : int)
    : lts_graph mm
    =
    if Queue.is_empty g.to_visit
    then return g (* finished if no more to visit*)
    else if S.cardinal g.states > bound
    then return g (* exit if bound reached *)
    else (
      let encoded_t : E.t = Queue.pop g.to_visit in
      let* (new_constrs : coq_ctor list) =
        get_new_constrs encoded_t the_primary_lts rlts_map
      in
      (* [get_new_states] also updates [g.to_visit] *)
      let* (new_states : S.t) = get_new_states ~weak encoded_t g new_constrs in
      let g : lts_graph = { g with states = S.union g.states new_states } in
      build_lts_graph ~weak the_primary_lts rlts_map g bound)
  ;;

  (* let check_for_primary_lts
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
     (Printf.sprintf
     "Found inductive definition that could be primary, when primary has \
     already been selected.\n\
     Please make sure that the primary LTS is provided first when using \
     the command.");
     return (fn_opt, cindef_map)
     | _, false -> return (fn_opt, cindef_map)
     ;; *)

  (* let resolve_primary_lts
     (primary_lts : Libnames.qualid)
     (primary_enc_opt : E.t option)
     (ty : EConstr.t)
     (cindef_map : cindef B.t)
     (i : int)
     : E.t mm
     =
     (* match primary_lts with
     | None ->
     (* if no primary explicitly provided, check we found one. *)
     (match primary_enc_opt with
     | None ->
     let* decoded_map = decode_map cindef_map in
     primary_lts_not_found (ty, List.of_seq (F.to_seq_keys decoded_map))
     | Some p -> return p)
     | Some primary_lts -> *)
     (* obtain the encoding for the explicitly provide primary lts *)
     let* (c : cindef) = get_lts_cindef i (Mebi_utils.ref_to_glob primary_lts) in
     let* (encoding : E.t) = encode c.info.name in
     return encoding
     ;; *)

  let handle_weak (cindef_map : cindef B.t) (i : int) : E.t option mm =
    if !weak_mode
    then (
      match !weak_type with
      | None -> return None
      | Some weak_kind ->
        (match weak_kind with
         | Option label_type ->
           Log.warning
             "command.MkGraph.handle_weak, Option weak_kind -- TODO fix \
              support for \"option _\" labels in coq lts.";
           return None
         | Custom (tau_term, label_type) ->
           let* (a : EConstr.t) = tref_to_econstr tau_term in
           let constr = Mebi_utils.ref_to_glob label_type in
           let* (c : cindef) = get_type_cindef i constr (Some a) in
           (* encode silent action type *)
           let* (constr_enc : E.t) = encode c.info.name in
           B.add cindef_map constr_enc c;
           (* return encode silent action *)
           let* (a_enc : E.t) = encode a in
           B.add cindef_map a_enc c;
           return (Some a_enc)))
    else return None
  ;;

  (** @return
        the key for the primary lts and hashtable mapping the name of the coq definition to the rlts.
  *)
  let build_cindef_map
        (primary_lts : Libnames.qualid)
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
      return (fn_opt, acc_map)
    in
    let* (primary_enc_opt, cindef_map) : E.t option * cindef B.t =
      iterate 0 (num_grefs - 1) (None, trmap) iter_body
    in
    (* encode the primary lts *)
    let* (c : cindef) =
      get_lts_cindef num_grefs (Mebi_utils.ref_to_glob primary_lts)
    in
    let* (the_primary_enc : E.t) = encode c.info.name in
    let* weak_enc_opt : E.t option = handle_weak cindef_map (num_grefs + 1) in
    return (the_primary_enc, weak_enc_opt, cindef_map)
  ;;

  (* return (fn_opt, acc_map)
     in
     let* (primary_enc_opt, cindef_map) : E.t option * cindef B.t =
     iterate 0 (num_grefs - 1) (None, trmap) iter_body
     in
     (* obtain the encoding for the explicitly provide primary lts *)
     let* (c : cindef) =
     get_lts_cindef num_grefs (Mebi_utils.ref_to_glob primary_lts)
     in
     let* (the_primary_enc : E.t) = encode c.info.name in
     let* weak_enc_opt : E.t option = handle_weak cindef_map (num_grefs + 1) in
     return (the_primary_enc, weak_enc_opt, cindef_map) *)

  (** [build_graph rlts_map fn_rlts tref bound] is the entry point for [build_lts_graph].
      @param rlts_map maps coq-term types to [cindef].
      @param tref is the original coq-term.
      @param bound is the number of states to explore until. *)
  let build_graph
        (primary_lts : Libnames.qualid)
        (tref : Constrexpr.constr_expr)
        (grefs : Names.GlobRef.t list)
    : lts_graph mm
    =
    (* make map of term types *)
    let* (t : EConstr.t) = tref_to_econstr tref in
    let* (the_primary_enc, the_weak_opt_enc, cindef_map)
      : E.t * E.t option * cindef B.t
      =
      build_cindef_map primary_lts t grefs
    in
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
        ~weak:the_weak_opt_enc
        the_primary_lts
        cindef_map
        { to_visit = q
        ; init = encoded_init
        ; terminals = S.empty
        ; states = S.empty
        ; transitions = H.create 0
        ; cindefs =
            B.fold
              (fun (_key : E.t) (_val : cindef) (acc : (E.t * cindef) list) ->
                match _val.kind with LTS _ -> (_key, _val) :: acc | _ -> acc)
              cindef_map
              []
        }
        !bound
    in
    let terminals =
      S.filter (fun (s : S.elt) -> Bool.not (H.mem g.transitions s)) g.states
    in
    return { g with terminals }
  ;;

  let decoq_enc ?(cache_decoding : bool = false) (s_enc : E.t)
    : Model.State.t mm
    =
    let* cached_decoding =
      if cache_decoding
      then
        let* s_decoding = decode s_enc in
        return (Some (Utils.clean_string (econstr_to_string s_decoding)))
      else return None
    in
    return (s_enc, cached_decoding)
  ;;

  let decoq_state ?(cache_decoding : bool = false) (s_enc : E.t)
    : Model.State.t mm
    =
    decoq_enc ~cache_decoding s_enc
  ;;

  let decoq_state_opt ?(cache_decoding : bool = false) (s_enc : E.t)
    : Model.State.t option mm
    =
    let* state = decoq_state ~cache_decoding s_enc in
    return (Some state)
  ;;

  let decoq_states ?(cache_decoding : bool = false) (ss : S.t)
    : Model.States.t mm
    =
    if S.is_empty ss
    then return Model.States.empty
    else (
      let raw_ss = S.to_list ss in
      let iter_body (i : int) (acc : Model.States.t) : Model.States.t mm =
        let state_enc = List.nth raw_ss i in
        let* state = decoq_state ~cache_decoding state_enc in
        return (Model.States.add state acc)
      in
      iterate 0 (List.length raw_ss - 1) Model.States.empty iter_body)
  ;;

  let decoq_action ?(cache_decoding : bool = false) (a : Model.Action.t)
    : Model.Action.t mm
    =
    match fst (snd a.label) with
    | None ->
      let* label_enc, label_dec = decoq_enc ~cache_decoding (fst a.label) in
      return { a with label = label_enc, (label_dec, snd (snd a.label)) }
    | Some _decoding -> return a
  ;;

  let decoq_destinations
        ?(cache_decoding : bool = false)
        (acc_trans : Model.Transitions.t)
        (from : Model.State.t)
        (action : Action.t)
        (dests : D.t)
    : Model.Transitions.t mm
    =
    if D.is_empty dests
    then return Model.Transitions.empty
    else (
      let raw_dests = D.to_list dests in
      let iter_dests (i : int) (acc_trans : Model.Transitions.t)
        : Model.Transitions.t mm
        =
        let dest, constr_tree = List.nth raw_dests i in
        let* dest = decoq_state ~cache_decoding dest in
        (* let num_trans = Model.Transitions.cardinal acc_trans in *)
        let new_trans = from, action.label, dest, Some action.meta in
        let acc_trans = Model.Transitions.add new_trans acc_trans in
        (* let num_trans' = Model.Transitions.cardinal acc_trans in
           if Int.equal num_trans num_trans'
           then
           Log.warning
           (Printf.sprintf
           "decoq_destinations, transition not added (%i // %i)\n\
           from: %s\n\
           action: %s\n\
           dest: %s\n\n\
           exists?: %b\n\
           compare?: %i\n\
           acc_trans: %s"
           num_trans
           num_trans'
           (Model.State.pstr from)
           (Model.Action.pstr action)
           (Model.State.pstr dest)
           (Model.Transitions.exists
           (fun tr -> Model.Transition.eq tr new_trans)
           acc_trans)
           (Model.Transitions.fold
           (fun tr acc ->
           if Int.equal acc 5
           then Model.Transition.compare tr new_trans
           else acc)
           acc_trans
           5)
           (Model.pstr_transitions acc_trans)); *)
        return acc_trans
      in
      iterate 0 (List.length raw_dests - 1) acc_trans iter_dests)
  ;;

  let decoq_actions
        ?(cache_decoding : bool = false)
        (acc : Model.Alphabet.t * Model.Transitions.t)
        (from : Model.State.t)
        (actions : constr_transitions)
    : (Model.Alphabet.t * Model.Transitions.t) mm
    =
    if Int.equal 0 (Hashtbl.length actions)
    then return (Model.Alphabet.empty, Model.Transitions.empty)
    else (
      let raw_actions = List.of_seq (Hashtbl.to_seq actions) in
      let iter_actions
            (i : int)
            ((acc_alpha, acc_trans) : Model.Alphabet.t * Model.Transitions.t)
        : (Model.Alphabet.t * Model.Transitions.t) mm
        =
        let action, dests = List.nth raw_actions i in
        let* action = decoq_action ~cache_decoding action in
        let acc_alpha = Model.Alphabet.add action.label acc_alpha in
        let* acc_trans =
          decoq_destinations ~cache_decoding acc_trans from action dests
        in
        return (acc_alpha, acc_trans)
      in
      iterate 0 (List.length raw_actions - 1) acc iter_actions)
  ;;

  let decoq_transitions
        ?(cache_decoding : bool = false)
        (transitions : constr_transitions H.t)
    : (Model.Alphabet.t * Model.Transitions.t) mm
    =
    if Int.equal 0 (H.length transitions)
    then return (Model.Alphabet.empty, Model.Transitions.empty)
    else (
      let raw_transitions = List.of_seq (H.to_seq transitions) in
      let iter_from (i : int) (acc : Model.Alphabet.t * Model.Transitions.t)
        : (Model.Alphabet.t * Model.Transitions.t) mm
        =
        let from, actions = List.nth raw_transitions i in
        let* from = decoq_state ~cache_decoding from in
        decoq_actions ~cache_decoding acc from actions
      in
      iterate
        0
        (List.length raw_transitions - 1)
        (Model.Alphabet.empty, Model.Transitions.empty)
        iter_from)
  ;;

  let decoq_cindefs (cindefs : (E.t * cindef) list)
    : (E.t * (string * string list)) list mm
    =
    if List.is_empty cindefs
    then return []
    else (
      let iter_body (i : int) (acc : (E.t * (string * string list)) list) =
        let ((enc, c) : E.t * cindef) = List.nth cindefs i in
        let def_str : string = econstr_to_string c.info.name in
        let names_list : string list =
          Array.fold_left
            (fun (acc : string list) (name : Names.variable) ->
              Names.Id.to_string name :: acc)
            []
            c.info.constr_names
        in
        return ((enc, (def_str, List.rev names_list)) :: acc)
      in
      iterate 0 (List.length cindefs - 1) [] iter_body)
  ;;

  let decoq_lts
        ?(cache_decoding : bool = false)
        ?(name : string = "unnamed")
        (g : lts_graph)
    : Lts.t mm
    =
    (* let* pstr_lts = pstr_lts_graph g in
       Logging.Log.warning pstr_lts; *)
    let* init : State.t option = decoq_state_opt ~cache_decoding g.init in
    let* states : Model.States.t = decoq_states ~cache_decoding g.states in
    let* terminals = decoq_states ~cache_decoding g.terminals in
    let* ((alphabet, transitions) : Model.Alphabet.t * Model.Transitions.t) =
      decoq_transitions ~cache_decoding g.transitions
    in
    let* cindefs : (E.t * (string * string list)) list =
      decoq_cindefs g.cindefs
    in
    let info : Info.t option =
      Some
        { is_complete = Queue.is_empty g.to_visit
        ; bound = !bound
        ; num_terminals = S.cardinal g.terminals
        ; num_labels = Model.Alphabet.cardinal alphabet
        ; num_states = S.cardinal g.states
        ; num_edges = num_transitions g.transitions
        ; coq_info = Some cindefs
        }
    in
    return (Lts.create init terminals alphabet states transitions info)
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

(**********************)
(** Entry point *******)
(**********************)

(* open Vernac

   let result_to_string (r : result_kind) : string =
   match r with
   | LTS the_lts -> Printf.sprintf "LTS: %s" (Lts.pstr the_lts)
   | FSM the_fsm -> Printf.sprintf "FSM: %s" (Fsm.pstr the_fsm)
   | Alg r ->
   (match r with
   | Satur the_fsm -> Printf.sprintf "FSM saturated: %s" (Fsm.pstr the_fsm)
   | Minim (the_fsm, the_partition) ->
   Printf.sprintf "FSM saturated: %s" (Fsm.pstr the_fsm)
   | Bisim
   ( (the_fsm_1, the_fsm_2)
   , the_merged_fsm
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
   (Fsm.pstr the_fsm_2))
   | Merge ((the_fsm_1, the_fsm_2), the_merged_fsm) ->
   Printf.sprintf
   "FSM merged: %s\n\nFSM A: %s\n\nFSM B: %s"
   (Fsm.pstr the_merged_fsm)
   (Fsm.pstr the_fsm_1)
   (Fsm.pstr the_fsm_2)
   | Minim _ -> "TODO: finish handle output for Minim"
   ;;

   let handle_output (o : output_kind) (r : result_kind) : unit mm =
   match o with
   | Check () -> return ()
   | Show () ->
   let s : string = result_to_string r in
   Log.notice  s;
   return ()
   | Dump name_opt ->
   let output_path : string =
   Dump_to_file.run (Default ()) (name_opt, Auto ()) (JSON ()) r
   in
   Log.notice

   (Printf.sprintf "dumped file to: '%s'" output_path);
   return ()
   ;;

   let should_cache_decoding (o : output_kind) : bool =
   match o with Check _ -> false | Show _ -> true | Dump _ -> true
   ;; *)

let () = Logging.set_output_mode (Coq ())

type model_kind =
  | LTS
  | FSM

type coq_model = Constrexpr.constr_expr * Libnames.qualid
type make_model = model_kind * coq_model

type command_kind =
  | MakeModel of make_model
  | SaturateModel of coq_model
  | MinimizeModel of coq_model
  | CheckBisimilarity of (coq_model * coq_model)

let run (k : command_kind) (refs : Libnames.qualid list) : unit mm =
  let* (graphM : (module GraphB)) = make_graph_builder in
  let module G = (val graphM) in
  let build_lts_graph
        (primary_lts : Libnames.qualid)
        (t : Constrexpr.constr_expr)
    : Lts.t mm
    =
    Log.notice "A";
    let lts_grefs : Names.GlobRef.t list =
      Mebi_utils.ref_list_to_glob_list (primary_lts :: refs)
    in
    Log.notice "B";
    let* graph_lts = G.build_graph primary_lts t lts_grefs in
    Log.notice "C";
    G.decoq_lts ~cache_decoding:true ~name:"TODO: fix name" graph_lts
  in
  match k with
  | MakeModel (kind, (x, primary_lts)) ->
    Log.notice "command.run, MakeModel";
    let* the_lts = build_lts_graph primary_lts x in
    (match kind with
     | LTS ->
       Log.notice "command.run, MakeModel LTS -- TODO finished";
       return ()
     | FSM ->
       let _the_fsm = Fsm.create_from (Lts.to_model the_lts) in
       Log.notice "command.run, MakeModel FSM -- TODO finished";
       return ())
  | SaturateModel (x, primary_lts) ->
    Log.notice "command.run, SaturateModel";
    let* the_lts = build_lts_graph primary_lts x in
    let the_fsm = Fsm.create_from (Lts.to_model the_lts) in
    let _the_saturated = Fsm.saturate the_fsm in
    Log.notice "command.run, SaturateModel -- TODO finished";
    return ()
  | MinimizeModel (x, primary_lts) ->
    Log.notice "command.run, MinimizeModel";
    let* the_lts = build_lts_graph primary_lts x in
    let the_fsm = Fsm.create_from (Lts.to_model the_lts) in
    let _the_minimized = Algorithms.run (Minim (!weak_mode, the_fsm)) in
    Log.notice "command.run, MinimizeModel -- TODO finished";
    return ()
  | CheckBisimilarity ((x, a), (y, b)) ->
    Log.notice "command.run, CheckBisimilarity";
    let* the_lts_1 = build_lts_graph a x in
    let* the_lts_2 = build_lts_graph b y in
    let _the_fsm_1 = Fsm.create_from (Lts.to_model the_lts_1) in
    let _the_fsm_2 = Fsm.create_from (Lts.to_model the_lts_2) in
    Log.notice "command.run, CheckBisimilarity -- TODO finished";
    return ()
;;
