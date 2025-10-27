open Mebi_wrapper
open Mebi_wrapper.Syntax
open Logging
open Model

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
  (* Log.debug
     (Printf.sprintf
     "Unifying\nt0: \"%s\"\nt1: \"%s\""
     (econstr_to_string t0)
     (econstr_to_string t1)); *)
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      (* Log.debug
         (Printf.sprintf
         "command.m_unitfy, Success, unified \"%s\" with \"%s\""
         (econstr_to_string t0)
         (econstr_to_string t1)); *)
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify (m, n, e))
      ->
      (* Log.debug
         (Printf.sprintf
         "command.m_unitfy, Could not unify \"%s\" with \"%s\""
         (econstr_to_string t0)
         (econstr_to_string t1)); *)
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
  : (EConstr.t * EConstr.t * EConstr.t) mm
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

let rec unify_all (i : (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list)
  : Mebi_constr.Tree.t list option mm
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
      (u : (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list)
  : (EConstr.t * Mebi_constr.Tree.t list) option mm
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

(* [act] should probably come from the unification problems? *)
let rec retrieve_tgt_nodes
          (acc : Mebi_constr.t list)
          (i : int)
          (act : EConstr.t)
          (tgt_term : EConstr.t)
  :  int * (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list
  -> Mebi_constr.t list mm
  = function
  | _, [] -> return acc
  | lts_index, u1 :: nctors ->
    let* success = sandboxed_unify tgt_term u1 in
    (match success with
     | None -> retrieve_tgt_nodes acc i act tgt_term (lts_index, nctors)
     | Some (tgt, ctor_tree) ->
       let$+ act env sigma = Reductionops.nf_all env sigma act in
       retrieve_tgt_nodes
         ((act, tgt, Node ((Enc.of_int lts_index, i), ctor_tree)) :: acc)
         i
         act
         tgt_term
         (lts_index, nctors))
;;

let unify_args_with
      (term_to_unify : EConstr.t)
      (action_to_unify : EConstr.t option)
      ((termL, act, termR) : EConstr.t * EConstr.t * EConstr.t)
  : bool mm
  =
  let* unified_termL = m_unify term_to_unify termL in
  if unified_termL
  then Option.cata (fun a -> m_unify a act) (return true) action_to_unify
  else return false
;;

let _debug_check_valid_constructor_acc lts_index ctor_index t ctor_vals
  : unit mm
  =
  let* env = get_env in
  let* sigma = get_sigma in
  Log.debug
    (Printf.sprintf
       "check_valid_constructor (%i:%i),\nfrom: %s\nctors:\n%s"
       lts_index
       ctor_index
       (Strfy.econstr env sigma t)
       (Strfy.list
          ~force_newline:true
          ~label:"ctors"
          (Strfy.lts_constr ~indent:1 env sigma)
          ctor_vals));
  return ()
;;

let _debug_check_valid_constructor_ctx_tm tm : unit mm =
  Log.debug
    (Printf.sprintf
       "check_valid_constructor (ctx, tm), tm:\n%s"
       (constr_to_string tm));
  return ()
;;

let next_constrs_to_string
      (ctors : (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list)
  : string
  =
  Strfy.list
    ~force_newline:true
    Strfy.constr_tree
    (List.fold_left
       (fun acc0 xs -> List.fold_left (fun acc1 (x, _) -> x :: acc1) acc0 xs)
       []
       ctors)
;;

let _debug_check_updated_ctx_acc
      ((lts_index, acc) :
        int * (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list)
  : unit
  =
  Log.debug
    (Printf.sprintf
       "check_updated_ctx, (%i) acc:\n%s"
       lts_index
       (next_constrs_to_string acc))
;;

let _debug_cross_product_acc
      ((lts_index, acc) :
        int * (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list)
  : unit
  =
  Log.debug
    (Printf.sprintf
       "cross_product, (%i) acc:\n%s"
       lts_index
       (next_constrs_to_string acc))
;;

(* Should return a list of unification problems *)
let rec check_updated_ctx
          (acc : int * (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list)
          (lts_encmap : Mebi_ind.t F.t)
  :  EConstr.t list * EConstr.rel_declaration list
  -> (int * (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list) option mm
  = function
  | [], [] -> return (Some acc)
  | _hsubstl :: substls, t_decl :: decls ->
    let$+ upd_t env sigma =
      EConstr.Vars.substl substls (Context.Rel.Declaration.get_type t_decl)
    in
    let* env = get_env in
    let* sigma = get_sigma in
    (match EConstr.kind sigma upd_t with
     | App (fn, args) ->
       (match F.find_opt lts_encmap fn with
        | None ->
          let* _ = handle_unrecognized_ctor_fn (fn, args) in
          check_updated_ctx acc lts_encmap (substls, decls)
        | Some c ->
          let$+ nextT env sigma = Reductionops.nf_evar sigma args.(0) in
          let* c_constr_transitions = Mebi_ind.get_constr_transitions c in
          let* ctors : Mebi_constr.t list =
            check_valid_constructor
              c_constr_transitions
              lts_encmap
              nextT
              (Some args.(1))
              c.index
          in
          (match ctors with
           | [] -> return None
           | ctors ->
             let ctors_unif_probs
               : (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list
               =
               List.map
                 (fun (_tAct, (tDest : EConstr.t), (i : Mebi_constr.Tree.t)) ->
                   let unif_prob : Mebi_setup.unif_problem =
                     { termL = tDest; termR = args.(2) }
                   in
                   i, unif_prob)
                 ctors
             in
             (cross_product acc ctors_unif_probs) lts_encmap (substls, decls)))
     | _ -> check_updated_ctx acc lts_encmap (substls, decls))
  | _substl, _ctxl -> invalid_check_updated_ctx _substl _ctxl
(* Impossible! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

and cross_product (lts_index, acc) ctree_unif_probs
  :  Mebi_ind.t F.t
  -> EConstr.Vars.substl * EConstr.rel_declaration list
  -> (int * (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list) option mm
  =
  _debug_check_updated_ctx_acc (lts_index, acc);
  (* We need to cross-product all possible unifications. This is in
     case we have a constructor of the form LTS t11 a1 t12 -> LTS t21
     a2 t22 -> ... -> LTS tn an t2n. Repetition may occur. It is not
     unavoidable, but we should make sure we understand well the
     problem before removing the source of repetition. *)
  let acc : (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list =
    List.fold_left
      (fun (acc0 : (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list)
        x ->
        List.fold_left
          (fun (acc1 : (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list)
            y -> (y :: x) :: acc1)
          acc0
          ctree_unif_probs)
      []
      acc
  in
  _debug_cross_product_acc (lts_index, acc);
  check_updated_ctx (lts_index, acc)

and check_next_constructors
      ((i, ctor_vals) : int * Mebi_constr.t list)
      (next_ctors :
        (int * (Mebi_constr.Tree.t * Mebi_setup.unif_problem) list list) option)
      ((act, tgt_term) : EConstr.t * EConstr.t)
  : Mebi_constr.t list mm
  =
  match next_ctors with
  | None -> return ctor_vals
  | Some index_ctor_pair ->
    (match snd index_ctor_pair with
     | [] ->
       let* sigma = get_sigma in
       if EConstr.isEvar sigma tgt_term
       then return ctor_vals
       else (
         let constr_tree : Mebi_constr.Tree.t =
           Mebi_constr.Tree.Node ((Enc.of_int (fst index_ctor_pair), i), [])
         in
         return ((act, tgt_term, constr_tree) :: ctor_vals))
     | nctors ->
       let tgt_nodes =
         retrieve_tgt_nodes ctor_vals i act tgt_term index_ctor_pair
       in
       tgt_nodes)

(** Checks possible transitions for this term: *)
and check_valid_constructor
      (ctor_transitions : (Constr.rel_context * Constr.types) array)
      (lts_encmap : Mebi_ind.t F.t)
      (t : EConstr.t)
      (ctor_action : EConstr.t option)
      (lts_index : int)
  : Mebi_constr.t list mm
  =
  let* t : EConstr.t = normalize_econstr t in
  let iter_body (i : int) (ctor_vals : Mebi_constr.t list) =
    let* () = _debug_check_valid_constructor_acc lts_index i t ctor_vals in
    let (ctx, tm) : Constr.rel_context * Constr.t = ctor_transitions.(i) in
    (* let* () = _debug_check_valid_constructor_ctx_tm tm in *)
    let ctx_tys = List.map EConstr.of_rel_decl ctx in
    let* substl = mk_ctx_substl [] (List.rev ctx_tys) in
    let* args = extract_args substl tm in
    let* successfully_unified : bool = unify_args_with t ctor_action args in
    if successfully_unified
    then (
      let (_termL, act, termR) : EConstr.t * EConstr.t * EConstr.t = args in
      let* act : EConstr.t = normalize_econstr act in
      let tgt_term : EConstr.t = EConstr.Vars.substl substl termR in
      let* next_ctors =
        check_updated_ctx (lts_index, [ [] ]) lts_encmap (substl, ctx_tys)
      in
      check_next_constructors (i, ctor_vals) next_ctors (act, tgt_term))
    else return ctor_vals
  in
  iterate 0 (Array.length ctor_transitions - 1) [] iter_body

(** raises exception -- we don't yet support these kinds of arguments
    - TODO: finish handling these, or throw helpful error *)
and handle_unrecognized_ctor_fn ((fn, args) : EConstr.t * EConstr.t array)
  : unit mm
  =
  let* env = get_env in
  let* sigma = get_sigma in
  Log.warning
    (Printf.sprintf
       "check_updated_ctx, unrecognized in lts_encmap:\n- fn (%s)\n- args: %s"
       (Strfy.econstr env sigma fn)
       (Strfy.list (Strfy.econstr env sigma) (Array.to_list args)));
  (* NOTE: testing handling the [@eq] premises *)
  match econstr_to_string fn with
  | "option" ->
    (* TODO: fail if weak is not Params.WeakEnc.OptionConstr *)
    return ()
  | "@eq" ->
    (* TODO: fail, or *)
    (* TODO: find way to propagate this (replace in F map?) *)
    let* lhs : EConstr.t = normalize_econstr args.(1) in
    let* rhs : EConstr.t = normalize_econstr args.(2) in
    Log.warning
      (Printf.sprintf
         "TODO: check_updated_ctx, handle premise (normalized):\n\
          fn: %s\n\
          lhs: %s\n\
          rhs: %s"
         (econstr_to_string fn)
         (econstr_to_string lhs)
         (econstr_to_string rhs));
    return ()
  | _ ->
    (* TODO: fail? *)
    return ()
;;

(** [GraphB] is ...
    (Essentially acts as a `.mli` for the [MkGraph] module.) *)
module type GraphB = sig
  module H : Hashtbl.S with type key = Enc.t
  module S : Set.S with type elt = Enc.t
  module D : Set.S with type elt = Enc.t * Mebi_constr.Tree.t

  type constr_transitions = (Action.t, D.t) Hashtbl.t

  type lts_graph =
    { to_visit : Enc.t Queue.t
    ; init : Enc.t
    ; terminals : S.t
    ; states : S.t
    ; transitions : constr_transitions H.t
      (* ; ind_defs : (Enc.t * Mebi_ind.t) list *)
    ; ind_defs : Mebi_ind.t B.t
    ; weak : Params.WeakEnc.t option
    }

  val insert_constr_transition
    :  constr_transitions
    -> Action.t
    -> Enc.t
    -> Mebi_constr.Tree.t
    -> unit mm

  val add_new_term_constr_transition
    :  lts_graph
    -> Enc.t
    -> Action.t
    -> Enc.t
    -> Mebi_constr.Tree.t
    -> unit mm

  val build_lts_graph
    :  Mebi_ind.t
    -> Mebi_ind.t B.t
    -> lts_graph
    -> int * Params.WeakEnc.t option
    -> lts_graph mm

  val build_graph
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Names.GlobRef.t list
    -> int * Params.WeakEnc.t option
    -> lts_graph mm

  val decoq_lts
    :  ?cache_decoding:bool
    -> ?name:string
    -> lts_graph (* -> cindef * cindef B.t *)
    -> int * Params.WeakEnc.t option
    -> Lts.t mm
end

(** [MkGraph M] is ...
    [M] is a ... *)
module MkGraph
    (M : Hashtbl.S with type key = Enc.t)
    (N : Set.S with type elt = Enc.t)
    (P : Set.S with type elt = Enc.t * Mebi_constr.Tree.t) : GraphB = struct
  (* [H] is the hashtbl of outgoing transitions, from some [EConstr.t] and also
     is used for mapping term types to [cindef]. *)
  module H = M

  (* [S] is the set of states, of [EConstr.t]. *)
  module S = N

  (* [D] is the set of destination tuples, each comprised of a term [EConstr.t]
     and the corresponding [Mebi_constr.Tree.t]. *)
  module D = P

  (** [constr_transitions] is a hashtbl mapping [action]s to terms of [EConstr.t] and [Mebi_constr.Tree.t].
  *)
  type constr_transitions = (Action.t, D.t) Hashtbl.t

  let num_transitions (ts : constr_transitions H.t) : int =
    H.fold
      (fun (_from : Enc.t) (transitions : constr_transitions) (acc : int) ->
        Hashtbl.fold
          (fun (_a : Action.t) (destinations : D.t) (acc' : int) ->
            acc' + D.cardinal destinations)
          transitions
          acc)
      ts
      0
  ;;

  (** [lts_graph] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Mebi_constr.Tree.t].
  *)
  type lts_graph =
    { to_visit : Enc.t Queue.t
    ; init : Enc.t
    ; terminals : S.t
    ; states : S.t
    ; transitions : constr_transitions H.t
      (* ; ind_defs : (Enc.t * Mebi_ind.t) list *)
    ; ind_defs : Mebi_ind.t B.t
    ; weak : Params.WeakEnc.t option
    }

  (** [insert_constr_transition] handles adding the mapping of action [a] to tuple [(term * Mebi_constr.Tree.t)] in a given [constr_transitions].
  *)
  let insert_constr_transition
        (constrs : constr_transitions)
        (a : Action.t)
        (d : Enc.t)
        (c : Mebi_constr.Tree.t)
    : unit mm
    =
    (match Hashtbl.find_opt constrs a with
     | None -> Hashtbl.add constrs a (D.singleton (d, c))
     | Some ds -> Hashtbl.replace constrs a (D.add (d, c) ds));
    return ()
  ;;

  let add_new_term_constr_transition
        (g : lts_graph)
        (t : Enc.t)
        (a : Action.t)
        (d : Enc.t)
        (c : Mebi_constr.Tree.t)
    : unit mm
    =
    H.add
      g.transitions
      t
      (Hashtbl.of_seq (List.to_seq [ a, D.singleton (d, c) ]));
    return ()
  ;;

  let is_silent_transition (weak : Params.WeakEnc.t option) (act : EConstr.t)
    : bool option mm
    =
    match weak with
    | None -> return None
    | Some weak_kind ->
      Log.trace "command.MkGraph.is_silent_transition";
      let* act_enc : Enc.t = encode act in
      let* ty : EConstr.t = type_of_econstr act in
      let* ty_enc : Enc.t = encode ty in
      Log.debug
        (Printf.sprintf
           "command.MkGraph.is_silent_transition, type of act: %s"
           (econstr_to_string ty));
      let open Params.WeakEnc in
      (match weak_kind with
       | OptionConstr label_enc ->
         let* decoding = decode label_enc in
         Log.debug
           (Printf.sprintf
              "command.MkGraph.is_silent_transition, OptionConstr (%s) => %s"
              (Enc.to_string label_enc)
              (econstr_to_string decoding));
         (* all non-silent actions should be of this type *)
         (* if Enc.eq label_enc ty_enc
         then return (Some false)
         else *)
         let* b = is_none_term act in
         return (Some b)
       | CustomConstr (tau_enc, label_enc) ->
         let* tau_decoding = decode tau_enc in
         let* label_decoding = decode label_enc in
         Log.debug
           (Printf.sprintf
              "command.MkGraph.is_silent_transition, CustomRef\n\
               - tau: (%s) => %s\n\
               - label: (%s) => %s"
              (Enc.to_string tau_enc)
              (econstr_to_string tau_decoding)
              (Enc.to_string label_enc)
              (econstr_to_string label_decoding));
         return (Some (Enc.eq tau_enc act_enc)))
  ;;

  let get_new_states
        ?(weak_type : Params.WeakEnc.t option = None)
        (from : Enc.t)
        (g : lts_graph)
        (ctors : Mebi_constr.t list)
    : S.t mm
    =
    Log.trace "command.MkGraph.get_new_states";
    let iter_body (i : int) (new_states : S.t) =
      let (act, tgt, int_tree) : Mebi_constr.t = List.nth ctors i in
      let* tgt_enc : Enc.t = encode tgt in
      let* act_enc : Enc.t = encode act in
      let* is_silent : bool option = is_silent_transition weak_type act in
      let meta : Action.MetaData.t = [ int_tree ] in
      Log.debug
        (Printf.sprintf
           "command.MkGraph.get_new_states, new meta: %s"
           (Action.MetaData.to_string meta));
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

  (** [get_new_constrs t lts_ind_def_map] returns the list of constructors applicable to term [t], using those provided in [lts_ind_def_map].
      If no immediate constructor is found matching [t] in [lts_ind_def_map] (likely due to unification problems), then each constructor in [lts_ind_def_map] is tried sequentially, until one of them returns some valid constructors.
      @raise CannotFindTypeOfTermToVisit
        if none of the constructors provided in [lts_ind_def_map] yield constructors from [check_valid_constructors].
  *)
  let get_new_constrs
        (from : Enc.t)
        (primary : Mebi_ind.t)
        (lts_ind_def_map : Mebi_ind.t B.t)
    : Mebi_constr.t list mm
    =
    Log.trace "command.MkGraph.get_new_constrs";
    let* from_dec : EConstr.t = decode from in
    let* decoded_map : Mebi_ind.t F.t = decode_map lts_ind_def_map in
    let* primary_constr_transitions = Mebi_ind.get_constr_transitions primary in
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
            (the_primary_lts : Mebi_ind.t)
            (lts_ind_def_map : Mebi_ind.t B.t)
            (g : lts_graph)
            ((bound, weak_type) : int * Params.WeakEnc.t option)
    : lts_graph mm
    =
    Log.trace "command.MkGraph.build_lts_graph";
    if Queue.is_empty g.to_visit
    then return g (* finished if no more to visit*)
    else if S.cardinal g.states > bound
    then return g (* exit if bound reached *)
    else (
      let encoded_t : Enc.t = Queue.pop g.to_visit in
      let* new_constrs : Mebi_constr.t list =
        get_new_constrs encoded_t the_primary_lts lts_ind_def_map
      in
      (* [get_new_states] also updates [g.to_visit] *)
      let* new_states : S.t =
        get_new_states ~weak_type encoded_t g new_constrs
      in
      let g : lts_graph = { g with states = S.union g.states new_states } in
      build_lts_graph the_primary_lts lts_ind_def_map g (bound, weak_type))
  ;;

  (** @return
        the key for the primary lts and hashtable mapping the name of the coq definition to the rlts.
  *)
  let build_lts_ind_def_map (grefs : Names.GlobRef.t list) : Mebi_ind.t B.t mm =
    Log.trace "command.MkGraph.build_lts_ind_def_map";
    let num_grefs : int = List.length grefs in
    let iter_body (i : int) (acc_map : Mebi_ind.t B.t) =
      let gref : Names.GlobRef.t = List.nth grefs i in
      let* lts_ind_def : Mebi_ind.t = Mebi_utils.get_ind_lts i gref in
      (* add name of inductive prop *)
      let* encoding : Enc.t = encode lts_ind_def.info.name in
      if Bool.not (B.mem acc_map encoding)
      then B.add acc_map encoding lts_ind_def;
      return acc_map
    in
    iterate 0 (num_grefs - 1) (B.create num_grefs) iter_body
  ;;

  let get_primary_lts
        (primary_lts : Libnames.qualid)
        (grefs : Names.GlobRef.t list)
        (lts_ind_def_map : Mebi_ind.t B.t)
    : Mebi_ind.t mm
    =
    let open Mebi_utils in
    (* encode the primary lts *)
    let* primary_lts_ind_def : Mebi_ind.t =
      get_ind_lts (List.length grefs) (ref_to_glob primary_lts)
    in
    let* the_primary_enc : Enc.t = encode primary_lts_ind_def.info.name in
    return (B.find lts_ind_def_map the_primary_enc)
  ;;

  (** [build_graph lts_ind_def_map fn_rlts tref bound] is the entry point for [build_lts_graph].
      @param lts_ind_def_map maps coq-term types to [Mebi_ind.t].
      @param tref is the original coq-term.
      @param bound is the number of states to explore until. *)
  let build_graph
        (primary_lts : Libnames.qualid)
        (tref : Constrexpr.constr_expr)
        (grefs : Names.GlobRef.t list)
        ((bound, weak_type) : int * Params.WeakEnc.t option)
    : lts_graph mm
    =
    Log.trace "command.MkGraph.build_graph";
    (* normalize the initial term *)
    let* t : EConstr.t = constrexpr_to_econstr tref in
    let* t : EConstr.t = normalize_econstr t in
    (* encode lts inductive definitions *)
    let* lts_ind_def_map : Mebi_ind.t B.t = build_lts_ind_def_map grefs in
    let* the_primary_lts = get_primary_lts primary_lts grefs lts_ind_def_map in
    (* update environment by typechecking *)
    let* primary_trm_type = Mebi_ind.get_lts_trm_type the_primary_lts in
    let$* u env sigma = Typing.check env sigma t primary_trm_type in
    (* get initial term *)
    let$ init env sigma = sigma, Reductionops.nf_all env sigma t in
    let* encoded_init = encode init in
    let q = Queue.create () in
    let* _ = return (Queue.push encoded_init q) in
    let* g =
      build_lts_graph
        the_primary_lts
        lts_ind_def_map
        { to_visit = q
        ; init = encoded_init
        ; terminals = S.empty
        ; states = S.empty
        ; transitions = H.create 0
        ; ind_defs =
            lts_ind_def_map
            (* B.fold
               (fun (_key : Enc.t)
               (_val : Mebi_ind.t)
               (acc : (Enc.t * Mebi_ind.t) list) ->
               match _val.kind with LTS _ -> (_key, _val) :: acc | _ -> acc)
               lts_ind_def_map
               [] *)
        ; weak = weak_type
        }
        (bound, weak_type)
    in
    let terminals =
      S.filter (fun (s : S.elt) -> Bool.not (H.mem g.transitions s)) g.states
    in
    return { g with terminals }
  ;;

  let decoq_enc ?(cache_decoding : bool = false) (s_enc : Enc.t)
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

  let decoq_state ?(cache_decoding : bool = false) (s_enc : Enc.t)
    : Model.State.t mm
    =
    decoq_enc ~cache_decoding s_enc
  ;;

  let decoq_state_opt ?(cache_decoding : bool = false) (s_enc : Enc.t)
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
        let new_trans = from, action.label, dest, Some action.meta in
        return (Model.Transitions.add new_trans acc_trans)
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

  let decoq_lts_ind_def_map (lts_ind_def_map : Mebi_ind.t B.t)
    : (Enc.t * (string * string list)) list
    =
    let x = ref 0 in
    let xpp () =
      let y = !x in
      x := y + 1;
      y
    in
    B.fold
      (fun (lts_enc : Enc.t)
        (the_ind_def : Mebi_ind.t)
        (acc : (Enc.t * (string * string list)) list) ->
        match the_ind_def.kind with
        | LTS the_lts_ind_def ->
          let lts_name = econstr_to_string the_ind_def.info.name in
          let lts_name = Printf.sprintf "%s_%i" lts_name (xpp ()) in
          let lts_constrs =
            Array.fold_left
              (fun (acc : string list) (name : Names.Id.t) ->
                Names.Id.to_string name :: acc)
              []
              the_ind_def.info.constr_names
          in
          (lts_enc, (lts_name, lts_constrs)) :: acc
        | _ -> acc)
      lts_ind_def_map
      []
  ;;

  let decoq_weak_enc (w : Params.WeakEnc.t option) : string list mm =
    return [ "TODO: decoq_weak_enc" ]
  ;;

  let decoq_lts
        ?(cache_decoding : bool = false)
        ?(name : string = "unnamed")
        (g : lts_graph)
        ((bound, weak_type) : int * Params.WeakEnc.t option)
    : Lts.t mm
    =
    Log.debug "command.MkGraph.decoq_lts";
    let* init : State.t option = decoq_state_opt ~cache_decoding g.init in
    let* states : Model.States.t = decoq_states ~cache_decoding g.states in
    let* terminals = decoq_states ~cache_decoding g.terminals in
    let* ((alphabet, transitions) : Model.Alphabet.t * Model.Transitions.t) =
      decoq_transitions ~cache_decoding g.transitions
    in
    let ind_defs : (Enc.t * (string * string list)) list =
      decoq_lts_ind_def_map g.ind_defs
    in
    let* w : string list = decoq_weak_enc g.weak in
    let info : Info.t option =
      Some
        { is_complete = Queue.is_empty g.to_visit
        ; bound
        ; num_terminals = S.cardinal g.terminals
        ; num_labels = Model.Alphabet.cardinal alphabet
        ; num_states = S.cardinal g.states
        ; num_edges = num_transitions g.transitions
        ; coq_info = Some ind_defs
        ; weak_info = Some w
        }
    in
    return (Lts.create init terminals alphabet states transitions info)
  ;;
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  Log.trace "command.make_graph_builder";
  let* h = make_transition_tbl in
  (* hashtabl of terms to (edges) or (cindef) *)
  let* s = make_state_set in
  (* set of states (econstr term) *)
  let* d = make_state_tree_pair_set in
  (* hashtabl mapping term type or cindef *)
  let module G : GraphB = MkGraph ((val h)) ((val s)) ((val d)) in
  return (module G : GraphB)
;;

let build_lts_graph
      (primary_lts : Libnames.qualid)
      (t : Constrexpr.constr_expr)
      (params : int * Params.WeakEnc.t option)
      (refs : Libnames.qualid list)
  : Lts.t mm
  =
  Log.debug "command.run.build_lts_graph";
  let* graphM : (module GraphB) = make_graph_builder in
  let module G = (val graphM) in
  let* graph_lts =
    G.build_graph
      primary_lts
      t
      (Mebi_utils.ref_list_to_glob_list (primary_lts :: refs))
      params
  in
  if
    !Params.the_fail_if_incomplete
    && Bool.not (Queue.is_empty graph_lts.to_visit)
  then params_fail_if_incomplete ()
  else G.decoq_lts ~cache_decoding:true ~name:"TODO: fix name" graph_lts params
;;

let build_fsm
      ?(saturate : bool = false)
      ?(minimize : bool = false)
      (primary_lts : Libnames.qualid)
      (t : Constrexpr.constr_expr)
      (params : int * Params.WeakEnc.t option)
      (refs : Libnames.qualid list)
  : Fsm.t mm
  =
  let* the_lts =
    build_lts_graph primary_lts t (Params.get_fst_params ()) refs
  in
  let the_fsm = Fsm.create_from (Lts.to_model the_lts) in
  Log.warning
    (Printf.sprintf "command.build_fsm:\n%s\n" (Fsm.to_string the_fsm));
  if minimize
  then (
    let the_minimized_fsm, _bisim_states =
      Algorithms.Minimize.run ~weak:!Params.the_weak_mode the_fsm
    in
    Log.details
      (Printf.sprintf
         "command.build_fsm, minimized & bisim states: %s\n"
         (Algorithms.Minimize.pstr (the_minimized_fsm, _bisim_states)));
    return the_minimized_fsm)
  else if saturate
  then return (Fsm.saturate the_fsm)
  else return the_fsm
;;

(**********************)
(** Entry point *******)
(**********************)

type model_kind =
  | LTS
  | FSM

type coq_model = Constrexpr.constr_expr * Libnames.qualid
type make_model = model_kind * coq_model

type command_kind =
  | Help of Mebi_help.help_kind
  | MakeModel of make_model
  | SaturateModel of coq_model
  | MinimizeModel of coq_model
  | CheckBisimilarity of (coq_model * coq_model)
  | Info of unit

let make_model args refs =
  Log.trace "command.make_model";
  let* result_str : string * string =
    match args with
    | LTS, (x, primary_lts) ->
      let* the_lts : Lts.t =
        build_lts_graph primary_lts x (Params.get_fst_params ()) refs
      in
      return ("LTS", Lts.to_string the_lts)
    | FSM, (x, primary_lts) ->
      let* the_fsm : Fsm.t =
        build_fsm primary_lts x (Params.get_fst_params ()) refs
      in
      return ("FSM", Fsm.to_string the_fsm)
  in
  Log.result
    (Printf.sprintf
       "command.make_model, finished %s:\n%s\n"
       (fst result_str)
       (snd result_str));
  return ()
;;

let only_in_weak_mode (f : 'a mm) : unit mm =
  match Params.fst_weak_type () with
  | None ->
    if !Params.the_weak_mode = false
    then Mebi_help.show_instructions_to_enable_weak ();
    Mebi_help.show_instructions_to_set_weak ();
    Log.warning "Aborting command.\n";
    return ()
  | Some _ ->
    if !Params.the_weak_mode = false
    then (
      Mebi_help.show_instructions_to_enable_weak ();
      Log.warning "Aborting command.\n";
      return ())
    else
      let* _ = f in
      return ()
;;

let saturate_model ((x, primary_lts) : coq_model) refs : unit mm =
  Log.trace "command.saturate_model";
  only_in_weak_mode
    (let* the_saturated_fsm =
       build_fsm ~saturate:true primary_lts x (Params.get_fst_params ()) refs
     in
     Log.result
       (Printf.sprintf
          "command.saturate_model, finished: %s\n"
          (Fsm.to_string the_saturated_fsm));
     return ())
;;

let minimize_model ((x, primary_lts) : coq_model) refs : unit mm =
  Log.trace "command.minimize_model";
  only_in_weak_mode
    (let* the_minimized_fsm =
       build_fsm ~minimize:true primary_lts x (Params.get_fst_params ()) refs
     in
     Log.result
       (Printf.sprintf
          "command.minimize_model, finished: %s\n"
          (Fsm.to_string the_minimized_fsm));
     return ())
;;

let check_bisimilarity ((x, a), (y, b)) refs : unit mm =
  Log.trace "command.check_bisimilarity";
  let* the_fsm_1 = build_fsm a x (Params.get_fst_params ()) refs in
  let* the_fsm_2 = build_fsm b y (Params.get_fst_params ()) refs in
  let the_bisimilar =
    Algorithms.Bisimilar.run ~weak:!Params.the_weak_mode (the_fsm_1, the_fsm_2)
  in
  Mebi_bisim.set_the_result the_bisimilar;
  Log.result
    (Printf.sprintf
       "command.run, CheckBisimilarity, finished: %s\n"
       (Algorithms.Bisimilar.pstr the_bisimilar));
  Log.details
    (Printf.sprintf
       "command.run, CheckBisimilarity, saturated:\nFSM 1: %s\n\nFSM 2: %s\n"
       (Fsm.to_string the_bisimilar.the_fsm_1)
       (Fsm.to_string the_bisimilar.the_fsm_2));
  if
    !Params.the_fail_if_not_bisim
    && Algorithms.Bisimilar.result_to_bool the_bisimilar
  then return ()
  else params_fail_if_not_bisim ()
;;

let run (k : command_kind) (refs : Libnames.qualid list) : 'a mm =
  Log.trace "command.run";
  let* _ = Params.obtain_weak_kinds_from_args () in
  (* if !Params.the_weak_mode = true then *)
  (* let* _ = Mebi_wrapper.load_none_term () in *)
  match k with
  | MakeModel args -> make_model args refs
  | SaturateModel args -> saturate_model args refs
  | MinimizeModel args -> minimize_model args refs
  | CheckBisimilarity args -> check_bisimilarity args refs
  | Info () ->
    Mebi_help.show_guidelines_and_limitations ();
    return ()
  | Help c ->
    Mebi_help.handle_help c;
    return ()
;;

let proof_intro
      (pstate : Declare.Proof.t)
      ((x, a) : coq_model)
      ((y, b) : coq_model)
      (refs : Libnames.qualid list)
  : Declare.Proof.t mm
  =
  Log.trace "command.proof_intro";
  Mebi_bisim.reset_the_proof_state ();
  let* _ = run (CheckBisimilarity ((x, a), (y, b))) refs in
  Log.debug
    (Printf.sprintf
       "command.proof_intro, the_fsm_1:\n%s"
       (Fsm.pstr !(Mebi_bisim.get_the_result ()).the_fsm_1));
  Log.debug
    (Printf.sprintf
       "command.proof_intro, the_fsm_2:\n%s"
       (Fsm.pstr !(Mebi_bisim.get_the_result ()).the_fsm_2));
  return
    (Mebi_tactics.update_proof_by_tactic
       pstate
       (Proofview.Goal.enter (fun gl ->
          Mebi_tactics.unfold_constrexpr_list gl [ x; y ])))
;;
