(* open Logging *)
open Mebi_debug
open Mebi_wrapper
open Mebi_wrapper.Syntax
open Mebi_unification

let show_unification_debug : bool = false
let show_extractargs_debug : bool = false

(****************************************************************************)

(** sandbox unifies the fresh with the original, returning the value which is then replaces the destination in the resulting constructor
*)
(* let rec fresh_constructors : (Constructor_arg.Fresh.t option * Mebi_constr.t) list -> Constructors.t = function 
| (None, c) ::tl-> c::  (fresh_constructors tl)
| (Some {sigma;evar;original},(action,destination,tree))::tl -> 
  let c' = 
  sandbox (

  )
  in
  c'::  (fresh_constructors tl)
;; *)

exception ConstructorArgsExpectsArraySize3 of unit

let constructor_args (args : EConstr.t array) : constructor_args =
  if Int.equal (Array.length args) 3
  then { lhs = args.(0); act = args.(1); rhs = args.(2) }
  else raise (*TODO:err*) (ConstructorArgsExpectsArraySize3 ())
;;

(** [a] may be re-freshed *)
let map_constr_to_pair (a : EConstr.t) (b : EConstr.t) : Pair.t mm =
  state (fun env sigma -> Pair.make env sigma a b)
;;

type labelled_problem = EConstr.t * Problem.t
type labelled_problems = EConstr.t * Problems.t

(** creates unification problems between the rhs of the current constructor and the lhs of the next.
*)
let map_constr_to_problem args : Mebi_constr.t -> Problem.t mm = function
  | _act, lhs, tree ->
    let* pair : Pair.t = map_constr_to_pair args.rhs lhs in
    return (pair, tree)
;;

let map_labelled_problems args (constructors : Constructors.t)
  : labelled_problem list mm
  =
  let iter_body (i : int) (acc : labelled_problem list)
    : labelled_problem list mm
    =
    let constructor : Mebi_constr.t = List.nth constructors i in
    let* problem : Problem.t = map_constr_to_problem args constructor in
    let action, _destination, _tree = constructor in
    return ((action, problem) :: acc)
  in
  iterate 0 (List.length constructors - 1) [] iter_body
;;

let map_problems args (constructors : Constructors.t) : Problems.t mm =
  let iter_body (i : int) (acc : Problems.t) : Problems.t mm =
    let constructor : Mebi_constr.t = List.nth constructors i in
    let* problem : Problem.t = map_constr_to_problem args constructor in
    return (problem :: acc)
  in
  iterate 0 (List.length constructors - 1) [] iter_body
;;

let flatten_labelled_probelm_list (p : labelled_problem list)
  : labelled_problems list
  =
  List.fold_left
    (fun (acc : labelled_problems list) (action, problem) ->
      match List.find_opt (fun (x : labelled_problems) -> true) acc with
      | None -> (action, [ problem ]) :: acc
      | Some (action, problems) -> (action, problem :: problems) :: acc)
    []
    p
;;

(* let labelled_cross_product
   (acc' : labelled_problem list list)
   (problems : labelled_problem list)
   : labelled_problem list list mm
   =
   Logging.Log.debug "labelled cross product";
   (* let acc' = flatten_labelled_probelm_list acc' in *)
   (* let cross_map *)
   let iter_body (i : int) (outer_acc : labelled_problem list list)
   : labelled_problem list list mm
   =
   let xproblems : labelled_problem list = List.nth acc' i in
   let iter_body2 (j : int) (acc : labelled_problem list list)
   : labelled_problem list list mm
   =
   let (yact, yproblem) : labelled_problem = List.nth problems j in
   (* let* () = debug_labelled_cross_product_mm xact yact xproblems yproblem in *)
   (* let (xact, xproblem) : labelled_problem = List.nth xproblems k in *)
   let* env = get_env in
   let* sigma = get_sigma in
   let f = Mebi_setup.Eq.econstr sigma yact in
   let new_problems : labelled_problem list list =
   List.map
   (fun (xact, xproblem) -> if f xact then [] else [ xact, xproblem ])
   (* if f xact
   then
   (* let h =  in  *)
   (* return ((xact, yproblem :: xproblems) :: acc) *)
   []
   else [] *)
   xproblems
   in
   return (List.concat [ new_problems; acc ])
   (* let* inner_acc = iterate 0 (List.length xproblems - 1) [] iter_body3 in *)
   (* return (inner_acc :: acc) *)
   in
   iterate 0 (List.length problems - 1) outer_acc iter_body2
   in
   iterate 0 (List.length acc' - 1) [ [] ] iter_body
   ;; *)

let cross_product (acc : Problems.t list) (problems : Problems.t)
  : Problems.t list
  =
  List.concat_map
    (fun (x : Problems.t) -> List.map (fun (y : Problem.t) -> y :: x) problems)
    acc
;;

exception TryUnifyConstrArgsYieldFresh of unit

let try_unify_constructor_arg
      ?(debug : bool = show_unification_debug)
      (a : EConstr.t)
      (b : EConstr.t)
  : bool mm
  =
  state (fun env sigma ->
    match Pair.unify ~debug env sigma (Pair.normal a b) with
    | sigma, _, false -> sigma, false
    | sigma, None, true -> sigma, true
    | sigma, Some fresh, true ->
      Logging.Log.warning "try_to_unify_constructor_args, did not expect fresh";
      raise (* TODO: err *) (TryUnifyConstrArgsYieldFresh ()))
;;

let try_unify_constructor_args
      ?(debug : bool = show_unification_debug)
      (lhs : EConstr.t)
      (act : EConstr.t option)
      (args : constructor_args)
  : bool mm
  =
  let f = try_unify_constructor_arg in
  let* lhs_unifies : bool = f args.lhs lhs in
  if lhs_unifies
  then Option.cata (fun act -> f args.act act) (return true) act
  else return false
;;

let subst_of_decl (substl : EConstr.Vars.substl) x : EConstr.t mm =
  let ty = Context.Rel.Declaration.get_type x in
  let$+ subst _ _ = EConstr.Vars.substl substl ty in
  return subst
;;

(** [mk_ctx_subst ?substl x] returns a new [evar] made from the type of [x], using any [substl] provided.
    @param ?substl
      is a list of substitutions, (* TODO: provided so that collisions don't occur? *)
    @param x
      corresponds to a (* TODO: universally? *) quantified term of a constructor.
    @return a new [evar] for [x]. *)
let mk_ctx_subst
      (substl : EConstr.Vars.substl)
      (x : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt)
  : EConstr.t mm
  =
  let* subst = subst_of_decl substl x in
  let$ vt env sigma = Evarutil.new_evar env sigma subst in
  return vt
;;

(** [mk_ctx_substl acc ts] makes an [evar] for each term declaration in [ts].
    @param acc
      contains the substitutions accumulated so far, and is returned once [ts=[]]
    @param ts
      is an [EConstr.rel_declaration list] (obtained from the context of a constructor).
    @return [acc] of [evars] once [ts] is empty. *)
let rec mk_ctx_substl (acc : EConstr.Vars.substl)
  :  ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
  -> EConstr.Vars.substl mm
  = function
  | [] -> return acc
  | t :: ts ->
    let* vt = mk_ctx_subst acc t in
    mk_ctx_substl (vt :: acc) ts
;;

let debug_extract_args name : constructor_args -> unit mm = function
  | { lhs; act; rhs } ->
    state (fun env sigma ->
      if show_extractargs_debug
      then (
        let name : string = Rocq_utils.Strfy.constr env sigma name in
        let f = Rocq_utils.Strfy.econstr env sigma in
        let m = Printf.sprintf "%s : %s  %s  %s" name (f lhs) (f act) (f rhs) in
        let () = Debug.Scope.return ~infix:"extract args" ~info:(Some m) () in
        sigma, ())
      else sigma, ())
;;

(** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
    @param ?substl
      is a list of substitutions applied to the terms prior to being returned.
    @param term
      must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
    @return a triple of [lhs_term, action, rhs_term]. *)
let extract_args ?(substl : EConstr.Vars.substl = []) (term : Constr.t)
  : constructor_args mm
  =
  match Constr.kind term with
  | App (_name, args) ->
    if Array.length args == 3
    then (
      let args = EConstr.of_constr_array args in
      let args = Array.map (EConstr.Vars.substl substl) args in
      let args = constructor_args args in
      let* () = debug_extract_args _name args in
      return args)
    else (* TODO: err *) invalid_lts_args_length (Array.length args)
  | _ -> (* TODO: err *) invalid_lts_term_kind term
;;

(* let update_constructor_action

   (constructor:Mebi_constr.t)
   : (EConstr.t * EConstr.t) option -> Mebi_constr.t mm = function
   | None -> return constructor
   | Some (fresh, original) ->
   let (action, destination, tree) = constructor in
   state (fun env sigma ->
   if Mebi_setup.Eq.econstr sigma action original then
   sigma, (fresh, destination, tree) else
   sigma, (action, destination, tree)
   )
   ;; *)

(*   let update_constructor_actions

     :
     ((Evd.econstr * Evd.econstr) option list * Constructors.t

     ) ->
     Constructors.t mm = function | [], constructors -> return constructors

     let constructors : Constructors.t =
     match fresh_act_opt with
     | None -> constructors
     | Some (original_act, fresh_act) ->
     Logging.Log.debug
     (Printf.sprintf
     "fresh_act B:\n- act: %s\n- fresh: %s"
     (Rocq_utils.Strfy.econstr env sigma original_act)
     (Rocq_utils.Strfy.econstr env sigma fresh_act));
     List.map
     (fun (x, destination, tree) ->
     if Mebi_setup.Eq.econstr sigma x original_act
     then fresh_act, destination, tree
     else x, destination, tree)
     constructors
     in *)

let update_constructors_action
      env
      sigma
      original
      fresh
      (constructors : Constructors.t)
  : Constructors.t
  =
  List.map
    (fun (x, destination, tree) ->
      if Mebi_setup.Eq.econstr sigma x original
      then fresh, destination, tree
      else x, destination, tree)
    constructors
;;

let update_constructors_fresh_action (constructors : Constructors.t)
  : (EConstr.t * EConstr.t) option -> Constructors.t mm
  = function
  | None -> return constructors
  | Some (original, fresh) ->
    state (fun env sigma ->
      Logging.Log.debug
        (Printf.sprintf
           "update fresh_act:\n- original: %s\n- fresh: %s"
           (Rocq_utils.Strfy.econstr env sigma original)
           (Rocq_utils.Strfy.econstr env sigma fresh));
      sigma, update_constructors_action env sigma original fresh constructors)
;;

let get_fresh_evar (original : Rocq_utils.evar_source) : EConstr.t mm =
  state (fun env sigma -> Rocq_utils.get_next env sigma original)
;;

(* let make_fresh_econstr env sigma (original : EConstr.t)
   : Evd.evar_map * (EConstr.t * EConstr.t)
   =
   let sigma, fresh = Rocq_utils.get_next env sigma original in
   sigma, (original, fresh)
   ;; *)

(* let check_fresh_econstr env sigma (original : EConstr.t)
   : Evd.evar_map * EConstr.t
   =
   if EConstr.isEvar sigma original
   then (
   let sigma, (_original, fresh) = make_fresh_econstr env sigma original in
   sigma, fresh)
   else sigma, original
   ;; *)

(* let make_fresh_action_opt (action : EConstr.t option)
   : (EConstr.t * EConstr.t) option mm
   =
   state (fun env sigma ->
   match action with
   | None -> sigma, None
   | Some original ->
   let sigma, (original, fresh) = make_fresh_econstr env sigma original in
   sigma, Some (original, fresh))
   ;; *)

(* let unpack_fresh_action_opt
   : (EConstr.t * EConstr.t) option -> (EConstr.t option * EConstr.t option) mm
   = function
   | None -> return (None, None)
   | Some (original, fresh) -> return (Some original, Some fresh)
   ;; *)

(* let update_args_with_fresh_action (args : constructor_args)
  : EConstr.t option -> constructor_args
  = function
  | None -> args
  | Some fresh -> { args with act = fresh }
;; *)

(* let update_args_action (args : constructor_args) : constructor_args mm =
  state (fun env sigma ->
    let sigma, act = check_fresh_econstr env sigma args.act in
    sigma, { args with act })
;; *)

exception ConstructorNameNotRecognized of (EConstr.t * EConstr.t)

(** Checks possible transitions for this term: *)
let rec check_valid_constructors
          (transitions : (Constr.rel_context * Constr.types) array)
          (indmap : Mebi_ind.t F.t)
          (from_term : EConstr.t)
          (act_term : EConstr.t)
          (lts_enc : Enc.t)
            (* : ((Evd.econstr * Evd.econstr) option list * Constructors.t) mm *)
  : Constructors.t mm
  =
  let* from_term : EConstr.t = Mebi_utils.econstr_normalize from_term in
  let* () = debug_validconstrs_start from_term in
  let iter_body
        (i : int)
        (* ((fresh_act_ops,constructors) : ((Evd.econstr * Evd.econstr) option list *Constructors.t)) *)
          (constructors : Constructors.t)
    =
    let* () = debug_validconstrs_iter_start i constructors in
    let (ctx, tm) : Constr.rel_context * Constr.t = transitions.(i) in
    let decls : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in
    let* substl = mk_ctx_substl [] (List.rev decls) in
    let* args : constructor_args = extract_args ~substl tm in
    let* act_term : EConstr.t = get_fresh_evar (TypeOf act_term) in
    (* NOTE: make fresh act_term *)
    (* let* fresh_act_opt = make_fresh_action_opt act_term in *)
    (*  *)
    (*  *)
    (* let act_term:EConstr.t = match act_term with | None -> (
      state (fun env sigma -> let sigma, fresh = Rocq_utils.get_next env sigma original in args.act)) *)
    (*  *)
    (*  *)
    (*  *)
    (*  *)
    (* let* original_act, fresh_act = unpack_fresh_action_opt fresh_act_opt in *)
    (* let* success = try_unify_constructor_args from_term fresh_act args in *)
    let* success = try_unify_constructor_args from_term (Some act_term) args in
    if success
    then (
      (* let* args : constructor_args = update_args_action args in *)
      (* let* () =
        Option.cata
          (fun fresh_act ->
            Rocq_debug.debug_econstr_mm "A CVC fresh_act" fresh_act)
          (Rocq_debug.debug_econstr_mm "A CVC args.act" args.act)
          fresh_act
      in *)
      (* let args = update_args_with_fresh_action args fresh_act in *)
      let* () = Rocq_debug.debug_econstr_mm "A CVC args.act" args.act in
      let args = { args with act = act_term } in
      let* () = Rocq_debug.debug_econstr_mm "B CVC args.act" args.act in
      let* constructors : Constructors.t =
        explore_valid_constructor
          indmap
          from_term
          (* fresh_act *)
          lts_enc
          args
          (i, constructors)
          (substl, decls)
      in
      Logging.Log.debug "VC fresh act fix:";
      (* let* constructors : Constructors.t =
         update_constructors_fresh_action constructors fresh_act_opt
         in *)
      let* () = debug_constructors_mm constructors in
      let* () = debug_validconstrs_iter_close i constructors in
      (* return (fresh_act_opt::fresh_act_ops, constructors) *)
      return constructors)
    else
      let* () = debug_validconstrs_iter_close i constructors in
      (* return (fresh_act_ops,constructors) *)
      return constructors
  in
  let*
    (* (fresh_act_ops,constructors) *)
      constructors
    =
    iterate 0 (Array.length transitions - 1) [] (* ([],[]) *) iter_body
  in
  let* () = debug_validconstrs_close from_term constructors in
  (* return (fresh_act_ops,constructors) *)
  return constructors

(** *)
and explore_valid_constructor
      (indmap : Mebi_ind.t F.t)
      (from_term : EConstr.t)
      (* (act_term : EConstr.t option) *)
        (lts_enc : Enc.t)
      (args : constructor_args)
      ((i, constructors) : int * Constructors.t)
      ((substl, decls) : EConstr.Vars.substl * EConstr.rel_declaration list)
  : Constructors.t mm
  =
  let* () =
    debug_validconstrs_iter_success_start from_term (Some args.act) args
  in
  (* NOTE: if args.act is evar then make fresh *)
  (* let* args = update_args_action args in *)
  (* let* fresh_act_opt = make_fresh_action_opt act_term in *)
  (* let* original_act, fresh_act = unpack_fresh_action_opt fresh_act_opt in *)
  let* act : EConstr.t = Mebi_utils.econstr_normalize args.act in
  let tgt_term : EConstr.t = EConstr.Vars.substl substl args.rhs in
  let* tgt_term : EConstr.t = Mebi_utils.econstr_normalize tgt_term in
  (*  *)
  (*  *)
  (*  *)
  (* TODO: we need to somehow obtain all of the actions that [act] turns into *)
  (* TODO: do we: *)
  (* TODO:  (1)  extend the problems themselves to contain them, or extend updated_ctx to return them? *)
  (*  *)
  (*  *)
  (*  *)
  let* () = Rocq_debug.debug_econstr_mm "A EVC act" act in
  let* next_constructor_problems : (Enc.t * Problems.t list) option =
    (* let* next_constructor_problems : (Enc.t * labelled_problem list list) option = *)
    (* check_updated_ctx lts_enc [ [] ] indmap (substl, decls) *)
    (* check_updated_ctx lts_enc [ None, [] ] indmap (substl, decls) *)
    (* check_updated_ctx lts_enc act [ [] ] indmap (substl, decls) *)
    check_updated_ctx lts_enc [ [] ] indmap (substl, decls)
  in
  let* () = Rocq_debug.debug_econstr_mm "B EVC act" act in
  let* constructors =
    check_for_next_constructors
      i
      indmap
      act
      tgt_term
      constructors
      next_constructor_problems
  in
  (* let* constructors : Constructors.t =
     update_constructors_fresh_action constructors fresh_act_opt
     in *)
  Logging.Log.debug
    (Printf.sprintf "V constructors: %i" (List.length constructors));
  let* () = debug_validconstrs_iter_success_close from_term (Some act) args in
  (* NOTE: update our sigma with the successful unifications *)
  (* TODO: *)
  return constructors

(* Should return a list of unification problems *)
and check_updated_ctx
      (lts_enc : Enc.t)
      (* (outer_act:EConstr.t) *)
      (* (acc : (EConstr.t option * Problems.t) list) *)
      (* (acc : labelled_problem list list) *)
        (acc : Problems.t list)
      (indmap : Mebi_ind.t F.t)
  :  EConstr.Vars.substl * EConstr.rel_declaration list
     (* -> (Enc.t * labelled_problem list list) option mm *)
  -> (Enc.t * Problems.t list) option mm
  = function
  | [], [] ->
    let* () = debug_updtcontext_return () in
    return (Some (lts_enc, acc))
  | _hsubstl :: substl, t :: tl ->
    (* let* () = debug_updtcontext_start () in *)
    let ty_t : EConstr.t = Context.Rel.Declaration.get_type t in
    let$+ upd_t env sigma = EConstr.Vars.substl substl ty_t in
    let* env = get_env in
    let* sigma = get_sigma in
    (match EConstr.kind sigma upd_t with
     | App (name, args) ->
       (match F.find_opt indmap name with
        | None ->
          (* let* () = debug_updtcontext_close_app name in *)
          check_updated_ctx lts_enc acc indmap (substl, tl)
        | Some c ->
          let args : constructor_args = constructor_args args in
          let$+ lhs env sigma = Reductionops.nf_evar sigma args.lhs in
          let$+ act env sigma = Reductionops.nf_evar sigma args.act in
          let args = { args with lhs; act } in
          let* next_lts = Mebi_ind.get_constr_transitions c in
          let* next_constructors : Constructors.t =
            check_valid_constructors next_lts indmap lhs act c.enc
          in
          (match next_constructors with
           | [] ->
             (* let* () =
                debug_updtcontext_close_app_known name c next_constructors
                in *)
             return None
           | next_constructors ->
             (* TODO: extract the actions from the next-constructors and incoorporate them within the *)
             let* () = Rocq_debug.debug_econstr_mm "CTX act" act in
             Logging.Log.notice "\n";
             Logging.Log.debug "ACTFIX:";
             Logging.Log.debug (Printf.sprintf "ACC: %i" (List.length acc));
             let* () = debug_constructors_mm next_constructors in
             let* problems : Problems.t = map_problems args next_constructors in
             (* Logging.Log.debug
               (Printf.sprintf "PROBLEMS: %i" (List.length problems)); *)
             (* let* () = debug_labelled_problem_list_mm problems in *)
             (* let* acc  = labelled_cross_product acc problems in *)
             let acc : Problems.t list = cross_product acc problems in
             (* Logging.Log.debug
               (Printf.sprintf "POST CROSS PRODUCT: %i" (List.length acc)); *)
             (* let* () = debug_labelled_problems_list_mm acc in *)
             let* acc = check_updated_ctx lts_enc acc indmap (substl, tl) in
             (* let* () =
                debug_updtcontext_close_app_known name c next_constructors
                in *)
             return acc))
     | _ ->
       let* acc = check_updated_ctx lts_enc acc indmap (substl, tl) in
       (* let* () = debug_updtcontext_close upd_t None [] in *)
       return acc)
  | _substl, _ctxl -> invalid_check_updated_ctx _substl _ctxl

and check_for_next_constructors
      (i : int)
      (indmap : Mebi_ind.t F.t)
      (outer_act : EConstr.t)
      (tgt_term : EConstr.t)
      (constructors : Constructors.t)
        (* : (Enc.t * labelled_problem list list) option -> Constructors.t mm *)
  : (Enc.t * Problems.t list) option -> Constructors.t mm
  = function
  | None ->
    let* () = debug_nextconstrs_return () in
    return constructors
  (* | Some (next_lts_enc, next_labelled_problems) -> *)
  | Some (next_lts_enc, next_problems) ->
    (match next_problems with
     | [ [] ] ->
       let* sigma = get_sigma in
       if EConstr.isEvar sigma tgt_term
       then return constructors
       else (
         let tree = Mebi_constr.Tree.Node ((next_lts_enc, i), []) in
         let constructors = (outer_act, tgt_term, tree) :: constructors in
         return constructors)
     | next_labelled_problems ->
       (* let iter_body (j : int) (acc : Constructors.t) : Constructors.t mm =
          (* let next_act, next_problems = List.nth next_labelled_problems j in *)
          Constructors.retrieve
          ~debug:true
          i
          acc
          outer_act (* next_act *)
          tgt_term
          (* (next_lts_enc, [ next_problems ]) *)
          (next_lts_enc, next_problems)
          in
          iterate 0 (List.length next_labelled_problems - 1) constructors iter_body *)
       let* constructors =
         Constructors.retrieve
           ~debug:true
           i
           constructors
           outer_act
           tgt_term
           (next_lts_enc, next_problems)
       in
       Logging.Log.debug
         (Printf.sprintf "N constructors: %i" (List.length constructors));
       let* () = debug_nextconstrs_close next_problems None constructors in
       return constructors)
;;

(* let* act = state (fun env sigma -> check_fresh_econstr env sigma act) in *)
(* let* () = debug_nextconstrs_start () in
let* () = Rocq_debug.debug_econstr_mm "CNC act" act in
match next_problems with
| [ [] ] ->
  let* sigma = get_sigma in
  if EConstr.isEvar sigma tgt_term
  then
    let* () = debug_nextconstrs_close next_problems (Some true) constructors in
    return constructors
  else (
    let tree = Mebi_constr.Tree.Node ((next_lts_enc, i), []) in
    let constructors = (act, tgt_term, tree) :: constructors in
    let* () = debug_nextconstrs_close next_problems (Some false) constructors in
    return constructors)
| _ ->
  Logging.Log.debug "NC constructors:";
  let* () = debug_constructors_mm constructors in
  Logging.Log.debug "NC problems list:";
  let* () = debug_problems_list_mm next_problems in
  let* constructors =
    Constructors.retrieve
      ~debug:true
      i
      constructors
      act
      tgt_term
      (next_lts_enc, next_problems)
  in
  Logging.Log.debug
    (Printf.sprintf "N constructors: %i" (List.length constructors));
  let* () = debug_nextconstrs_close next_problems None constructors in
  return constructors
;; *)

(* ! Impossible ! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

let collect_valid_constructors
      (transitions : (Constr.rel_context * Constr.types) array)
      (indmap : Mebi_ind.t F.t)
      (from_term : EConstr.t)
      (label_type : EConstr.t)
      (* (act_term : EConstr.t option) *)
        (lts_enc : Enc.t)
  : Constructors.t mm
  =
  let* fresh_evar = get_fresh_evar (Rocq_utils.OfType label_type) in
  let* constructors : Constructors.t =
    check_valid_constructors transitions indmap from_term fresh_evar lts_enc
  in
  Logging.Log.notice "\n=/==/=/=/==/===";
  let* () = debug_constructors_mm constructors in
  return constructors
;;
