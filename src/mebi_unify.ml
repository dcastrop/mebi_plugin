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

(** creates unification problems between the rhs of the current constructor and the lhs of the next.
*)
let map_constr_to_problem args : Mebi_constr.t -> Problem.t mm = function
  | act, lhs, tree ->
    let* pair : Pair.t = map_constr_to_pair args.rhs lhs in
    return (act, pair, tree)
;;

let map_problems args (constructors : Constructors.t) : Problems.t mm =
  let iter_body (i : int) (acc : Problems.t) : Problems.t mm =
    let constructor : Mebi_constr.t = List.nth constructors i in
    let* problem : Problem.t = map_constr_to_problem args constructor in
    return (problem :: acc)
  in
  iterate 0 (List.length constructors - 1) [] iter_body
;;

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

let get_fresh_evar (original : Rocq_utils.evar_source) : EConstr.t mm =
  state (fun env sigma -> Rocq_utils.get_next env sigma original)
;;

(** Checks possible transitions for this term: *)
let rec check_valid_constructors
          (transitions : (Constr.rel_context * Constr.types) array)
          (indmap : Mebi_ind.t F.t)
          (from_term : EConstr.t)
          (act_term : EConstr.t)
          (lts_enc : Enc.t)
  : Constructors.t mm
  =
  let* from_term : EConstr.t = Mebi_utils.econstr_normalize from_term in
  let* () = debug_validconstrs_start from_term in
  let iter_body (i : int) (constructors : Constructors.t) =
    let* () = debug_validconstrs_iter_start i constructors in
    let (ctx, tm) : Constr.rel_context * Constr.t = transitions.(i) in
    let decls : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in
    let* substl = mk_ctx_substl [] (List.rev decls) in
    let* args : constructor_args = extract_args ~substl tm in
    (* NOTE: make fresh [act_term] *)
    let* act_term : EConstr.t = get_fresh_evar (TypeOf act_term) in
    let* success = try_unify_constructor_args from_term (Some act_term) args in
    if success
    then (
      (* NOTE: replace [act] with the fresh [act_term] *)
      let args : constructor_args = { args with act = act_term } in
      let* constructors : Constructors.t =
        explore_valid_constructor
          indmap
          from_term
          lts_enc
          args
          (i, constructors)
          (substl, decls)
      in
      Logging.Log.debug "CVC constructors:";
      let* () = debug_constructors_mm constructors in
      let* () = debug_validconstrs_iter_close i constructors in
      return constructors)
    else
      let* () = debug_validconstrs_iter_close i constructors in
      return constructors
  in
  let* constructors = iterate 0 (Array.length transitions - 1) [] iter_body in
  let* () = debug_validconstrs_close from_term constructors in
  return constructors

(** *)
and explore_valid_constructor
      (indmap : Mebi_ind.t F.t)
      (from_term : EConstr.t)
      (lts_enc : Enc.t)
      (args : constructor_args)
      ((i, constructors) : int * Constructors.t)
      ((substl, decls) : EConstr.Vars.substl * EConstr.rel_declaration list)
  : Constructors.t mm
  =
  let* () =
    debug_validconstrs_iter_success_start from_term (Some args.act) args
  in
  (* NOTE: unpack and normalize [act] and [tgt] from [args] *)
  let tgt : EConstr.t = EConstr.Vars.substl substl args.rhs in
  let* tgt : EConstr.t = Mebi_utils.econstr_normalize tgt in
  let* act : EConstr.t = Mebi_utils.econstr_normalize args.act in
  (* ! NOTE: any next consturctors will replace [act] with their own fresh evar *)
  let* () = Rocq_debug.debug_econstr_mm "A EVC act" act in
  let* next_constructor_problems : (Enc.t * Problems.t list) option =
    check_updated_ctx lts_enc [ [] ] indmap (substl, decls)
  in
  (* ! NOTE: we only have the initial [act] to use for the constructors *)
  let* () = Rocq_debug.debug_econstr_mm "B EVC act" act in
  (* NOTE: that is why we now store the action within the problems *)
  let* constructors =
    check_for_next_constructors
      i
      indmap
      act
      tgt
      constructors
      next_constructor_problems
  in
  let* () = debug_validconstrs_iter_success_close from_term (Some act) args in
  return constructors

(* Should return a list of unification problems *)
and check_updated_ctx
      (lts_enc : Enc.t)
      (acc : Problems.t list)
      (indmap : Mebi_ind.t F.t)
  :  EConstr.Vars.substl * EConstr.rel_declaration list
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
             (* let* () = debug_updtcontext_close_app_known name c next_constructors in *)
             return None
           | next_constructors ->
             (* TODO: extract the actions from the next-constructors and incoorporate them within the *)
             (* Logging.Log.notice "\n"; *)
             let* () = Rocq_debug.debug_econstr_mm "CTX act" act in
             let* () = debug_constructors_mm next_constructors in
             let* problems : Problems.t = map_problems args next_constructors in
             let* () = debug_problems_mm problems in
             let acc : Problems.t list = cross_product acc problems in
             let* () = debug_problems_list_mm acc in
             let* acc = check_updated_ctx lts_enc acc indmap (substl, tl) in
             return acc))
     | _ ->
       let* acc = check_updated_ctx lts_enc acc indmap (substl, tl) in
       (* let* () = debug_updtcontext_close upd_t None [] in *)
       return acc)
  | _substl, _ctxl -> invalid_check_updated_ctx _substl _ctxl
(* ! Impossible ! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

and check_for_next_constructors
      (i : int)
      (indmap : Mebi_ind.t F.t)
      (outer_act : EConstr.t)
      (tgt_term : EConstr.t)
      (constructors : Constructors.t)
  : (Enc.t * Problems.t list) option -> Constructors.t mm
  = function
  | None ->
    let* () = debug_nextconstrs_return () in
    return constructors
  | Some (next_lts_enc, next_problems) ->
    let* () = Rocq_debug.debug_econstr_mm "CNC act" outer_act in
    (match next_problems with
     | [ [] ] ->
       let* sigma = get_sigma in
       if EConstr.isEvar sigma tgt_term
       then return constructors
       else (
         let tree = Mebi_constr.Tree.Node ((next_lts_enc, i), []) in
         let constructors = (outer_act, tgt_term, tree) :: constructors in
         return constructors)
     | next_problems ->
       Logging.Log.debug "NC constructors:";
       let* () = debug_constructors_mm constructors in
       Logging.Log.debug "NC problems list:";
       let* () = debug_problems_list_mm next_problems in
       let* constructors =
         Constructors.retrieve
           ~debug:true
           i
           constructors
           outer_act
           tgt_term
           (next_lts_enc, next_problems)
       in
       let* () = debug_nextconstrs_close next_problems None constructors in
       return constructors)
;;

let collect_valid_constructors
      (transitions : (Constr.rel_context * Constr.types) array)
      (indmap : Mebi_ind.t F.t)
      (from_term : EConstr.t)
      (label_type : EConstr.t)
      (lts_enc : Enc.t)
  : Constructors.t mm
  =
  let* fresh_evar = get_fresh_evar (Rocq_utils.OfType label_type) in
  let* constructors : Constructors.t =
    check_valid_constructors transitions indmap from_term fresh_evar lts_enc
  in
  (* Logging.Log.notice "\n=/==/=/=/==/==="; *)
  let* () = debug_constructors_mm constructors in
  return constructors
;;
