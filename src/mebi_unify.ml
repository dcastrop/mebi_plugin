(* open Mebi_debug *)
open Mebi_wrapper
open Mebi_wrapper.Syntax
open Mebi_unification

type constructor_args = Rocq_utils.constructor_args

let constructor_args : EConstr.t array -> constructor_args =
  Rocq_utils.constructor_args
;;

let get_fresh_evar : Rocq_utils.evar_source -> EConstr.t mm =
  Mebi_utils.get_fresh_evar
;;

let mk_ctx_substl
  :  EConstr.Vars.substl
  -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
  -> EConstr.Vars.substl mm
  =
  Mebi_utils.mk_ctx_substl
;;

let extract_args
  : ?substl:EConstr.Vars.substl -> Constr.t -> constructor_args mm
  =
  Mebi_utils.extract_args
;;

(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

(** creates unification problems between the rhs of the current constructor and the lhs of the next, along with the actions of both.
    (* NOTE: this is only relevant when deciding whether to explore a given constructor from a premise of another *)
*)
let constr_to_problem (args : constructor_args) : Mebi_constr.t -> Problem.t =
  Log.trace __FUNCTION__;
  function
  | act, rhs, tree ->
    let act : Pair.t = { a = args.act; b = act } in
    let goto : Pair.t = { a = args.rhs; b = rhs } in
    { act; goto; tree }
;;

let map_problems args (constructors : Constructors.t) : Problems.t mm =
  Log.trace __FUNCTION__;
  let* sigma = get_sigma in
  let to_unify : Problem.t list =
    List.map (constr_to_problem args) constructors
  in
  let p : Problems.t = { sigma; to_unify } in
  return p
;;

let cross_product (acc : Problems.t list) ({ sigma; to_unify } : Problems.t)
  : Problems.t list
  =
  Log.trace __FUNCTION__;
  List.concat_map
    (fun ({ to_unify = xs; _ } : Problems.t) : Problems.t list ->
      List.map
        (fun (y : Problem.t) : Problems.t -> { sigma; to_unify = y :: xs })
        to_unify)
    acc
;;

(*********************************************************)

let does_constructor_unify (a : EConstr.t) (b : EConstr.t) : bool mm =
  Log.trace __FUNCTION__;
  let open Mebi_unification in
  state (fun env sigma -> Pair.unify env sigma (Pair.normal a b))
;;

let check_constructor_args_unify
      (lhs : EConstr.t)
      (act : EConstr.t)
      (args : constructor_args)
  : bool mm
  =
  Log.trace __FUNCTION__;
  let f = does_constructor_unify in
  let* lhs_unifies : bool = f args.lhs lhs in
  if lhs_unifies then f args.act act else return false
;;

(*********************************************************)

(* let debug_extract_args name : constructor_args -> unit mm =
  Log.trace __FUNCTION__;
  function
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
;; *)

let axiom_constructor
      (act : EConstr.t)
      (tgt : EConstr.t)
      (constructor_index : Enc.t * int)
      (constructors : Constructors.t)
  : Constructors.t mm
  =
  Log.trace __FUNCTION__;
  let* is_evar : bool = Mebi_utils.econstr_is_evar tgt in
  if is_evar
  then return constructors
  else
    let open Mebi_constr in
    let tree : Tree.t = Tree.Node (constructor_index, []) in
    let axiom : Mebi_constr.t = act, tgt, tree in
    return (axiom :: constructors)
;;

(** Checks possible transitions for this term: *)
let rec check_valid_constructors
          (constructors : Mebi_ind.lts_constructor array)
          (indmap : Mebi_ind.t F.t)
          (from_term : EConstr.t)
          (act_term : EConstr.t)
          (lts_enc : Enc.t)
  : Constructors.t mm
  =
  Log.trace __FUNCTION__;
  let* from_term : EConstr.t = Mebi_utils.econstr_normalize from_term in
  (* let* () = debug_validconstrs_start from_term in *)
  let iter_body (i : int) (acc : Constructors.t) : Constructors.t mm =
    (* let* () = debug_validconstrs_iter_start i constructors in *)
    (* NOTE: extract args for constructor *)
    let { constructor = ctx, tm; _ } : Mebi_ind.lts_constructor =
      constructors.(i)
    in
    let decls : Rocq_utils.econstr_decl list =
      Rocq_utils.get_econstr_decls ctx
    in
    let* substl = mk_ctx_substl [] (List.rev decls) in
    let* args : constructor_args = extract_args ~substl tm in
    (* NOTE: make fresh [act_term] to avoid conflicts with sibling constructors *)
    let* act_term : EConstr.t = get_fresh_evar (TypeOf act_term) in
    let* success = check_constructor_args_unify from_term act_term args in
    if success
    then (
      (* NOTE: replace [act] with the fresh [act_term] *)
      let fresh_args : constructor_args = { args with act = act_term } in
      let* acc : Constructors.t =
        explore_valid_constructor
          indmap
          from_term
          lts_enc
          fresh_args
          (i, acc)
          (substl, decls)
      in
      Log.debug ~__FUNCTION__ "CVC constructors:";
      (* ! NOTE: here we obtain the successfully unified and distinct action and destination -- BUT as this is returned, we see that it is actually another evar and this then unifies incorrectly. *)
      (* let* () = debug_constructors_mm constructors in *)
      (* let* () = debug_validconstrs_iter_close i constructors in *)
      return acc)
    else
      (* let* () = debug_validconstrs_iter_close i constructors in *)
      return acc
  in
  let* constructors = iterate 0 (Array.length constructors - 1) [] iter_body in
  (* let* () = debug_validconstrs_close from_term constructors in *)
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
  Log.trace __FUNCTION__;
  (* let* () =
    debug_validconstrs_iter_success_start from_term (Some args.act) args
  in *)
  (* NOTE: unpack and normalize [act] and [tgt] from [args] *)
  let tgt : EConstr.t = EConstr.Vars.substl substl args.rhs in
  let* tgt : EConstr.t = Mebi_utils.econstr_normalize tgt in
  let* act : EConstr.t = Mebi_utils.econstr_normalize args.act in
  (* TODO: make fresh sigma from fresh act+tgt, and use that from this point onwards. then, during cross-product, make sure that it is used over the parents sigma stored within the unification problems during ctx *)
  (* let* constructors =
    sandbox *)
  let* empty_problems : Problems.t = Problems.empty () in
  let* next_constructor_problems : (Enc.t * Problems.t list) option =
    check_updated_ctx lts_enc [ empty_problems ] indmap (substl, decls)
  in
  let* constructors =
    check_for_next_constructors i act tgt constructors next_constructor_problems
  in
  (* let* () = debug_validconstrs_iter_success_close from_term (Some act) args in *)
  return constructors

(* Should return a list of unification problems *)
and check_updated_ctx
      (lts_enc : Enc.t)
      (acc : Problems.t list)
      (indmap : Mebi_ind.t F.t)
  :  EConstr.Vars.substl * EConstr.rel_declaration list
  -> (Enc.t * Problems.t list) option mm
  =
  Log.trace __FUNCTION__;
  function
  | [], [] ->
    (* let* () = debug_updtcontext_return () in *)
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
          let next_lts : Mebi_ind.lts_constructor array =
            Mebi_ind.get_lts_constructor_types c
          in
          let* next_constructors : Constructors.t =
            check_valid_constructors next_lts indmap lhs act c.enc
          in
          (match next_constructors with
           | [] ->
             (* let* () = debug_updtcontext_close_app_known name c next_constructors in *)
             return None
           | next_constructors ->
             (* let* () = Rocq_debug.debug_econstr_mm "CTX act" act in *)
             (* ! NOTE: *)
             (* let* () = debug_constructors_mm next_constructors in *)
             let* problems : Problems.t = map_problems args next_constructors in
             (* let* () = debug_problems_mm problems in *)
             let acc : Problems.t list = cross_product acc problems in
             (* let* () = debug_problems_list_mm acc in *)
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
      (outer_act : EConstr.t)
      (tgt_term : EConstr.t)
      (constructors : Constructors.t)
  : (Enc.t * Problems.t list) option -> Constructors.t mm
  =
  Log.trace __FUNCTION__;
  function
  | None ->
    (* let* () = debug_nextconstrs_return () in *)
    return constructors
  | Some (next_lts_enc, next_problems) ->
    (* let* () = debug_nextconstrs_start () in *)
    if Problems.list_is_empty next_problems
    then (
      Log.debug ~__FUNCTION__ "CNC axiom";
      let* constructors =
        axiom_constructor outer_act tgt_term (next_lts_enc, i) constructors
      in
      (* let* () = debug_nextconstrs_close next_problems None constructors in *)
      return constructors)
    else (
      Log.debug ~__FUNCTION__ "CNC premises";
      let* constructors : Mebi_constr.t list =
        Constructors.retrieve
          i
          constructors
          outer_act
          tgt_term
          (next_lts_enc, next_problems)
      in
      (* let* () = debug_nextconstrs_close next_problems None constructors in *)
      return constructors)
;;

let collect_valid_constructors
      (constructors : Mebi_ind.lts_constructor array)
      (indmap : Mebi_ind.t F.t)
      (from_term : EConstr.t)
      (label_type : EConstr.t)
      (lts_enc : Enc.t)
  : Constructors.t mm
  =
  Log.trace __FUNCTION__;
  Log.thing ~__FUNCTION__ Debug "from" from_term (Of econstr_to_string);
  let* fresh_evar = get_fresh_evar (Rocq_utils.OfType label_type) in
  let* constructors : Constructors.t =
    check_valid_constructors constructors indmap from_term fresh_evar lts_enc
  in
  (* .Log.notice "\n=/==/=/=/==/==="; *)
  (* let* () = debug_constructors_mm constructors in *)
  return constructors
;;
