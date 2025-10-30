open Logging
open Mebi_wrapper
open Mebi_wrapper.Syntax
open Mebi_utils

(** [snd] is a term (e.g., destination) that we want to check unifies with [fst] (which we have already reached).
  @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
type unification_pair = EConstr.t * EConstr.t

(** since it may contain [evars] and is related to unification, we [sandbox] to ensure that we don't unintentionally make any changes to [sigma].
*)
let debugstr_unification_pair ((a, b) : unification_pair) : string mm =
  sandbox
    (state (fun env sigma ->
       let f : EConstr.t -> string = Strfy.econstr env sigma in
       sigma, Printf.sprintf "(%s, %s)" (f a) (f b)))
;;

let debug_unification_pair ?(prefix = "") (p : unification_pair) : unit mm =
  let* s = debugstr_unification_pair p in
  return (Log.debug (Printf.sprintf "%s unification_pair: %s" prefix s))
;;

(** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
*)
type unification_problem = unification_pair * Mebi_constr.Tree.t

let debugstr_unification_problem ((p, tree) : unification_problem) : string mm =
  let* s = debugstr_unification_pair p in
  return (Printf.sprintf "(%s, %s)" s (Mebi_constr.Tree.to_string tree))
;;

let debug_unification_problem ?(prefix = "") (p : unification_problem) : unit mm
  =
  let* s = debugstr_unification_problem p in
  return (Log.debug (Printf.sprintf "%s unification_problem: %s" prefix s))
;;

let debug_unification_problem_list
      ?(prefix = "")
      (ps : unification_problem list)
  : unit mm
  =
  let iter_body (i : int) (acc : string list) =
    let* s = debugstr_unification_problem (List.nth ps i) in
    return (s :: acc)
  in
  let* sl : string list = iterate 0 (List.length ps - 1) [] iter_body in
  let s : string = Strfy.list Strfy.str sl in
  return (Log.debug (Printf.sprintf "%s unification_problem list: %s" prefix s))
;;

(** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
let unify ((a, b) : unification_pair) : bool mm =
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    let open Pretype_errors in
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL a b in
      sigma, true
    with
    | PretypeError (_, _, CannotUnify (_a, _b, _e)) -> sigma, false)
;;

(** [debug_unify a b] an alternate wrapper for [unify] that shows additional debugging information. @see [unify] for details. *)
let debug_unify ((a, b) : unification_pair) : bool mm =
  let* r : bool = unify (a, b) in
  let* () =
    state (fun env sigma ->
      let f : EConstr.t -> string = Strfy.econstr env sigma in
      Log.debug (Printf.sprintf "unify : %b\n- a: %s\n- b: %s" r (f a) (f b));
      sigma, ())
  in
  return r
;;

(** [do_terms_unify ?f a b] checks if terms [a] and [b] unify using [?f] within a sandbox (i.e., ensuring that the [sigma] of the state-monad is not updated).
    @return [true] if [a] and [b] do unify else [false]. *)
let do_terms_unify ?(f : unification_pair -> bool mm = unify) a b : bool mm =
  sandbox (f (a, b))
;;

(** [do_terms_unify_opt ?f a b] is the same as [do_terms_unify] except that [a] and [b] are [option].
    @return [true] if [Some a] and [Some b] unify else [false]. *)
let do_terms_unify_opt ?(f : unification_pair -> bool mm = unify) a b : bool mm =
  match a, b with
  | Some a, Some b -> do_terms_unify ~f a b
  | _, _ -> return false
;;

(** [try_unify_constructor_args ?f from_term action args]
    (*TODO: finish doc *)
    (* NOTE: the unification is within a [sandbox] -- which is different from the original implementation. *)
*)
let try_unify_constructor_args
      ?(f : unification_pair -> bool mm = unify)
      (from_term : EConstr.t)
      (action : EConstr.t option)
      ((lhs_term, act_term, _rhs_term) : EConstr.t * EConstr.t * EConstr.t)
  : bool mm
  =
  (* NOTE: these unification checks are within a [sandbox]. *)
  (* TODO: this is a new change, check the impact of this *)
  let* lhs_term_unifies : bool = do_terms_unify ~f from_term lhs_term in
  let* act_term_unifies : bool = do_terms_unify_opt ~f action (Some act_term) in
  (* if lhs_term_unifies && act_term_unifies then return true else return false *)
  return (lhs_term_unifies && act_term_unifies)
;;

(** [unify_all_opt ?f ps] calls [unify] on each unification problems in [ps], aborting at once if any fail to be unified.
    @param ?f
      is the function performing unification and is [unify] by default, but may be set to [debug_unify] for additional debug information.
    @param ps
      is the list of constructor trees and their corresponding unification problems.
    @return
      [None] if any unifications fail, or [Some list] containing the [Mebi_constr.Tree.t] of each of the corresponding successful unifications.
*)
let rec unify_all_opt ?(f : unification_pair -> bool mm = unify)
  : unification_problem list -> Mebi_constr.Tree.t list option mm
  = function
  | [] -> return (Some [])
  | (unif_prob, ctor_tree) :: t ->
    let* success = f unif_prob in
    if Bool.not success
    then return None
    else
      let* unified_all_opt = unify_all_opt ~f t in
      (match unified_all_opt with
       | None -> return None
       | Some ctor_trees -> return (Some (ctor_tree :: ctor_trees)))
;;

(** [sandbox_unify_all_opt tgt ps] calls [unify_all_opt ps] from inside of a [sandbox] (i.e., the state ([env], [sigma]) are restored afterwards) and wraps the [Some ctor_tree] result of [unify_all_opt] with the [tgt] term to indicate that [tgt] can be reached via application of any paths within [ctor_tree].
    @param tgt
      is the right-most term reached after applying any of the provided [Mebi_constr.Tree.t] trees. After unification, it will either remain an [evar] or will have become a concrete term (indicating whether we have reached, e.g., an "axiom").
      (* FIXME: , and we require a concrete term to be reached so that it may either (a) , or (b) be used as the left-most term of another transition (which we then check has any further ) *)
    @param ps
      is the list of constructor trees and their corresponding unification problems.
    @return
      [None] if [unify_all_opt] is also [None] or if [tgt] is an [Evar], else returns [Some (normalized_tgt, ctor_trees)].
*)
let sandbox_unify_all_opt (tgt : EConstr.t) (ps : unification_problem list)
  : (EConstr.t * Mebi_constr.Tree.t list) option mm
  =
  sandbox
    (let* unified_all_opt = unify_all_opt ps in
     match unified_all_opt with
     | None -> return None
     | Some ctor_trees ->
       let* term = econstr_normalize tgt in
       let* is_undefined = econstr_is_evar term in
       if is_undefined then return None else return (Some (term, ctor_trees)))
;;

(** [build_constrs acc ctor_index act tgt (lts_index, ps)] is ...
    (* TODO: document *)
    @param ctor_index is the constructor index (from 0).
    @param act is the action (label) that is leads to some [tgt] term.
    @param tgt
      is the term reached by applying constructor [ctor_index] to some term with [act], and will at first be an [evar] but then resolve to some concrete [econstr].
    @return a list containing the
    @see retrieve_tgt_nodes  *)
let rec build_constrs
          (acc : Mebi_constr.t list)
          (ctor_index : int)
          (act : EConstr.t)
          (tgt : EConstr.t)
  : int * unification_problem list list -> Mebi_constr.t list mm
  = function
  | _, [] -> return acc
  | lts_index, unif_probs :: unif_probs_list ->
    let* success = sandbox_unify_all_opt tgt unif_probs in
    (match success with
     | None -> build_constrs acc ctor_index act tgt (lts_index, unif_probs_list)
     | Some (unified_tgt, ctor_trees) ->
       let* act = econstr_normalize act in
       let open Mebi_constr.Tree in
       let tree = Node ((Enc.of_int lts_index, ctor_index), ctor_trees) in
       let ctor = act, tgt, tree in
       let acc = ctor :: acc in
       build_constrs acc ctor_index act tgt (lts_index, unif_probs_list))
;;

(*****************************************************************************)

(** [mk_ctx_subst ?substl x] returns a new [evar] made from the type of [x], using any [substl] provided.
    @param ?substl
      is a list of substitutions, (* TODO: provided so that collisions don't occur? *)
    @param x
      corresponds to a (* TODO: universally? *) quantified term of a constructor.
    @return a new [evar] for [x]. *)
let mk_ctx_subst
      ?(substl : EConstr.Vars.substl = [])
      (x : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt)
  : EConstr.t mm
  =
  state (fun env sigma ->
    let ty : EConstr.t = Context.Rel.Declaration.get_type x in
    let subst : EConstr.t = EConstr.Vars.substl substl ty in
    Evarutil.new_evar env sigma subst)
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
    let* vt = mk_ctx_subst ~substl:acc t in
    mk_ctx_substl (vt :: acc) ts
;;

(** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
    @param ?substl
      is a list of substitutions applied to the terms prior to being returned.
    @param term
      must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
    @return a triple of [lhs_term, action, rhs_term]. *)
let extract_args ?(substl : EConstr.Vars.substl = []) (term : Constr.t)
  : (EConstr.t * EConstr.t * EConstr.t) mm
  =
  match Constr.kind term with
  | App (_fn, args) ->
    if Array.length args == 3
    then (
      let args = EConstr.of_constr_array args in
      let args = Array.map (EConstr.Vars.substl substl) args in
      return (args.(0), args.(1), args.(2)))
    else (* TODO: err *) invalid_lts_args_length (Array.length args)
  | _ -> (* TODO: err *) invalid_lts_term_kind term
;;

(* type constr_data = { lts_index : int;ctor_index : int  } *)

type data =
  { ind_map : Mebi_ind.t F.t
  ; lts_index : int
  ; mutable acc : unification_problem list list
  }

exception ConstructorNameNotRecognized of (EConstr.t * EConstr.t)

let fail_if_unrecognized_constructor : bool = true

exception InductiveKindNotLTS of Mebi_ind.t

(** [get_constrs_opt x d] is [Some constrs] if [x] is an [EConstr.t] term corresponding to an inductively defined LTS that has been passed to the plugin as an argument, and already processed and stored within [d.ind_map], otherwise is [None].
    @param [x]
      is an [EConstr.t] term assumed to be derived from some declaration within the context of a constructor of an inductively defined LTS.
    @param [d] is a record [data].
    @return
      [None] if [x] does not correspond to an application (i.e., [Constr.App(fn, args)]) else [Some constrs].
    @raise ConstructorNameNotRecognized
      if [fail_if_unrecognized_constructor] is set to true, otherwise returns [None].
    @raise InductiveKindNotLTS if [x] does not *)
let get_ind_constrs_opt (x : EConstr.t) (d : data)
  : Rocq_utils.ind_constrs option mm
  =
  state (fun env sigma ->
    match EConstr.kind sigma x with
    | App (name, args) ->
      (match F.find_opt d.ind_map name with
       | None ->
         if fail_if_unrecognized_constructor
         then (* TODO: err *) raise (ConstructorNameNotRecognized (x, name))
         else sigma, None
       | Some ctor ->
         (match ctor.kind with
          | Mebi_ind.LTS l -> sigma, Some l.constr_transitions
          | _ -> (* TODO: err *) raise (InductiveKindNotLTS ctor)))
    | _ -> sigma, None)
;;

let debug_ind_constr p (x : Rocq_utils.ind_constr) : unit mm =
  state (fun env sigma ->
    let constr_str : string = Strfy.ind_constr env sigma x in
    ( sigma
    , Log.debug (Printf.sprintf "%sind_constr:\n%s" (Utils.prefix p) constr_str)
    ))
;;

let debug_ind_constrs p (x : Rocq_utils.ind_constrs) : unit mm =
  state (fun env sigma ->
    let constrs_str : string = Strfy.ind_constrs env sigma x in
    ( sigma
    , Log.debug
        (Printf.sprintf "%sind_constrs:\n%s" (Utils.prefix p) constrs_str) ))
;;

let debug_ind_constrs_opt p (x : Rocq_utils.ind_constrs option) : unit mm =
  state (fun env sigma ->
    ( sigma
    , match x with
      | None ->
        Log.debug (Printf.sprintf "%sind_constrs_opt is None" (Utils.prefix p))
      | Some constrs ->
        let constrs_str = Strfy.ind_constrs env sigma constrs in
        Log.debug
          (Printf.sprintf
             "%sind_constrs_opt is Some:\n%s"
             (Utils.prefix p)
             constrs_str) ))
;;

let debug_args
      p
      ((lhs_term, act_term, rhs_term) : EConstr.t * EConstr.t * EConstr.t)
  : unit mm
  =
  state (fun env sigma ->
    let f = Strfy.econstr env sigma in
    let arg_str =
      Strfy.list
        (Strfy.tuple Strfy.str f)
        [ "lhs", lhs_term; "act", act_term; "rhs", rhs_term ]
    in
    sigma, Log.debug (Printf.sprintf "%sargs:\n%s" (Utils.prefix p) arg_str))
;;

let make_constr_tree (constr_index : int) (d : data) =
  Mebi_constr.Tree.Node ((Enc.of_int d.lts_index, constr_index), [])
;;

(** [collect_valid_constructors from_term constrs d] ...
(* TODO: finish doc *)
@see [check_valid_constructor] *)
let rec collect_valid_constructors
          ?(action : EConstr.t option = None)
          (from_term : EConstr.t)
          (constrs : Rocq_utils.ind_constrs)
          (d : data)
  : Mebi_constr.t list mm
  =
  let* () = debug_ind_constrs "Checking new" constrs in
  let* from_term : EConstr.t = Mebi_utils.econstr_normalize from_term in
  let iter_body (i : int) (acc : Mebi_constr.t list) : Mebi_constr.t list mm =
    let (ctx, term) : Constr.rel_context * Constr.t = constrs.(i) in
    let* () = debug_ind_constr (Printf.sprintf "Checking (%i)" i) (ctx, term) in
    let decls : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in
    let* substl : EConstr.Vars.substl = mk_ctx_substl [] (List.rev decls) in
    let* args = extract_args ~substl term in
    (* TODO: refactor from mk_ctx_substl, understand *)
    let* terms_unify = try_unify_constructor_args from_term action args in
    if terms_unify
    then collect_next_constructors acc args d (substl, decls)
    else return acc
  in
  iterate 0 (Array.length constrs - 1) [] iter_body

(** [collect_next_constructors acc args d context]
    (* TODO: finish doc *)
    (*NOTE: essnetially a [check_next_constructors] with the call to [check_update_ctx] handled inside (via [update_sigma]) *)
    @return
      [acc] updated with any additional constructors that must be applied from the current constructor [args].
*)
and collect_next_constructors
      ((constr_index, acc) : int * Mebi_constr.t list)
      ((lhs_term, act_term, rhs_term) : EConstr.t * EConstr.t * EConstr.t)
      (d : data)
      (context : EConstr.Vars.substl * EConstr.rel_declaration list)
  : Mebi_constr.t list mm
  =
  let* new_data = update_sigma d context in
  match new_data with
  (* NOTE: this constructor cannot be fully applied (some failure later on) *)
  | None -> return acc
  (* NOTE: constructor applies without any further constructors *)
  | Some { acc = []; _ } ->
    state (fun env sigma ->
      if EConstr.isEvar sigma rhs_term
      then sigma, acc
      else sigma, (act_term, rhs_term, make_constr_tree constr_index d) :: acc)
  (* NOTE: constructor applies with some additional constructors *)
  | Some new_data ->
    let* () = debug_args "collect_next_A" (lhs_term, act_term, rhs_term) in
    let* act_term : EConstr.t = Mebi_utils.econstr_normalize act_term in
    let* rhs_term : EConstr.t = Mebi_utils.econstr_normalize rhs_term in
    let* () = debug_args "collect_next_B" (lhs_term, act_term, rhs_term) in
    (* TODO: finish this *)
    return acc

(** [update_sigma d context]
    @param context
      is a tuple of lists [(subst, decls)] ...
      (* TODO: finish doc *) *)
and update_sigma (d : data)
  : EConstr.Vars.substl * EConstr.rel_declaration list -> data option mm
  = function
  | [], [] -> return (Some d)
  | _ :: substls, t_decl :: decls ->
    (* TODO: finish this *)
    return None
  | _substl, _decls -> (* TODO: err *) invalid_check_updated_ctx _substl _decls
;;
(* let* act_term : EConstr.t = Mebi_utils.econstr_normalize act_term in *)

(* let get_new_constrs *)
