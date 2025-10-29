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

(* type constr_data = { lts_index : int;ctor_index : int  } *)

type data =
  { ind_map : Mebi_ind.t F.t
  ; lts_index : int
  ; acc : unification_problem list list
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
         then raise (ConstructorNameNotRecognized (x, name))
         else sigma, None
       | Some ctor ->
         (match ctor.kind with
          | Mebi_ind.LTS l -> sigma, Some l.constr_transitions
          | _ -> raise (InductiveKindNotLTS ctor)))
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

(** [collect_valid_constructors from_term constrs d] ...
(* TODO: finish doc *)
@see [check_valid_constructor] *)
let
  (*rec*)
    collect_valid_constructors
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
    (* TODO: refactor from mk_ctx_substl, understand *)
    (* let ctx_tys : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in *)
    return []
  in
  iterate 0 (Array.length constrs - 1) [] iter_body
;;

(* let get_new_constrs *)
