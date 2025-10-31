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

let debugstr_unification_problem_list (ps : unification_problem list)
  : string mm
  =
  let iter_body (i : int) (acc : string list) =
    let* s = debugstr_unification_problem (List.nth ps i) in
    return (s :: acc)
  in
  let* sl : string list = iterate 0 (List.length ps - 1) [] iter_body in
  return (Strfy.list ~force_newline:true ~indent:1 Strfy.str sl)
;;

let debug_unification_problem_list
      ?(prefix = "")
      (ps : unification_problem list)
  : unit mm
  =
  let* s : string = debugstr_unification_problem_list ps in
  return (Log.debug (Printf.sprintf "%s unification_problem list: %s" prefix s))
;;

let debugstr_unification_problem_list_list
      ?(prefix = "")
      (ps : unification_problem list list)
  : string mm
  =
  let iter_body (i : int) (acc : string list) =
    let* s = debugstr_unification_problem_list (List.nth ps i) in
    return (s :: acc)
  in
  let* sl : string list = iterate 0 (List.length ps - 1) [] iter_body in
  return (Strfy.list ~force_newline:true Strfy.str sl)
;;

let debug_unification_problem_list_list
      ?(prefix = "")
      (ps : unification_problem list list)
  : unit mm
  =
  let* s : string = debugstr_unification_problem_list_list ps in
  return
    (Log.debug (Printf.sprintf "%s unification_problem list list: %s" prefix s))
;;

let debugstr_mebiconstrs env sigma (cs : Mebi_constr.t list) : string =
  let f = Strfy.econstr env sigma in
  Strfy.list
    ~force_newline:true
    ~indent:2
    (fun (action, dest, tree) ->
      Strfy.list
        ~force_newline:true
        ~indent:1
        Strfy.str
        [ Strfy.tuple ~is_keyval:true Strfy.str f ("action", action)
        ; Strfy.tuple ~is_keyval:true Strfy.str f ("dest", dest)
        ; Strfy.tuple
            ~is_keyval:true
            Strfy.str
            Mebi_constr.Tree.to_string
            ("tree", tree)
        ])
    cs
;;

let debug_mebiconstrs p (acc : Mebi_constr.t list) : unit mm =
  state (fun env sigma ->
    Log.debug
      (Printf.sprintf
         "%sacc:\n%s"
         (Utils.prefix p)
         (debugstr_mebiconstrs env sigma acc));
    sigma, ())
;;

let debug_buildconstrs_some act tgt unified_tgt acc : unit mm =
  state (fun env sigma ->
    let f = Strfy.econstr env sigma in
    Log.debug
      (Printf.sprintf
         "build_constrs:\n%s"
         (Strfy.list
            ~force_newline:true
            (Strfy.tuple ~is_keyval:true Strfy.str Strfy.str)
            [ "act", f act
            ; "tgt", f tgt
            ; "unified_tgt", f unified_tgt
            ; "acc", debugstr_mebiconstrs env sigma acc
            ]));
    sigma, ())
;;

let debug_buildconstrs_none act tgt acc : unit mm =
  state (fun env sigma ->
    let f = Strfy.econstr env sigma in
    Log.debug
      (Printf.sprintf
         "build_constrs:\n%s"
         (Strfy.list
            ~force_newline:true
            (Strfy.tuple ~is_keyval:true Strfy.str Strfy.str)
            [ "act", f act
            ; "tgt", f tgt
            ; "acc", debugstr_mebiconstrs env sigma acc
            ]));
    sigma, ())
;;

(*****************************************************************************)

type data =
  { ind_map : Mebi_ind.t F.t
  ; lts_index : int
  ; mutable to_unify : unification_problem list list
  }

let debugstr_data ({ lts_index; to_unify; _ } : data) : string mm =
  let* to_unify_str : string =
    (* if List.is_empty to_unify
       then return "[ ] (empty)"
       else *)
    debugstr_unification_problem_list_list to_unify
  in
  return
    (Printf.sprintf "- lts_index: %i\n- to_unify:\n%s\n" lts_index to_unify_str)
;;

let debug_data p (d : data) : unit mm =
  let* s = debugstr_data d in
  Log.debug (Printf.sprintf "%s\n%s" (Utils.prefix p) s);
  return ()
;;

(* let new_unif_pair (a : EConstr.t) (b : EConstr.t)
   : Mebi_setup.unif_problem mm
   =
   (* let* sigma = get_sigma in
   let* termR =
   if EConstr.isEvar sigma termR
   then
   let* termR_ty = Mebi_utils.type_of_econstr termR in
   let$ termR env sigma = Evarutil.new_evar env sigma termR_ty in
   return termR
   else return termR
   in *)
   return (a,b)
   ;; *)

(** [next_unification_problems constrs args] creates a new [unification_pair] for the [rhs_term] of each of the next applicable [constrs]. 
(* NOTE: these have already been checked prior to this point, we are just now preparing them for when we try to unify them again, altogether with any other applicable constructors from some constructor applied earlier in the tree. *)
(* TODO: finish doc *)
@see [map_ctor_unif_probs] *)
let build_next_unification_problems
      (constrs : Mebi_constr.t list)
      (args : EConstr.t array)
  : unification_problem list mm
  =
  let iter_body (i : int) (acc : unification_problem list) =
    let act_term, rhs_term, constr_tree = List.nth constrs i in
    let rhs_pair : unification_pair = rhs_term, args.(2) in
    (* NOTE: what if we also tried creating new pairs for the action here? *)
    return ((rhs_pair, constr_tree) :: acc)
  in
  iterate 0 (List.length constrs - 1) [] iter_body
;;

let debug_update_unification_problems next_problems d new_to_unify : unit mm =
  let* () =
    debug_unification_problem_list_list ~prefix:"to_unify acc" d.to_unify
  in
  let* () =
    debug_unification_problem_list ~prefix:"next_problems" next_problems
  in
  let* () =
    debug_unification_problem_list_list ~prefix:"cross product" d.to_unify
  in
  return ()
;;

(* NOTE: this is the original one, unsure if problematic *)
let cross_product1 acc next_problems : unification_problem list list =
  List.concat_map
    (fun d_to_unify ->
      List.map (fun next_problem -> next_problem :: d_to_unify) next_problems)
    acc
;;

(* NOTE: this was what "fixed" the old one, but perhaps causes more issues. for now using old one. *)
let cross_product2 acc next_problems : unification_problem list list =
  List.fold_left
    (fun (acc0 : unification_problem list list) x ->
      List.fold_left
        (fun (acc1 : unification_problem list list) y -> (y :: x) :: acc1)
        acc0
        next_problems)
    []
    acc
;;

let update_unification_problems
      (next_constrs : Mebi_constr.t list)
      (args : EConstr.t array)
      (d : data)
  : data mm
  =
  let* next_problems = build_next_unification_problems next_constrs args in
  let new_to_unify : unification_problem list list =
    (* cross_product1 d.to_unify next_problems *)
    cross_product2 d.to_unify next_problems
  in
  let* () = debug_update_unification_problems next_problems d new_to_unify in
  d.to_unify <- new_to_unify;
  return d
;;

(*****************************************************************************)

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
  (* sandbox (f (a, b)) *)
  f (a, b)
;;

(** [do_terms_unify_opt ?f a b] is the same as [do_terms_unify] except that [a] and [b] are [option].
    @return
      [false] if [Some a] and [Some b] do NOT unify else [true] (i.e., if either are [None] return [true].)
*)
let do_terms_unify_opt ?(f : unification_pair -> bool mm = unify) a b : bool mm =
  match a, b with
  | Some a, Some b -> do_terms_unify ~f a b
  | _, _ -> return true
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
  if lhs_term_unifies
  then
    Option.cata
      (fun a -> do_terms_unify ~f from_term lhs_term)
      (return true)
      action
  else return false
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
let sandbox_unify_all_opt
      ?(f : unification_pair -> bool mm = unify)
      (tgt : EConstr.t)
      (ps : unification_problem list)
  : (EConstr.t * Mebi_constr.Tree.t list) option mm
  =
  sandbox
    (let* unified_all_opt = unify_all_opt ~f ps in
     match unified_all_opt with
     | None -> return None
     | Some ctor_trees ->
       let* is_undefined = Mebi_utils.econstr_is_evar tgt in
       if is_undefined then return None else return (Some (tgt, ctor_trees)))
;;

(** [build_constrs (ctor_index,acc) act tgt (lts_index, ps)] is ...
    (* TODO: document *)
    @param (ctor_index,acc) is the constructor index (from 0).
    @param act is the action (label) that is leads to some [tgt] term.
    @param tgt
      is the term reached by applying constructor [ctor_index] to some term with [act], and will at first be an [evar] but then resolve to some concrete [econstr].
    @return a list containing the
    @see retrieve_tgt_nodes  *)
let rec build_constrs
          ?(f : unification_pair -> bool mm = unify)
          ((ctor_index, acc) : int * Mebi_constr.t list)
          (act : EConstr.t)
          (tgt : EConstr.t)
  : int * unification_problem list list -> Mebi_constr.t list mm
  = function
  | _, [] ->
    Log.debug "build_constrs (return)";
    return acc
  | lts_index, unif_probs :: unif_probs_list ->
    Log.debug "build_constrs";
    let* success = sandbox_unify_all_opt ~f tgt unif_probs in
    (match success with
     | None ->
       let* () = debug_buildconstrs_none act tgt acc in
       build_constrs ~f (ctor_index, acc) act tgt (lts_index, unif_probs_list)
     | Some (unified_tgt, ctor_trees) ->
       let* unified_tgt = econstr_normalize unified_tgt in
       let open Mebi_constr.Tree in
       let tree = Node ((Enc.of_int lts_index, ctor_index), ctor_trees) in
       let ctor = act, unified_tgt, tree in
       let acc = ctor :: acc in
       let* () = debug_buildconstrs_some act tgt unified_tgt acc in
       build_constrs ~f (ctor_index, acc) act tgt (lts_index, unif_probs_list))
;;

(*****************************************************************************)

let subst_of_decl ?(substl : EConstr.Vars.substl = []) x : EConstr.t =
  EConstr.Vars.substl substl (Context.Rel.Declaration.get_type x)
;;

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
  state (fun env sigma -> Evarutil.new_evar env sigma (subst_of_decl ~substl x))
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

let normalize_args
      ((lhs_term, act_term, rhs_term) : EConstr.t * EConstr.t * EConstr.t)
  : (EConstr.t * EConstr.t * EConstr.t) mm
  =
  let* lhs : EConstr.t = econstr_normalize lhs_term in
  let* act : EConstr.t = econstr_normalize act_term in
  let* tgt : EConstr.t = econstr_normalize rhs_term in
  return (lhs, act, tgt)
;;

(*****************************************************************************)

exception ConstructorNameNotRecognized of (EConstr.t * EConstr.t)

let fail_if_unrecognized_constructor : bool = true

let handle_unrecognized_ctor_fn env sigma x name args =
  if fail_if_unrecognized_constructor
  then (* TODO: err *) raise (ConstructorNameNotRecognized (x, name))
  else (
    Log.warning
      (Printf.sprintf
         "unrecognized constructor: %s"
         (Strfy.econstr env sigma name));
    sigma, Some (None, args))
;;

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
  : ((int * Rocq_utils.ind_constrs) option * EConstr.t array) option mm
  =
  state (fun env sigma ->
    match EConstr.kind sigma x with
    | App (name, args) ->
      (match F.find_opt d.ind_map name with
       | None -> handle_unrecognized_ctor_fn env sigma x name args
       | Some ind ->
         let lts_index = ind.index in
         (match ind.kind with
          | Mebi_ind.LTS l ->
            sigma, Some (Some (lts_index, l.constr_transitions), args)
          | _ -> (* TODO: err *) raise (InductiveKindNotLTS ind)))
    | _ -> sigma, None)
;;

let debug_ind_constr p (x : Rocq_utils.ind_constr) : unit mm =
  state (fun env sigma ->
    let constr_str : string = Strfy.ind_constr env sigma x in
    Log.debug (Printf.sprintf "%sind_constr:\n%s" (Utils.prefix p) constr_str);
    sigma, ())
;;

let debug_ind_constrs p (x : Rocq_utils.ind_constrs) : unit mm =
  state (fun env sigma ->
    let constrs_str : string = Strfy.ind_constrs env sigma x in
    Log.debug (Printf.sprintf "%sind_constrs:\n%s" (Utils.prefix p) constrs_str);
    sigma, ())
;;

let debug_ind_constrs_opt p (x : Rocq_utils.ind_constrs option) : unit mm =
  state (fun env sigma ->
    (match x with
     | None ->
       Log.debug (Printf.sprintf "%sind_constrs_opt is None" (Utils.prefix p))
     | Some constrs ->
       let constrs_str = Strfy.ind_constrs env sigma constrs in
       Log.debug
         (Printf.sprintf
            "%sind_constrs_opt is Some:\n%s"
            (Utils.prefix p)
            constrs_str));
    sigma, ())
;;

let debug_validconstrs_begin constrs : unit mm =
  debug_ind_constrs "Checking new" constrs
;;

let debug_validconstrs_iter i context : unit mm =
  debug_ind_constr (Printf.sprintf "Checking (%i)" i) context
;;

let debug_validconstrs_iter_unify i context unifies : unit mm =
  debug_ind_constr
    (Printf.sprintf "Checking (%i) unifies (%b)" i unifies)
    context
;;

let debug_args
      p
      ((lhs_term, act_term, rhs_term) : EConstr.t * EConstr.t * EConstr.t)
  : unit mm
  =
  state (fun env sigma ->
    let f : EConstr.t -> string = Strfy.econstr env sigma in
    let arg_str : string =
      Strfy.list
        ~force_newline:true
        (Strfy.tuple ~is_keyval:true Strfy.str f)
        [ "lhs", lhs_term; "act", act_term; "rhs", rhs_term ]
    in
    Log.debug (Printf.sprintf "%sargs:\n%s" (Utils.prefix p) arg_str);
    sigma, ())
;;

let debug_nextconstrs_none args : unit mm =
  debug_args
    "check_for_next_constructors, update sigma failed to fully apply \
     constructor"
    args
;;

let debug_nextconstrs_some_empty args : unit mm =
  debug_args
    "check_for_next_constructors, update sigma applied constructor without any \
     further constructors"
    args
;;

let debug_nextconstrs_some_next args : unit mm =
  debug_args
    "check_for_next_constructors, update sigma applied constructor with \
     additional constructors"
    args
;;

let debug_updatesigma_none t_subst : unit mm =
  state (fun env sigma ->
    let t_str : string = Strfy.econstr env sigma t_subst in
    Log.debug
      (Printf.sprintf "update_sigma, t_subst is not an application:\n%s" t_str);
    sigma, ())
;;

let debug_updatesigma_some_args args : unit mm =
  debug_args
    "update_sigma, found no additional constructors for"
    (args.(0), args.(1), args.(2))
;;

let debug_updatesigma_some_pair args : unit mm =
  debug_args
    "update_sigma, additional constructors found for"
    (args.(0), args.(1), args.(2))
;;

let debug_term p t : unit mm =
  state (fun env sigma ->
    Log.debug
      (Printf.sprintf "%sterm: %s" (Utils.prefix p) (Strfy.econstr env sigma t));
    sigma, ())
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
  Log.info "\n====================";
  Log.debug "A1";
  let* from_term : EConstr.t = econstr_normalize from_term in
  let* () = debug_validconstrs_begin constrs in
  let* () = debug_term "collect_valid_constructors" from_term in
  let* () = debug_data "collect_valid_constructors" d in
  let iter_body (i : int) (acc : Mebi_constr.t list) : Mebi_constr.t list mm =
    Log.info "\n-----------------------";
    Log.debug "A2";
    let* () = debug_term "collect_valid_constructors" from_term in
    let* () = debug_mebiconstrs "collect_valid_constructors iter" acc in
    let (ctx, term) : Constr.rel_context * Constr.t = constrs.(i) in
    let* () = debug_validconstrs_iter i (ctx, term) in
    let decls : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in
    let* substl : EConstr.Vars.substl = mk_ctx_substl [] decls in
    let* args = extract_args ~substl term in
    let* terms_unify =
      (* try_unify_constructor_args from_term action args *)
      try_unify_constructor_args ~f:debug_unify from_term action args
    in
    let* () = debug_validconstrs_iter_unify i (ctx, term) terms_unify in
    if terms_unify
    then check_for_next_constructors (i, acc) args d (substl, decls)
    else return acc
  in
  iterate 0 (Array.length constrs - 1) [] iter_body

(** [check_for_next_constructors acc args d context]
    (* TODO: finish doc *)
    (*NOTE: essnetially a [check_next_constructors] with the call to [check_update_ctx] handled inside (via [update_sigma]) *)
    @return
      [acc] updated with any additional constructors that must be applied from the current constructor [args].
*)
and check_for_next_constructors
      ((constr_index, acc) : int * Mebi_constr.t list)
      (args : EConstr.t * EConstr.t * EConstr.t)
      (d : data)
      (context : EConstr.Vars.substl * EConstr.rel_declaration list)
  : Mebi_constr.t list mm
  =
  Log.info "\n- - - - - - - - - - - - -";
  Log.debug "B1, check_for_next_constructors";
  let* () = debug_data "check_for_next_constructors" d in
  let* args = normalize_args args in
  let _lhs, act, tgt = args in
  let* new_data = update_sigma d context in
  match new_data with
  (* NOTE: this constructor cannot be fully applied (some failure later on) *)
  | None ->
    Log.debug "B1A";
    let* () = debug_nextconstrs_none args in
    return acc
  | Some d ->
    let* () = debug_data "check_for_next_constructors, update_sigma" d in
    (match d.to_unify with
     (* NOTE: constructor applies without any further constructors *)
     | [] ->
       Log.debug "B1B";
       let* () = debug_nextconstrs_some_empty args in
       state (fun env sigma ->
         if EConstr.isEvar sigma tgt
         then sigma, acc
         else sigma, (act, tgt, make_constr_tree constr_index d) :: acc)
     (* NOTE: constructor applies with some additional constructors *)
     | _ ->
       Log.debug "B1C";
       let* () = debug_nextconstrs_some_next args in
       let* () = (* NOTE: *) debug_args "collect_next_B" (_lhs, act, tgt) in
       build_constrs
         (* ~f:debug_unify *)
         (constr_index, acc)
         act
         tgt
         (d.lts_index, d.to_unify))

(** [update_sigma d context]
    (* TODO: finish doc *)
    @param context is a tuple of lists [(subst, decls)] ... *)
and update_sigma (d : data)
  : EConstr.Vars.substl * EConstr.rel_declaration list -> data option mm
  = function
  | [], [] ->
    Log.debug "C1, update_sigma return";
    let* () = debug_data "update_sigma" d in
    return (Some d)
  | _ :: substls, t_decl :: decls ->
    Log.debug "C2";
    let* () = debug_data "update_sigma" d in
    let t_subst = subst_of_decl ~substl:substls t_decl in
    let* constrs_opt = get_ind_constrs_opt t_subst d in
    (match constrs_opt with
     (* NOTE: skip, [t_subst] is not an application. *)
     | None ->
       Log.debug "C2A";
       (* TODO: should we fail here? @see [get_ind_constrs_opt] *)
       let* () = debug_updatesigma_none t_subst in
       update_sigma d (substls, decls)
     (* NOTE: continue, no additional constructors for [t_subst]. *)
     | Some (None, args) ->
       Log.debug "C2B";
       let* () = debug_updatesigma_some_args args in
       update_sigma d (substls, decls)
     (* NOTE: found applicable constructors [next_ind] for [t_subst]. *)
     | Some (Some (next_lts_index, next_ind_constrs), args) ->
       Log.debug "C2C";
       let* () = debug_updatesigma_some_pair args in
       collect_next_constructors
         (next_ind_constrs, args)
         { d with lts_index = next_lts_index; to_unify = [] })
  | _substl, _decls ->
    Log.debug "C3";
    (* TODO: err *) invalid_check_updated_ctx _substl _decls

and collect_next_constructors
      ((next_ind_constrs, args) : Rocq_utils.ind_constrs * EConstr.t array)
      (d : data)
  =
  Log.debug "D1";
  let* () = debug_data "collect_next_constructors" d in
  let$+ next_from_term _ sigma = Reductionops.nf_evar sigma args.(0) in
  let* next_constrs =
    collect_valid_constructors
      ~action:(Some args.(1))
      next_from_term
      next_ind_constrs
      d
  in
  match next_constrs with
  | [] ->
    Log.debug "D1A";
    return None
  | next_constrs ->
    Log.debug "D1B";
    let* new_d = update_unification_problems next_constrs args d in
    return (Some new_d)
;;
