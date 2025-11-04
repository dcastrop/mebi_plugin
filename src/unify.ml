open Logging
open Mebi_wrapper
open Mebi_wrapper.Syntax
open Mebi_utils

let default_debug : bool = false
let default_debug_unify_fail : bool = false

let debug_term p t : unit mm =
  state (fun env sigma ->
    Log.debug
      (Printf.sprintf "%sterm: %s" (Utils.prefix p) (Strfy.econstr env sigma t));
    sigma, ())
;;

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

type unification_problems = unification_problem list

let debugstr_unification_problems (ps : unification_problems) : string mm =
  let iter_body (i : int) (acc : string list) =
    let* s = debugstr_unification_problem (List.nth ps i) in
    return (s :: acc)
  in
  let* sl : string list = iterate 0 (List.length ps - 1) [] iter_body in
  return (Strfy.list ~force_newline:true ~indent:1 Strfy.str sl)
;;

let debug_unification_problems ?(prefix = "") (ps : unification_problems)
  : unit mm
  =
  let* s : string = debugstr_unification_problems ps in
  return (Log.debug (Printf.sprintf "%s unification_problems:\n%s" prefix s))
;;

let debugstr_unification_problems_list
      ?(prefix = "")
      (ps : unification_problems list)
  : string mm
  =
  let iter_body (i : int) (acc : string list) =
    let* s = debugstr_unification_problems (List.nth ps i) in
    return (s :: acc)
  in
  let* sl : string list = iterate 0 (List.length ps - 1) [] iter_body in
  return (Strfy.list ~force_newline:true Strfy.str sl)
;;

let debug_unification_problems_list
      ?(prefix = "")
      (ps : unification_problems list)
  : unit mm
  =
  let* s : string = debugstr_unification_problems_list ps in
  return
    (Log.debug (Printf.sprintf "%s unification_problems list:\n%s" prefix s))
;;

let debugstr_mebiconstrs env sigma (cs : Mebi_constr.t list) : string =
  let f = Strfy.econstr env sigma in
  Strfy.list
    ~force_newline:true
    ~label:"mebi_constr.t list"
    (fun (action, dest, tree) ->
      Strfy.list
        ~force_newline:true
        ~indent:1
        ~use:("{", "}")
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
         "build_constrs (some):\n%s\nacc:\n%s"
         (Strfy.list
            ~force_newline:true
            ~use:("{", "}")
            (Strfy.tuple ~is_keyval:true Strfy.str Strfy.str)
            [ "act", f act; "tgt", f tgt; "unified_tgt", f unified_tgt ])
         (debugstr_mebiconstrs env sigma acc));
    sigma, ())
;;

let debug_buildconstrs_none act tgt acc : unit mm =
  state (fun env sigma ->
    let f = Strfy.econstr env sigma in
    Log.debug
      (Printf.sprintf
         "build_constrs (none):\n%s"
         (Strfy.list
            ~force_newline:true
            ~use:("{", "}")
            (Strfy.tuple ~is_keyval:true Strfy.str Strfy.str)
            [ "act", f act
            ; "tgt", f tgt
            ; "acc", debugstr_mebiconstrs env sigma acc
            ]));
    sigma, ())
;;

(*****************************************************************************)

(** [debug_unify a b] an alternate wrapper for [unify] that shows additional debugging information. @see [unify] for details. *)
(* let debug_unify ((a, b) : unification_pair) : bool mm =
   let* r : bool = unify (a, b) in
   let* _ =
   sandbox
   (state (fun env sigma ->
   let f : EConstr.t -> string = Strfy.econstr env sigma in
   Log.debug (Printf.sprintf "unify : %b\n- a: %s\n- b: %s" r (f a) (f b));
   sigma, ()))
   in
   return r
   ;; *)

type lts_arg =
  { term : EConstr.t
  ; need_substl : bool
  }

let lts_arg ?(need_substl : bool = true) term : lts_arg = { term; need_substl }

type lts_args =
  { lhs : lts_arg
  ; act : lts_arg
  ; rhs : lts_arg
  }

let lts_args_of_array ?(need_substl : bool = true) args : lts_args =
  { lhs = lts_arg ~need_substl args.(0)
  ; act = lts_arg ~need_substl args.(1)
  ; rhs = lts_arg ~need_substl args.(2)
  }
;;

exception LtsArgTermNeedsSubstl of EConstr.t

let get_lts_arg_term { term; need_substl } : EConstr.t =
  if need_substl then raise (LtsArgTermNeedsSubstl term) else term
;;

let get_lhs { lhs; _ } : EConstr.t = get_lts_arg_term lhs
let get_act { act; _ } : EConstr.t = get_lts_arg_term act
let get_rhs { rhs; _ } : EConstr.t = get_lts_arg_term rhs

(** *)
let var_substl substl : EConstr.t -> EConstr.t = EConstr.Vars.substl substl

let arg_substl substl { term; _ } : lts_arg =
  { term = var_substl substl term; need_substl = false }
;;

let substl_lhs substl args : lts_args =
  { args with lhs = arg_substl substl args.lhs }
;;

let substl_act substl args : lts_args =
  { args with act = arg_substl substl args.act }
;;

let substl_rhs substl args : lts_args =
  { args with rhs = arg_substl substl args.rhs }
;;

let args_substl substl { lhs; act; rhs } : lts_args =
  { lhs = arg_substl substl lhs
  ; act = arg_substl substl act
  ; rhs = arg_substl substl rhs
  }
;;

let arg_normalize { term; need_substl } : lts_arg mm =
  let* () =
    let* env = get_env in
    let* sigma = get_sigma in
    if need_substl
    then (
      let term_str : string = Strfy.econstr env sigma term in
      let warn_str : string =
        Printf.sprintf "normalizing term that need_substl:\n%s" term_str
      in
      Log.warning warn_str);
    return ()
  in
  let* term : EConstr.t = econstr_normalize term in
  return { term; need_substl }
;;

let normalize_lhs args : lts_args mm =
  let* lhs = arg_normalize args.lhs in
  return { args with lhs }
;;

let normalize_act args : lts_args mm =
  let* act = arg_normalize args.act in
  return { args with act }
;;

let normalize_rhs args : lts_args mm =
  let* rhs = arg_normalize args.rhs in
  return { args with rhs }
;;

let args_normalize { lhs; act; rhs } : lts_args mm =
  let* lhs = arg_normalize lhs in
  let* act = arg_normalize act in
  let* rhs = arg_normalize rhs in
  return { lhs; act; rhs }
;;

(** [try_unify_constructor_args ?f from_term action args]
    (*TODO: finish doc *)
    (* NOTE: the unification is within a [sandbox] -- which is different from the original implementation. *)
*)
(* let try_unify_constructor_args
   ?(debug : bool = false)
   (from_term : EConstr.t)
   (action : EConstr.t option)
   (args : lts_args)
   : bool mm
   =
   (* NOTE: these unification checks are within a [sandbox]. *)
   (* TODO: this is a new change, check the impact of this *)
   let* lhs_term_unifies : bool = unify ~debug (from_term, get_lhs args) in
   if lhs_term_unifies
   then
   Option.cata
   (fun action -> unify ~debug (action, get_act args))
   (return true)
   action
   else return false
   ;; *)

(*****************************************************************************)

type data =
  { ind_map : Mebi_ind.t F.t
  ; lts_index : int (* ; mutable to_unify : unification_problems list *)
  }

(* let debugstr_data ({ lts_index; to_unify; _ } : data) : string mm = *)
let debugstr_data
      ({ lts_index; _ } : data)
      (to_unify : unification_problems list)
  : string mm
  =
  let* to_unify_str : string =
    (* if List.is_empty to_unify
       then return "[ ] (empty)"
       else *)
    debugstr_unification_problems_list to_unify
  in
  return
    (Printf.sprintf "- lts_index: %i\n- to_unify:\n%s\n" lts_index to_unify_str)
;;

let debug_data p (d : data) (to_unify : unification_problems list) : unit mm =
  let* s = debugstr_data d to_unify in
  Log.debug (Printf.sprintf "%s\n%s" (Utils.prefix p) s);
  return ()
;;

let debug_update_unification_problems next_problems to_unify new_to_unify
  : unit mm
  =
  let* () = debug_unification_problems_list ~prefix:"to_unify acc" to_unify in
  let* () = debug_unification_problems ~prefix:"next_problems" next_problems in
  let* () = debug_unification_problems_list ~prefix:"cross product" to_unify in
  return ()
;;

(* NOTE: this is the original one, unsure if problematic *)
let cross_product1 acc next_problems : unification_problems list =
  List.concat_map
    (fun d_to_unify ->
      List.map (fun next_problem -> next_problem :: d_to_unify) next_problems)
    acc
;;

(* NOTE: this was what "fixed" the old one, but perhaps causes more issues. for now using old one. *)
let cross_product2 acc next_problems : unification_problems list =
  List.fold_left
    (fun (acc0 : unification_problems list) x ->
      List.fold_left
        (fun (acc1 : unification_problems list) y -> (y :: x) :: acc1)
        acc0
        next_problems)
    []
    acc
;;

(** [next_unification_problems constrs args] creates a new [unification_pair] for the [rhs_term] of each of the next applicable [constrs]. 
(* NOTE: these have already been checked prior to this point, we are just now preparing them for when we try to unify them again, altogether with any other applicable constructors from some constructor applied earlier in the tree. *)
(* TODO: finish doc *)
@see [map_ctor_unif_probs] *)
let build_next_unification_problems
      (constrs : Mebi_constr.t list)
      (args : lts_args)
  : unification_problems mm
  =
  let iter_body (i : int) (acc : unification_problems) =
    let act_term, rhs_term, constr_tree = List.nth constrs i in
    let tgt : EConstr.t = get_rhs args in
    let rhs_pair : unification_pair = rhs_term, tgt in
    (* NOTE: what if we also tried creating new pairs for the action here? *)
    return ((rhs_pair, constr_tree) :: acc)
  in
  iterate 0 (List.length constrs - 1) [] iter_body
;;

let update_unification_problems
      (next_constrs : Mebi_constr.t list)
      (args : lts_args)
      (* (d : data) *)
        (to_unify : unification_problems list)
  : unification_problems list mm
  =
  let* next_problems = build_next_unification_problems next_constrs args in
  let new_to_unify : unification_problems list =
    (* cross_product1 to_unify next_problems *)
    cross_product2 to_unify next_problems
  in
  let* () =
    debug_update_unification_problems next_problems to_unify new_to_unify
  in
  (* d.to_unify <- new_to_unify; *)
  return new_to_unify
;;

(*****************************************************************************)

let debug_unify env sigma a b =
  let f = Strfy.econstr env sigma in
  Log.debug (Printf.sprintf "unify, unified:\n- a: %s\n- b: %s\n" (f a) (f b))
;;

let debug_unifyerr env sigma a b c d =
  let f = Strfy.econstr env sigma in
  let s1 = Printf.sprintf "unify, failed:\n- a: %s\n- b: %s" (f a) (f b) in
  let s2 = Printf.sprintf "specifically, \"%s\" with: %s" (f c) (f d) in
  Log.debug (Printf.sprintf "%s\n%s\n" s1 s2)
;;

(** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
let unify ?(debug : bool = default_debug) ((a, b) : unification_pair) : bool mm =
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    let open Pretype_errors in
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL a b in
      if debug then debug_unify env sigma a b;
      sigma, true
    with
    | PretypeError (_, _, CannotUnify (c, d, _e)) ->
      if default_debug_unify_fail then debug_unifyerr env sigma a b c d;
      sigma, false)
;;

(** [unify_all_opt ?f ps] calls [unify] on each unification problems in [ps], aborting at once if any fail to be unified.
    @param ?f
      is the function performing unification and is [unify] by default, but may be set to [debug_unify] for additional debug information.
    @param ps
      is the list of constructor trees and their corresponding unification problems.
    @return
      [None] if any unifications fail, or [Some list] containing the [Mebi_constr.Tree.t] of each of the corresponding successful unifications.
*)
let rec unify_all_opt ?(debug : bool = default_debug)
  : unification_problems -> Mebi_constr.Tree.t list option mm
  = function
  | [] -> return (Some [])
  | (unif_prob, ctor_tree) :: t ->
    let* success = unify ~debug unif_prob in
    if Bool.not success
    then return None
    else
      let* unified_all_opt = unify_all_opt ~debug t in
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
      ?(debug : bool = default_debug)
      (tgt : EConstr.t)
      (ps : unification_problems)
  : (EConstr.t * Mebi_constr.Tree.t list) option mm
  =
  sandbox
    (let* unified_all_opt = unify_all_opt ~debug ps in
     match unified_all_opt with
     | None -> return None
     | Some ctor_trees ->
       let* term = econstr_normalize tgt in
       let* is_undefined = Mebi_utils.econstr_is_evar term in
       if is_undefined then return None else return (Some (term, ctor_trees)))
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
          ?(debug : bool = default_debug)
          ((constr_index, acc) : int * Mebi_constr.t list)
          (act : EConstr.t)
          (tgt : EConstr.t)
  : int * unification_problems list -> Mebi_constr.t list mm
  = function
  | _, [] -> return acc
  | lts_index, unif_probs :: unif_probs_list ->
    let* () = debug_term "build_constrs tgt" tgt in
    let* success = sandbox_unify_all_opt ~debug tgt unif_probs in
    (match success with
     | None ->
       let* () = debug_buildconstrs_none act tgt acc in
       build_constrs
         ~debug
         (constr_index, acc)
         act
         tgt
         (lts_index, unif_probs_list)
     | Some (unified_tgt, ctor_trees) ->
       let* unified_tgt = econstr_normalize unified_tgt in
       let* act = econstr_normalize act in
       let open Mebi_constr.Tree in
       let tree = Node ((Enc.of_int lts_index, constr_index), ctor_trees) in
       let ctor = act, unified_tgt, tree in
       let acc = ctor :: acc in
       let* () = debug_buildconstrs_some act tgt unified_tgt acc in
       build_constrs
         ~debug
         (constr_index, acc)
         act
         tgt
         (lts_index, unif_probs_list))
;;

(*****************************************************************************)

let debugstr_substl env sigma (xs : EConstr.Vars.substl) : string =
  Strfy.list ~force_newline:true (Strfy.econstr env sigma) xs
;;

let debug_substl p (xs : EConstr.Vars.substl) : unit mm =
  state (fun env sigma ->
    Log.debug
      (Printf.sprintf
         "%ssubstl:\n%s"
         (Utils.prefix p)
         (debugstr_substl env sigma xs));
    sigma, ())
;;

let debugstr_decls env sigma (xs : Rocq_utils.econstr_decls) : string =
  Strfy.list ~force_newline:true (Strfy.econstr_rel_decl env sigma) xs
;;

let debug_decls p (xs : Rocq_utils.econstr_decls) : unit mm =
  state (fun env sigma ->
    Log.debug
      (Printf.sprintf
         "%sdecls:\n%s"
         (Utils.prefix p)
         (debugstr_decls env sigma xs));
    sigma, ())
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
  let* env = get_env in
  let* sigma = get_sigma in
  let$ vt _ _ = Evarutil.new_evar env sigma subst in
  return vt
;;

(* state (fun env sigma -> Evarutil.new_evar env sigma (subst_of_decl substl x)) *)

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

(** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
    @param ?substl
      is a list of substitutions applied to the terms prior to being returned.
    @param term
      must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
    @return a triple of [lhs_term, action, rhs_term]. *)
let extract_args
      (* (substl : EConstr.Vars.substl) *)
        (term : Constr.t)
  : lts_args mm
  =
  match Constr.kind term with
  | App (_fn, args) ->
    if Array.length args == 3
    then (
      let args = EConstr.of_constr_array args in
      (* let args = Array.map (EConstr.Vars.substl substl) args in *)
      (* return (args.(0), args.(1), args.(2))) *)
      return (lts_args_of_array args))
    else (* TODO: err *) invalid_lts_args_length (Array.length args)
  | _ -> (* TODO: err *) invalid_lts_term_kind term
;;

(*****************************************************************************)

exception ConstructorNameNotRecognized of (EConstr.t * EConstr.t)

let fail_if_unrecognized_constructor : bool = true

let handle_unrecognized_ctor_fn env sigma x name (args : EConstr.t array) =
  if fail_if_unrecognized_constructor
  then (* TODO: err *) raise (ConstructorNameNotRecognized (x, name))
  else (
    Log.warning
      (Printf.sprintf
         "unrecognized constructor \"%s\" has args:\n%s"
         (Strfy.econstr env sigma name)
         (Strfy.array ~force_newline:true (Strfy.econstr env sigma) args));
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

let debugstr_arg env sigma { term; need_substl } : string =
  Strfy.tuple
    Strfy.str
    Strfy.str
    ( Strfy.tuple
        ~is_keyval:true
        ~indent:1
        Strfy.str
        Strfy.bool
        ("need_substl", need_substl)
    , Strfy.tuple
        ~is_keyval:true
        ~indent:1
        Strfy.str
        (Strfy.econstr env sigma)
        ("term", term) )
;;

let debug_args p ({ lhs; act; rhs } : lts_args) : unit mm =
  state (fun env sigma ->
    let arg_str : string =
      Strfy.list
        ~force_newline:true
        (Strfy.tuple ~is_keyval:true Strfy.str Strfy.str)
        [ "lhs", debugstr_arg env sigma lhs
        ; "act", debugstr_arg env sigma act
        ; "rhs", debugstr_arg env sigma rhs
        ]
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
  debug_args "update_sigma, found no additional constructors for" args
;;

let debug_updatesigma_some_pair args : unit mm =
  debug_args "update_sigma, additional constructors found for" args
;;

let make_constr_tree (constr_index : int) (lts_index : int) =
  Mebi_constr.Tree.Node ((Enc.of_int lts_index, constr_index), [])
;;

type substl_maker = unit -> EConstr.Vars.substl mm

let make_substl_fun (decls : Rocq_utils.econstr_decls) =
  fun () -> mk_ctx_substl [] (List.rev decls)
;;

(** [collect_valid_constructors from_term constrs d] ...
(* TODO: finish doc *)
@see [check_valid_constructor] *)
let rec collect_valid_constructors
          ?(action : EConstr.t option = None)
          (from_term : EConstr.t)
          (constrs : Rocq_utils.ind_constrs)
          (d : data) (* (to_unify : unification_problems list) *)
  : Mebi_constr.t list mm
  =
  Log.info "\n====================";
  Log.debug "A1";
  let* from_term : EConstr.t = econstr_normalize from_term in
  (* let* () = debug_validconstrs_begin constrs in *)
  (* let* () = debug_term "collect_valid_constructors" from_term in *)
  (* let* () = debug_data "collect_valid_constructors" d to_unify in *)
  let iter_body (i : int) (acc : Mebi_constr.t list) : Mebi_constr.t list mm =
    Log.info "\n-----------------------";
    Log.debug (Printf.sprintf "collect_valid_constructors: #%i" i);
    let* () = debug_term "collect_valid_constructors" from_term in
    let* () = debug_mebiconstrs "collect_valid_constructors iter" acc in
    let (ctx, term) : Constr.rel_context * Constr.t = constrs.(i) in
    (* let* () = debug_validconstrs_iter i (ctx, term) in *)
    (* NOTE: [decls] contains the types of each *)
    let decls : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in
    (* NOTE: [substl] contains the terms and evars *)
    let* substl : EConstr.Vars.substl = mk_ctx_substl [] (List.rev decls) in
    let* raw_args = extract_args term in
    let args : lts_args = substl_lhs substl raw_args in
    let* args : lts_args = normalize_lhs args in
    (* NOTE: check if [from_term] unifies with [lhs] *)
    let* lhs_term_unifies : bool = unify (from_term, get_lhs args) in
    if Bool.not lhs_term_unifies
    then return acc
    else
      (* NOTE: after this point we can have [args = (lhs,?act,?rhs)] *)
      (* NOTE: the first constructor to unify will resolve [?act] and [?rhs] *)
      (* NOTE: any other constructors with a greater index will not unify *)
      (* NOTE: check_for_next_constructors *)
      (* NOTE: -> update_sigma *)
      (* NOTE: -> collect_valid_constructors *)
      check_for_next_constructors (i, acc) args d [ [] ] (substl, decls)
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
      (args : lts_args)
      (d : data)
      (to_unify : unification_problems list)
      (context : EConstr.Vars.substl * EConstr.rel_declaration list)
  : Mebi_constr.t list mm
  =
  Log.info "\n- - - - - - - - - - - - -";
  Log.debug "B1, check_for_next_constructors";
  let* () = debug_data "check_for_next_constructors" d to_unify in
  (* NOTE: try to move normalization to [update_sigma] *)
  (* let* args = normalize_args args in *)
  (* let lhs, act, tgt = args in *)
  let* to_unify_opt = update_sigma d to_unify context in
  match to_unify_opt with
  (* NOTE: this constructor cannot be fully applied (some failure later on) *)
  | None ->
    Log.debug "B1A";
    let* () = debug_nextconstrs_none args in
    return acc
  (* NOTE: constructor applies without any further constructors *)
  | Some [ [] ] ->
    Log.debug "B1B";
    (* let* substl : EConstr.Vars.substl = f_substl () in *)
    (* let args : lts_args = substl_lhs substl raw_args in *)
    let substl, _decls = context in
    let args = substl_act substl args in
    let args = substl_rhs substl args in
    let act = get_act args in
    let tgt = get_rhs args in
    let* () = debug_nextconstrs_some_empty args in
    state (fun env sigma ->
      if EConstr.isEvar sigma tgt
      then (
        Log.warning
          (Printf.sprintf "tgt is evar: %s" (Strfy.econstr env sigma tgt));
        sigma, acc)
      else if EConstr.isEvar sigma act
      then (
        Log.warning
          (Printf.sprintf "act is evar: %s" (Strfy.econstr env sigma act));
        sigma, acc)
      else
        let open Mebi_constr.Tree in
        let tree = Node ((Enc.of_int d.lts_index, constr_index), []) in
        let constrs = (act, tgt, tree) :: acc in
        sigma, constrs)
  (* NOTE: constructor applies with some additional constructors *)
  | Some to_unify ->
    Log.debug "B1C";
    (* let* substl : EConstr.Vars.substl = f_substl () in *)
    (* let args : lts_args = substl_lhs substl raw_args in *)
    let substl, _decls = context in
    let args = substl_act substl args in
    let act = get_act args in
    let args = substl_rhs substl args in
    let tgt = get_rhs args in
    let* () = debug_nextconstrs_some_next args in
    (* let* () = (* NOTE: *) debug_args "collect_next_B" args in *)
    (* let act = args.act in *)
    (* let tgt = args.rhs in *)
    let* () =
      debug_unification_problems_list ~prefix:"pre-build_constrs" to_unify
    in
    build_constrs ~debug:true (constr_index, acc) act tgt (d.lts_index, to_unify)

(** [update_sigma d context]
    (* TODO: finish doc *)
    @param context is a tuple of lists [(subst, decls)] ... *)
and update_sigma (d : data) (to_unify : unification_problems list)
  :  EConstr.Vars.substl * EConstr.rel_declaration list
  -> unification_problems list option mm
  = function
  | [], [] ->
    Log.debug "C1, update_sigma return";
    let* () = debug_data "update_sigma" d to_unify in
    return (Some to_unify)
  | _ :: substls, t_decl :: decls ->
    Log.debug "C2";
    let* () = debug_data "update_sigma" d to_unify in
    let* t_subst : EConstr.t = subst_of_decl substls t_decl in
    let* constrs_opt = get_ind_constrs_opt t_subst d in
    (match constrs_opt with
     (* NOTE: skip, [t_subst] is not an application. *)
     | None ->
       Log.debug "C2A";
       let* () = debug_updatesigma_none t_subst in
       update_sigma d to_unify (substls, decls)
     (* NOTE: continue, no additional constructors for [t_subst]. *)
     | Some (None, args) ->
       Log.debug "C2B";
       update_sigma d to_unify (substls, decls)
     (* NOTE: found applicable constructors [next_ind] for [t_subst]. *)
     | Some (Some (next_lts_index, next_ind_constrs), raw_args) ->
       Log.debug "C2C";
       let args = lts_args_of_array raw_args in
       let* () = debug_updatesigma_some_pair args in
       collect_next_constructors
         (next_ind_constrs, raw_args)
         { d with lts_index = next_lts_index }
         to_unify
         substls)
  | _substl, _decls ->
    Log.debug "C3";
    (* TODO: err *) invalid_check_updated_ctx _substl _decls

and collect_next_constructors
      ((next_ind_constrs, raw_args) : Rocq_utils.ind_constrs * EConstr.t array)
      (d : data)
      (to_unify : unification_problems list)
      (substl : EConstr.Vars.substl)
  : unification_problems list option mm
  =
  Log.info "\n~~~~~~~~~~~~~~~~~~~~~~~~~";
  Log.debug "D1";
  let* () = debug_data "collect_next_constructors" d to_unify in
  let$+ next_from_term _ sigma = Reductionops.nf_evar sigma raw_args.(0) in
  let* () = debug_term "next_from_term" next_from_term in
  let args : lts_args =
    { lhs = lts_arg next_from_term
    ; act = lts_arg raw_args.(1)
    ; rhs = lts_arg raw_args.(2)
    }
  in
  (* let args : lts_args = substl_lhs substl args in *)
  let args : lts_args = substl_act substl args in
  let act : EConstr.t = get_act args in
  Log.debug "D1A (next_constrs = collect_valid_constructors ...)";
  let* next_constrs =
    collect_valid_constructors
      ~action:(Some act)
      next_from_term
      next_ind_constrs
      d
  in
  Log.info "\n~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~";
  Log.debug "D2";
  match next_constrs with
  | [] ->
    Log.debug "D2A";
    return None
  | next_constrs ->
    Log.debug "D2B";
    let args : lts_args = substl_rhs substl args in
    let* () = debug_args "D2B" args in
    Log.info "\n~ ~ ~ ~ ~ ~";
    let* () =
      debug_unification_problems_list ~prefix:"pre-update_unifprobs" to_unify
    in
    let* to_unify = update_unification_problems next_constrs args to_unify in
    let* () =
      debug_unification_problems_list ~prefix:"updated_unifprobs" to_unify
    in
    return (Some to_unify)
;;
