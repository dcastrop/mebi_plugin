open Logging
open Mebi_wrapper
open Mebi_wrapper.Syntax
(* open Mebi_unification *)

exception ExitDevelopmentTest of unit

let dev_stop () : unit = raise (ExitDevelopmentTest ())
let dev_counter : int ref = ref 0
let dev_stop_at : int = 3

let dev_checkin () : unit =
  dev_counter := !dev_counter + 1;
  if !dev_counter == dev_stop_at then dev_stop () else ()
;;

let debug_term p (x : EConstr.t) : unit mm =
  state (fun env sigma ->
    let sx : string = Rocq_utils.Strfy.econstr env sigma x in
    Log.debug (Printf.sprintf "%s%s" (Utils.prefix p) sx);
    sigma, ())
;;

type data =
  { ind_map : Mebi_ind.t F.t
  ; lts_enc : Enc.t
  }

let debug_collect_valid_constructors
      (d : data)
      (from_term : EConstr.t)
      (constructors : Rocq_utils.ind_constrs)
  : unit mm
  =
  Log.info "\n====================";
  state (fun env sigma ->
    let from_str : string = Rocq_utils.Strfy.econstr env sigma from_term in
    let constructors_str : string = Strfy.ind_constrs env sigma constructors in
    Log.debug
      (Printf.sprintf
         "collect_valid_constructors, lts enc (%s)\n\
          - from term: %s\n\
          - constructors:\n\
          %s"
         (Enc.to_string d.lts_enc)
         from_str
         constructors_str);
    sigma, ())
;;

(******************************************************************************)

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

exception ConstructorNameNotRecognized of (EConstr.t * EConstr.t)
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
let get_ind_constrs_opt (x : EConstr.t) (fmap : Mebi_ind.t F.t)
  : (Enc.t * Rocq_utils.ind_constrs * EConstr.t array) option mm
  =
  state (fun env sigma ->
    match EConstr.kind sigma x with
    | App (name, args) ->
      (match F.find_opt fmap name with
       | None ->
         let f = Rocq_utils.Strfy.econstr env sigma in
         Log.warning (Printf.sprintf "%s has unknown name: %s" (f x) (f name));
         (* TODO: err *) raise (ConstructorNameNotRecognized (x, name))
       | Some ind ->
         let lts_enc = ind.enc in
         (match ind.kind with
          | Mebi_ind.LTS l -> sigma, Some (lts_enc, l.constr_transitions, args)
          | _ -> (* TODO: err *) raise (InductiveKindNotLTS ind)))
    | _ -> sigma, None)
;;

(******************************************************************************)

(** create "blank/empty" templates for the constructors, which we will try to unify with (in a sandbox). if the unification succeeds, then we begin to search recursively outwards, and each iteration we "split" the existential variables provided so that each constructor does not interfere with the unification of another (i.e., just replace them with fresh templates, carrying over any hard-set information). this ensures that on the return of the recursion, we have a list corresponding to each leaf of the tree we have explored, where existential variable that remains is unique to that element in the list.
*)

type constructor_args =
  { constructor : Rocq_utils.ind_constr
  ; decls : Rocq_utils.econstr_decls
  ; substl : EConstr.Vars.substl
  ; tree : Mebi_constr.Tree.t
  ; lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

let is_evar sigma : EConstr.t -> bool = EConstr.isEvar sigma
let lhs_is_evar sigma { lhs; _ } : bool = is_evar sigma lhs
let act_is_evar sigma { act; _ } : bool = is_evar sigma act
let rhs_is_evar sigma { rhs; _ } : bool = is_evar sigma rhs

let is_constructor_args_axiom : constructor_args -> bool mm = function
  | { lhs; act; rhs; _ } ->
    state (fun env sigma ->
      let lhs : bool = is_evar sigma lhs in
      let act : bool = is_evar sigma act in
      let rhs : bool = is_evar sigma rhs in
      sigma, lhs && act && rhs)
;;

(** represents a sequence of constructor applications *)
type constructor_datatree = constructor_args Mebi_constr.Tree.tree

let mk_constructor_datatree (x : constructor_args) : constructor_datatree =
  Mebi_constr.Tree.Node (x, [])
;;

(** maps a list of [constructor_args] to a list of [constructor_datatree], where each element becomes the root-node of it's own tree.
*)
let mk_constructor_datatree_list (xs : constructor_args list)
  : constructor_datatree list
  =
  List.map mk_constructor_datatree xs
;;

let constructor_args_to_string ?(indent : int = 0) env sigma
  : constructor_args -> string
  = function
  | { constructor; decls; substl; tree; lhs; act; rhs } ->
    let indent' = indent + 1 in
    let f =
      Utils.Strfy.tuple ~is_keyval:true ~indent Utils.Strfy.str Utils.Strfy.str
    in
    let g = Rocq_utils.Strfy.econstr env sigma in
    let h = Rocq_utils.Strfy.econstr_rel_decl env sigma in
    let i =
      Utils.Strfy.list ~force_newline:true ~indent:indent' ~label:"decls" h
    in
    let j =
      Utils.Strfy.list ~force_newline:true ~indent:indent' ~label:"substl" g
    in
    let k =
      Utils.Strfy.tuple
        ~is_keyval:true
        ~indent:indent'
        Utils.Strfy.str
        (Printf.sprintf "\n%s%s" (Utils.Strfy.str_tabs indent'))
    in
    let l = Strfy.ind_constr ~indent env sigma in
    Utils.Strfy.list
      ~force_newline:true
      ~label:"constructor_args"
      ~indent
      ~use:("{", "}")
      Utils.Strfy.str
      [ k ("constructor", l constructor)
      ; k ("decls", i decls)
      ; k ("substl", j substl)
      ; f ("lhs", g lhs)
      ; f ("act", g act)
      ; f ("rhs", g rhs)
      ; f ("tree", Rocq_utils.Strfy.constr_tree tree)
      ]
;;

(* let debug_datatree p (x : constructor_datatree) : unit mm =
   state (fun env sigma ->
   let sx : string = Rocq_utils.Strfy.econstr env sigma x in
   Log.debug (Printf.sprintf "%s%s" (Utils.prefix p) sx);
   sigma, ())
   ;; *)

let debug_constructor_args ?(s : string = "constructor_args") p x : unit mm =
  state (fun env sigma ->
    let sx : string = constructor_args_to_string env sigma x in
    Log.debug (Printf.sprintf "%s%s:\n%s\n" (Utils.prefix p) s sx);
    sigma, ())
;;

let constructor_args_list_to_string env sigma : constructor_args list -> string =
  Utils.Strfy.list
    ~force_newline:true
    (constructor_args_to_string ~indent:1 env sigma)
;;

let debug_constructor_args_list ?(s : string = "constructor_args list") p x
  : unit mm
  =
  state (fun env sigma ->
    let sx : string = constructor_args_list_to_string env sigma x in
    Log.debug (Printf.sprintf "%s%s:\n%s\n" (Utils.prefix p) s sx);
    sigma, ())
;;

let debug_expand_constructor_args_list d acc tl : unit mm =
  let s : string = Enc.to_string d.lts_enc in
  let p : string =
    Printf.sprintf
      "expand (lts enc: %s) / (list |%i|) / (acc |%i|)"
      s
      (List.length tl)
      (List.length acc)
  in
  let* t : string =
    state (fun env sigma -> sigma, constructor_args_list_to_string env sigma tl)
  in
  let m : string =
    Printf.sprintf "%sconstructor_args list:\n%s\n\n" (Utils.prefix p) t
  in
  debug_constructor_args_list ~s:"expand acc" m acc
;;

let debug_update_constructor_args d arg acc : unit mm =
  let s : string = Enc.to_string d.lts_enc in
  let p : string = Printf.sprintf "update (%s) #%i" s (List.length acc) in
  debug_constructor_args p arg
;;

let debug_update_constructor_args_acc d arg acc : unit mm =
  let s : string = Enc.to_string d.lts_enc in
  let p : string = Printf.sprintf "update (%s)" s in
  let* a : string =
    state (fun env sigma -> sigma, constructor_args_to_string env sigma arg)
  in
  let m : string = Printf.sprintf "%sto update:\n%s\n\n" (Utils.prefix p) a in
  debug_constructor_args_list ~s:"update acc" m acc
;;

let debug_split_constructor_args d arg acc : unit mm =
  let s : string = Enc.to_string d.lts_enc in
  let p : string = Printf.sprintf "split (%s) #%i" s (List.length acc) in
  debug_constructor_args p arg
;;

let debug_split_constructor_args_acc d arg acc : unit mm =
  let s : string = Enc.to_string d.lts_enc in
  let p : string = Printf.sprintf "split (%s)" s in
  let* a : string =
    state (fun env sigma -> sigma, constructor_args_to_string env sigma arg)
  in
  let m : string =
    Printf.sprintf "%sconstructor_args:\n%s\n\n" (Utils.prefix p) a
  in
  debug_constructor_args_list ~s:"split acc" m acc
;;

(******************************************************************************)

let unify x y = Unify.unify ~debug:true (x, y)
let unify_opt x y = Option.cata (fun x -> unify x y) (return true) x

let does_constructor_apply
      (lhs : EConstr.t)
      (act : EConstr.t option)
      (constructor : constructor_args)
  : bool mm
  =
  let* lhs_unifies : bool = unify lhs constructor.lhs in
  let* act_unifies : bool = unify_opt act constructor.act in
  return (lhs_unifies && act_unifies)
;;

let mk_constructor_args
      (lts_enc : Enc.t)
      (constructor_index : int)
      (constructor : Rocq_utils.ind_constr)
  : constructor_args mm
  =
  let (ctx, term) : Constr.rel_context * Constr.t = constructor in
  let decls : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in
  let* substl : EConstr.Vars.substl = mk_ctx_substl [] (List.rev decls) in
  let* lhs, act, rhs = extract_args ~substl term in
  let open Mebi_constr.Tree in
  let tree : (Enc.t * int) tree = Node ((lts_enc, constructor_index), []) in
  return { constructor; decls; substl; tree; lhs; act; rhs }
;;

type split_evar =
  { old : EConstr.t
  ; fresh : EConstr.t
  }

let split_evar old fresh : split_evar = { old; fresh }

(* let append_constructor_args
      (original : constructor_args)
      (fresh : constructor_args)
  : constructor_args mm
  =
  let open Mebi_constr.Tree in
  let lhs : EConstr.t = fresh.lhs in
  let constructor : Rocq_utils.ind_constr = fresh.constructor in
  let decls : Rocq_utils.econstr_decls = fresh.decls in
  let tree : Mebi_constr.Tree.t = add fresh.tree original.tree in
  return { lhs; act; rhs; tree; decls; substl; constructor }
;; *)

(** this is where we handle checking if they sandbox-unify, and if so then we merge, otherwise we return None.
    (* TODO: don't what to prematurely optimize, but maybe we could also do the evar-replacing here? idk *)
*)
let mk_fresh_constructor_args
      (original : constructor_args)
      (d : data)
      (constructor_index : int)
      (constructor : Rocq_utils.ind_constr)
  : constructor_args mm
  =
  let* fresh : constructor_args =
    mk_constructor_args d.lts_enc constructor_index constructor
  in
  (* let* lhs_unifies = sandbox (unify original.lhs fresh.lhs) in
     let* act_unifies = sandbox (unify original.act fresh.act) in
     let* rhs_unifies = sandbox (unify original.rhs fresh.rhs) in
     (* if Bool.not (lhs_unifies && act_unifies && rhs_unifies) *)
     (* then return None *)
     (* else *)
     let* appended : constructor_args = append_constructor_args original fresh in
     return appended *)
  return fresh
;;

let debugstr_constructor_args_lts : constructor_args -> string mm = function
  | { lhs; act; rhs; _ } ->
    state (fun env sigma ->
      let f = Rocq_utils.Strfy.econstr env sigma in
      let s =
        Printf.sprintf "- lhs: %s\n- act: %s\n- rhs: %s" (f lhs) (f act) (f rhs)
      in
      sigma, s)
;;

let handle_acc (d : data) (acc : constructor_args list) (h : constructor_args)
  : constructor_args list -> constructor_args list mm
  = function
  | [] ->
    (* let acc' = h :: acc in *)
    let* hlts : string = debugstr_constructor_args_lts h in
    Log.debug
      (Printf.sprintf "refreshed yielded empty for (dropping):\n%s" hlts);
    return acc
  | refreshed ->
    let acc' = List.append acc refreshed in
    let* hlts : string = debugstr_constructor_args_lts h in
    let* rstr : string =
      state (fun env sigma ->
        sigma, constructor_args_list_to_string env sigma refreshed)
    in
    Log.debug (Printf.sprintf "for:\n%s\nrefreshed yielded some:\n%s" hlts rstr);
    return acc'
;;

(** [expand_constructor_args_list d acc constructor_args_list] recursively calls [update_constructor_args d] for each [constructor_args] in [constructor_args_list], effectively replacing the original with a list of new [constructor_args]. For any returned, any [evars] have been refreshed and replaced to ensure that each valid trace of constructors that can be applied after this point do not interefere with each other.
*)
let rec expand_constructor_args_list (d : data) (acc : constructor_args list)
  : constructor_args list -> constructor_args list mm
  =
  Log.info "\n====================";
  function
  | [] ->
    let* () = debug_expand_constructor_args_list d acc [] in
    return acc
  | h :: tl ->
    let* () = debug_expand_constructor_args_list d acc (h :: tl) in
    let* refreshed : constructor_args list = update_constructor_args d h in
    let* acc : constructor_args list = handle_acc d acc h refreshed in
    let () = (* ! *) dev_checkin () in
    expand_constructor_args_list d acc tl

and update_constructor_args (d : data)
  : constructor_args -> constructor_args list mm
  =
  Log.info "\n--------------------";
  function
  | the_constructor_args ->
    let { decls; substl; _ } = the_constructor_args in
    (* TODO: see [update_sigma] and [get_ind_constrs_opt] *)
    (* NOTE: we want to turn a single [constructor_args] into a list, where any [evars] in lhs,act,rhs are replaced with new ones, and when we copy over [substl] and [decls] to the new [constructor_args] we need to make sure to replace the old-evars with the corresponding new ones too. *)
    let iter_body (i : int) (acc : constructor_args list)
      : constructor_args list mm
      =
      let decl : Rocq_utils.econstr_decl = List.nth decls i in
      let substl : EConstr.Vars.substl = List.drop i substl in
      let* premise : EConstr.t = subst_of_decl substl decl in
      let* () = debug_term "update, premise" premise in
      let* constrs_opt = get_ind_constrs_opt premise d.ind_map in
      match constrs_opt with
      (* NOTE: no other constructors to apply for [premise] *)
      | None ->
        Log.debug "no additional constructors apply for premise";
        (* TODO: check if this has any evars? if yes then we need to create a new one ??? *)
        return acc
      (* NOTE: found applicable constructor for [premise] *)
      | Some (next_lts_enc, next_constructors, next_args) ->
        let* () =
          debug_update_constructor_args_acc d the_constructor_args acc
        in
        let* split_evars, next_constructor_args_list =
          split_constructor_args
            { d with lts_enc = next_lts_enc }
            next_constructors
            next_args
            the_constructor_args
        in
        Log.debug "returned from split";
        return (List.append acc next_constructor_args_list)
    in
    iterate 0 (List.length decls - 1) [] iter_body

and split_constructor_args
      (d : data)
      (next_constructors : Rocq_utils.ind_constrs)
      (next_args : EConstr.t array)
  : constructor_args -> (split_evar list * constructor_args list) mm
  =
  Log.info "\n- - - - - - - - - -";
  function
  | the_constructor_args ->
    (* let* () = debug_split_constructor_args d the_constructor_args [] in *)
    let { constructor; decls; substl; tree; lhs; act; rhs } =
      the_constructor_args
    in
    let iter_body
          (i : int)
          ((split_evars, acc) : split_evar list * constructor_args list)
      : (split_evar list * constructor_args list) mm
      =
      let next_constructor = next_constructors.(i) in
      let* fresh =
        mk_fresh_constructor_args the_constructor_args d i next_constructor
      in
      let* is_axiom : bool = is_constructor_args_axiom fresh in
      if is_axiom
      then (
        (* NOTE: no other constructors to apply, add to acc and stop *)
        Log.debug "is axiom";
        return (split_evars, fresh :: acc))
      else
        (* NOTE: some unresolved metavariables, indicating more to explore *)
        (* let* () = debug_constructor_args "fresh" fresh in *)
        (* TODO: see [update_sigma] and [collect_next_constructors] *)
        let$+ next_from_term _ sigma =
          Reductionops.nf_evar sigma next_args.(0)
        in
        (* *)
        (* *)
        (* *)
        (* TODO: here *)
        (* *)
        (* *)
        (* *)
        (* let* valid_constructors =
           filter_valid_constructors next_from_term action_term [] constructors
           in *)
        let* acc = expand_constructor_args_list d acc [ fresh ] in
        let* () = debug_constructor_args_list "split" acc in
        (* let () = dev_stop () in *)
        return (split_evars, acc)
    in
    iterate 0 (List.length decls - 1) ([], []) iter_body
;;

(******************************************************************************)

let mk_init_constructor_args_list
      (lts_enc : Enc.t)
      (raw_constructors : Rocq_utils.ind_constrs)
  : constructor_args list mm
  =
  let iter_body (i : int) (acc : constructor_args list)
    : constructor_args list mm
    =
    let constructor : Rocq_utils.ind_constr = raw_constructors.(i) in
    let* constructor_args : constructor_args =
      mk_constructor_args lts_enc i constructor
    in
    return (constructor_args :: acc)
  in
  iterate 0 (Array.length raw_constructors - 1) [] iter_body
;;

let rec filter_valid_constructors
          (lhs : EConstr.t)
          (act : EConstr.t option)
          (acc : constructor_args list)
  : constructor_args list -> constructor_args list mm
  = function
  | [] -> return acc
  | h :: t ->
    let* acc = filter_valid_constructors lhs act acc t in
    let* constructor_applies : bool = does_constructor_apply lhs act h in
    if constructor_applies then return (h :: acc) else return acc
;;

exception DataTreeLeavesExpectedEmpty of unit

let rec expand_constructor_datatrees
          (d : data)
          (acc : constructor_datatree list)
  : constructor_datatree list -> constructor_datatree list mm
  = function
  | [] ->
    Log.info "\n.....................";
    return acc
  | h :: tl ->
    Log.info "\n=====================";
    let* h' = expand_constructor_datatree d h in
    expand_constructor_datatrees d (h' :: acc) tl

and expand_constructor_datatree (d : data)
  : constructor_datatree -> constructor_datatree mm
  =
  let open Mebi_constr.Tree in
  function
  | Node (the_constructor_args, []) ->
    Log.info "\n---------------------";
    sandbox
      (let decls = the_constructor_args.decls
       and substl = the_constructor_args.substl in
       let* treelist = sandbox_constructor_datatree d [] (substl, decls) in
       return (Node (the_constructor_args, treelist)))
  | Node (the_constructor_args, the_next_constructor_args) ->
    raise (DataTreeLeavesExpectedEmpty ())

and sandbox_constructor_datatree (d : data) (acc : constructor_datatree list)
  :  EConstr.Vars.substl * Rocq_utils.econstr_decls
  -> constructor_datatree list mm
  = function
  | [], [] ->
    Log.info "\n. . . . . . . . . . .";
    return acc
  | _ :: substls, decl :: decls ->
    Log.info "\n- - - - - - - - - - -";
    let* acc =
      sandbox
        (let* subst : EConstr.t = subst_of_decl substls decl in
         let* sigma = get_sigma in
         match EConstr.kind sigma subst with
         | App (name, args) ->
           (match F.find_opt d.ind_map name with
            | None ->
              (* TODO: err *) raise (ConstructorNameNotRecognized (subst, name))
            | Some ind ->
              (match ind.kind with
               | Mebi_ind.LTS l ->
                 let$+ next_from_term _ sigma =
                   Reductionops.nf_evar sigma args.(0)
                 in
                 let* constructors =
                   mk_init_constructor_args_list ind.enc l.constr_transitions
                 in
                 let* valid_constructors =
                   filter_valid_constructors
                     next_from_term
                     (Some args.(1))
                     []
                     constructors
                 in
                 let datatree_constructors : constructor_datatree list =
                   mk_constructor_datatree_list valid_constructors
                 in
                 expand_constructor_datatrees d acc datatree_constructors
               | _ -> (* TODO: err *) raise (InductiveKindNotLTS ind)))
         | _ -> return acc)
    in
    sandbox_constructor_datatree d acc (substls, decls)
  | _substl, _decls -> (* TODO: err *) invalid_check_updated_ctx _substl _decls
;;

let explore_valid_constructors
      (d : data)
      (from_term : EConstr.t)
      (action_term : EConstr.t option)
      (constructors : constructor_args list)
  : Mebi_constr.t list mm
  =
  let* valid_constructors =
    filter_valid_constructors from_term action_term [] constructors
  in
  let datatree_constructors : constructor_datatree list =
    mk_constructor_datatree_list valid_constructors
  in
  (* NOTE: then for each valid constructor, "split" into fresh constructor_args for the next constructor to be applied -- recursively do this until we have fully explored all possible constructors, for all reachable terms [from_term] *)
  let* fully_expanded_constructors : constructor_datatree list =
    expand_constructor_datatrees d [] datatree_constructors
    (* NOTE: we sandbox them individually since otherwise when we "unsandbox" everything and start going through the valid constructors, then they're going to share the same evars and start unifying beyond their scope again. *)
  in
  (* NOTE: next, group each of the [refreshed_constructors] by the first constructor to be applied, and iteratively descend (depth-first) -- agressively culling any that fail sandbox-unification (sandboxed-with their peers), and continue recursively descending until nothing to do *)
  (* TODO: *)
  return []
;;

let collect_valid_constructors
      (d : data)
      (from_term : EConstr.t)
      (raw_constructors : Rocq_utils.ind_constrs)
  : Mebi_constr.t list mm
  =
  let* () = debug_collect_valid_constructors d from_term raw_constructors in
  let* constructors =
    mk_init_constructor_args_list d.lts_enc raw_constructors
  in
  explore_valid_constructors d from_term None constructors
;;
