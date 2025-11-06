open Logging
open Mebi_wrapper
open Mebi_wrapper.Syntax
(* open Mebi_unification *)

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
    let from_str : string = Strfy.econstr env sigma from_term in
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
       | None -> (* TODO: err *) raise (ConstructorNameNotRecognized (x, name))
       | Some ind ->
         let lts_enc = ind.index in
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

let constructor_args_to_string env sigma : constructor_args -> string = function
  | { constructor; decls; substl; tree; lhs; act; rhs } ->
    let lhs_str : string = Strfy.econstr env sigma lhs in
    let act_str : string = Strfy.econstr env sigma act in
    let rhs_str : string = Strfy.econstr env sigma rhs in
    let tree_str : string = Strfy.constr_tree tree in
    let f_decl_str = Strfy.econstr_rel_decl env sigma in
    let decls_str : string = Strfy.list f_decl_str decls in
    let substl_str : string =
      Strfy.list ~force_newline:true (Strfy.econstr env sigma) substl
    in
    let constructor_str : string = Strfy.ind_constr env sigma constructor in
    let f = Strfy.tuple ~is_keyval:true Strfy.str Strfy.str in
    Strfy.list
      ~force_newline:true
      f
      [ "lhs", lhs_str
      ; "act", act_str
      ; "rhs", rhs_str
      ; "tree", tree_str
      ; "decls", decls_str
      ; "substl", substl_str
      ; "constructor", constructor_str
      ]
;;

let debug_constructor_args p x : unit mm =
  state (fun env sigma ->
    let s : string = constructor_args_to_string env sigma x in
    Log.debug (Printf.sprintf "%sconstructor_args:\n%s" (Utils.prefix p) s);
    sigma, ())
;;

let constructor_args_list_to_string env sigma : constructor_args list -> string =
  Strfy.list ~force_newline:true (constructor_args_to_string env sigma)
;;

let debug_constructor_args_list p x : unit mm =
  state (fun env sigma ->
    let s : string = constructor_args_list_to_string env sigma x in
    Log.debug (Printf.sprintf "%sconstructor_args list:\n%s" (Utils.prefix p) s);
    sigma, ())
;;

let debug_expand_constructor_args_list d acc tl : unit mm =
  let s : string = Enc.to_string d.lts_enc in
  let p : string =
    Printf.sprintf "expand (%s) %i / %i" s (List.length acc) (List.length tl)
  in
  debug_constructor_args_list p acc
;;

let is_evar sigma : EConstr.t -> bool = EConstr.isEvar sigma
let lhs_is_evar sigma { lhs; _ } : bool = is_evar sigma lhs
let act_is_evar sigma { act; _ } : bool = is_evar sigma act
let rhs_is_evar sigma { rhs; _ } : bool = is_evar sigma rhs

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

(** [expand_constructor_args_list d acc constructor_args_list] recursively calls [update_constructor_args d] for each [constructor_args] in [constructor_args_list], effectively replacing the original with a list of new [constructor_args]. For any returned, any [evars] have been refreshed and replaced to ensure that each valid trace of constructors that can be applied after this point do not interefere with each other.
*)
let rec expand_constructor_args_list (d : data) (acc : constructor_args list)
  : constructor_args list -> constructor_args list mm
  = function
  | [] ->
    let* () = debug_expand_constructor_args_list d acc [] in
    return acc
  | h :: t ->
    let* () = debug_expand_constructor_args_list d acc t in
    let* refreshed : constructor_args list = update_constructor_args d h in
    expand_constructor_args_list d (List.append refreshed acc) t

and update_constructor_args (d : data)
  : constructor_args -> constructor_args list mm
  = function
  | the_constructor_args ->
    let { decls; substl; _ } = the_constructor_args in
    (* TODO: make a map to keep track of the changed/split evars *)
    (* let change_map : EConstr.t F.t = F.create 0 in  *)
    (* TODO: see [update_sigma] and [get_ind_constrs_opt] *)
    (* NOTE: we want to turn a single [constructor_args] into a list, where any [evars] in lhs,act,rhs are replaced with new ones, and when we copy over [substl] and [decls] to the new [constructor_args] we need to make sure to replace the old-evars with the corresponding new ones too. *)
    let iter_body (i : int) (acc : constructor_args list)
      : constructor_args list mm
      =
      let decl : Rocq_utils.econstr_decl = List.nth decls i in
      let substl : EConstr.Vars.substl = List.drop i substl in
      let* premise : EConstr.t = subst_of_decl substl decl in
      let* constrs_opt = get_ind_constrs_opt premise d.ind_map in
      match constrs_opt with
      (* NOTE: no other constructors to apply for [premise] *)
      | None ->
        (* TODO: check if this has any evars? if yes then we need to create a new one ??? *)
        return acc
      (* NOTE: found applicable constructor for [premise] *)
      | Some (next_lts_enc, next_constructors, next_args) ->
        let* split_evars, next_constructor_args_list =
          split_constructor_args
            { d with lts_enc = next_lts_enc }
            next_constructors
            next_args
            the_constructor_args
        in
        return (List.append acc next_constructor_args_list)
    in
    iterate 0 (List.length decls - 1) [] iter_body

and split_constructor_args (d : data) next_constructors next_args
  : constructor_args -> (split_evar list * constructor_args list) mm
  = function
  | { constructor; decls; substl; tree; lhs; act; rhs } ->
    let iter_body
          (i : int)
          ((split_evars, acc) : split_evar list * constructor_args list)
      : (split_evar list * constructor_args list) mm
      =
      let* new_constructor_args =
        mk_constructor_args d.lts_enc i next_constructors.(i)
      in
      let _tree = Mebi_constr.Tree.add new_constructor_args.tree tree in
      (* TODO: how to update the [decls] and [substl]? *)
      (* TODO: at what point do we start sandbox-unifying stuff again? *)
      return (split_evars, acc)
    in
    iterate 0 (List.length decls - 1) ([], []) iter_body
;;

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

(******************************************************************************)

(* *)
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

let rec filter_valid_constructors
          (lhs : EConstr.t)
          (act : EConstr.t option)
          (acc : constructor_args list)
  : constructor_args list -> constructor_args list mm
  = function
  | [] -> return acc
  | h :: t ->
    let* constructor_applies : bool = does_constructor_apply lhs act h in
    if constructor_applies
    then filter_valid_constructors lhs act (h :: acc) t
    else filter_valid_constructors lhs act acc t
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
  (* NOTE: then for each valid constructor, "split" into fresh constructor_args for the next constructor to be applied -- recursively do this until we have fully explored all possible constructors, for all reachable terms [from_term] *)
  let* fully_expanded_constructors =
    expand_constructor_args_list d [] valid_constructors
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
