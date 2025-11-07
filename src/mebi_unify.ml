(* open Logging *)
open Mebi_wrapper
open Mebi_wrapper.Syntax
open Mebi_unification

type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

exception ConstructorArgsExpectsArraySize3 of unit

let constructor_args (args : EConstr.t array) : constructor_args =
  if Int.equal (Array.length args) 3
  then { lhs = args.(0); act = args.(1); rhs = args.(2) }
  else raise (ConstructorArgsExpectsArraySize3 ())
;;

(** creates unification problems between the rhs of the current constructor and the lhs of the next.
*)
let map_problems args : Constructors.t -> Problems.t =
  List.map (fun ((_act, lhs, tree) : Mebi_constr.t) -> (lhs, args.rhs), tree)
;;

let cross_product acc problems =
  List.concat_map
    (fun (x : Problems.t) -> List.map (fun (y : Problem.t) -> y :: x) problems)
    acc
;;

let try_unify_constructor_args
      ?(debug : bool = false)
      (lhs : EConstr.t)
      (act : EConstr.t option)
      (args : constructor_args)
  : bool mm
  =
  let f : Pair.t -> bool mm = Pair.unify ~debug in
  let* lhs_unifies : bool = f (lhs, args.lhs) in
  if lhs_unifies
  then Option.cata (fun act -> f (act, args.act)) (return true) act
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
  let* env = get_env in
  let* sigma = get_sigma in
  let$ vt _ _ = Evarutil.new_evar env sigma subst in
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
      return (constructor_args args))
    else (* TODO: err *) invalid_lts_args_length (Array.length args)
  | _ -> (* TODO: err *) invalid_lts_term_kind term
;;

exception ConstructorNameNotRecognized of (EConstr.t * EConstr.t)

(** Checks possible transitions for this term: *)
let rec check_valid_constructors
          (transitions : (Constr.rel_context * Constr.types) array)
          (indmap : Mebi_ind.t F.t)
          (from_term : EConstr.t)
          (act_term : EConstr.t option)
          (lts_enc : Enc.t)
  : Constructors.t mm
  =
  let* from_term : EConstr.t = Mebi_utils.econstr_normalize from_term in
  let iter_body (i : int) (constructors : Constructors.t) =
    let (ctx, tm) : Constr.rel_context * Constr.t = transitions.(i) in
    let decls : Rocq_utils.econstr_decls = List.map EConstr.of_rel_decl ctx in
    let* substl = mk_ctx_substl [] (List.rev decls) in
    let* args : constructor_args = extract_args ~substl tm in
    let* success = try_unify_constructor_args from_term act_term args in
    if success
    then
      let* act : EConstr.t = Mebi_utils.econstr_normalize args.act in
      let tgt_term : EConstr.t = EConstr.Vars.substl substl args.rhs in
      let* next_constructors : (Enc.t * Problems.t list) option =
        check_updated_ctx lts_enc [ [] ] indmap (substl, decls)
      in
      check_for_next_constructors
        i
        indmap
        act
        tgt_term
        constructors
        next_constructors
    else return constructors
  in
  iterate 0 (Array.length transitions - 1) [] iter_body

and check_for_next_constructors
      (i : int)
      (indmap : Mebi_ind.t F.t)
      (act : EConstr.t)
      (tgt_term : EConstr.t)
      (constructors : Constructors.t)
  : (Enc.t * Problems.t list) option -> Constructors.t mm
  = function
  | None -> return constructors
  | Some next_params ->
    let next_lts_enc, next_problems = next_params in
    (match next_problems with
     | [] ->
       let* sigma = get_sigma in
       if EConstr.isEvar sigma tgt_term
       then return constructors
       else (
         let tree = Mebi_constr.Tree.Node ((next_lts_enc, i), []) in
         return ((act, tgt_term, tree) :: constructors))
     | _ -> Constructors.retrieve i constructors act tgt_term next_params)

(* Should return a list of unification problems *)
and check_updated_ctx
      (lts_enc : Enc.t)
      (acc : Problems.t list)
      (indmap : Mebi_ind.t F.t)
  :  EConstr.Vars.substl * EConstr.rel_declaration list
  -> (Enc.t * Problems.t list) option mm
  = function
  | [], [] -> return (Some (lts_enc, acc))
  | _hsubstl :: substl, t :: tl ->
    let$+ upd_t env sigma =
      EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t)
    in
    let* env = get_env in
    let* sigma = get_sigma in
    (match EConstr.kind sigma upd_t with
     | App (name, args) ->
       (match F.find_opt indmap name with
        | None ->
          (* TODO: err *) raise (ConstructorNameNotRecognized (upd_t, name))
        | Some c ->
          let args = constructor_args args in
          let$+ nextT env sigma = Reductionops.nf_evar sigma args.lhs in
          let* next_lts = Mebi_ind.get_constr_transitions c in
          let* next_constructors : Constructors.t =
            check_valid_constructors next_lts indmap nextT (Some args.act) c.enc
          in
          (match next_constructors with
           | [] -> return None
           | next_constructors ->
             let problems : Problems.t = map_problems args next_constructors in
             let acc : Problems.t list = cross_product acc problems in
             check_updated_ctx lts_enc acc indmap (substl, tl)))
     | _ -> check_updated_ctx lts_enc acc indmap (substl, tl))
  | _substl, _ctxl -> invalid_check_updated_ctx _substl _ctxl
;;
(* ! Impossible ! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)
