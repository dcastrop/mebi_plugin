(** [kind_pair] are the arguments of [AtomicType (ty, tys)] returned by e.g., [EConstr.kind_of_type]
*)
type kind_pair = EConstr.t * EConstr.t array

exception
  Rocq_utils_EConstrIsNot_Atomic of
    (Evd.evar_map * EConstr.t * EConstr.kind_of_type)

exception Rocq_utils_EConstrIsNotA_Type of (Evd.evar_map * EConstr.t * string)

let econstr_to_atomic (sigma : Evd.evar_map) (x : EConstr.t) : kind_pair =
  try
    match EConstr.kind_of_type sigma x with
    | AtomicType (ty, tys) -> ty, tys
    | k -> raise (Rocq_utils_EConstrIsNot_Atomic (sigma, x, k))
  with
  | Failure e ->
    (* Logger.Default.debug ~__FUNCTION__ e; *)
    raise (Rocq_utils_EConstrIsNotA_Type (sigma, x, e))
;;

(*****************************************************************************)

type constr_kind =
  ( EConstr.t
    , EConstr.t
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

exception
  Rocq_utils_EConstrIsNot_App of (Evd.evar_map * EConstr.t * constr_kind)

let econstr_to_app (sigma : Evd.evar_map) (x : EConstr.t) : kind_pair =
  match EConstr.kind sigma x with
  | App (ty, tys) -> ty, tys
  | k -> raise (Rocq_utils_EConstrIsNot_App (sigma, x, k))
;;

(*****************************************************************************)

type lambda_triple =
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot * EConstr.t * EConstr.t

exception
  Rocq_utils_EConstrIsNot_Lambda of (Evd.evar_map * EConstr.t * constr_kind)

let econstr_to_lambda (sigma : Evd.evar_map) (x : EConstr.t) : lambda_triple =
  match EConstr.kind sigma x with
  | Lambda (binder, types, constr) -> binder, types, constr
  | k -> raise (Rocq_utils_EConstrIsNot_App (sigma, x, k))
;;

(*****************************************************************************)

type hyp = (EConstr.t, EConstr.t, Evd.erelevance) Context.Named.Declaration.pt

exception
  Rocq_utils_HypIsNot_Atomic of (Evd.evar_map * hyp * EConstr.kind_of_type)

let hyp_to_atomic (sigma : Evd.evar_map) (h : hyp) : kind_pair =
  let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
  try econstr_to_atomic sigma h_ty with
  | Rocq_utils_EConstrIsNot_Atomic (sigma, h_ty, k) ->
    raise (Rocq_utils_HypIsNot_Atomic (sigma, h, k))
;;

(*****************************************************************************)

type ind_constr = Constr.rel_context * Constr.t
type ind_constrs = ind_constr array
type econstr_decl = EConstr.rel_declaration
type econstr_decls = econstr_decl list

let list_of_constr_kinds : Constr.t -> (string * bool) list =
  fun (x : Constr.t) ->
  [ "App", Constr.isApp x
  ; "Case", Constr.isCase x
  ; "Cast", Constr.isCast x
  ; "CoFix", Constr.isCoFix x
  ; "Const", Constr.isConst x
  ; "Construct", Constr.isConstruct x
  ; "Evar", Constr.isEvar x
  ; "Fix", Constr.isFix x
  ; "Ind", Constr.isInd x
  ; "Prod", Constr.isProd x
  ; "Lambda", Constr.isLambda x
  ; "LetIn", Constr.isLetIn x
  ; "Meta", Constr.isMeta x
  ; "Proj", Constr.isProj x
  ; "Rel", Constr.isRel x
  ; "Ref", Constr.isRef x
  ; "Sort", Constr.isSort x
  ; "Var", Constr.isVar x
  ]
;;

let list_of_econstr_kinds sigma : EConstr.t -> (string * bool) list =
  fun (x : EConstr.t) ->
  [ "App", EConstr.isApp sigma x
  ; "Arity", EConstr.isArity sigma x
  ; "Case", EConstr.isCase sigma x
  ; "Cast", EConstr.isCast sigma x
  ; "CoFix", EConstr.isCoFix sigma x
  ; "Const", EConstr.isConst sigma x
  ; "Construct", EConstr.isConstruct sigma x
  ; "Evar", EConstr.isEvar sigma x
  ; "Fix", EConstr.isFix sigma x
  ; "Ind", EConstr.isInd sigma x
  ; "Prod", EConstr.isProd sigma x
  ; "Lambda", EConstr.isLambda sigma x
  ; "LetIn", EConstr.isLetIn sigma x
  ; "Meta", EConstr.isMeta sigma x
  ; "Proj", EConstr.isProj sigma x
  ; "Rel", EConstr.isRel sigma x
  ; "Ref", EConstr.isRef sigma x
  ; "Sort", EConstr.isSort sigma x (* ; "Type", EConstr.isType sigma x *)
  ; "Var", EConstr.isVar sigma x
  ]
;;

(*****************************************************************************)

let type_of_econstr_rel
      ?(substl : EConstr.t list option)
      (t : EConstr.rel_declaration)
  : EConstr.t
  =
  let ty : EConstr.t = Context.Rel.Declaration.get_type t in
  match substl with None -> ty | Some substl -> EConstr.Vars.substl substl ty
;;

let type_of_econstr env sigma (x : EConstr.t) : Evd.evar_map * EConstr.t =
  Typing.type_of env sigma x
;;

module Strfy = struct
  open Utils.Strfy

  (**********************************)
  (****** ROCQ **********************)
  (**********************************)

  let pp ?(clean : bool = true) ?(args : style_args = style_args ()) (x : Pp.t)
    : string
    =
    let s = Pp.string_of_ppcmds x in
    if clean then Utils.clean_string s else s
  ;;

  let evar ?(args : style_args = style_args ()) : Evar.t -> string =
    fun (x : Evar.t) -> pp (Evar.print x)
  ;;

  let evar' env sigma ?(args : style_args = style_args ()) (x : Evar.t) : string
    =
    pp (Printer.pr_existential_key env sigma x)
  ;;

  let constr env sigma ?(args : style_args = style_args ()) (x : Constr.t)
    : string
    =
    pp (Printer.pr_constr_env env sigma x)
  ;;

  let constr_opt env sigma ?(args : style_args = style_args ())
    : Constr.t option -> string
    =
    option (constr env sigma)
  ;;

  let constr_rel_decl
        env
        sigma
        ?(args : style_args = style_args ())
        (x : Constr.rel_declaration)
    : string
    =
    pp (Printer.pr_rel_decl env sigma x)
  ;;

  let constr_rel_context
        env
        sigma
        ?(args : style_args = style_args ())
        (x : Constr.rel_context)
    : string
    =
    pp (Printer.pr_rel_context env sigma x)
  ;;

  let constr_kind env sigma ?(args : style_args = style_args ()) (x : Constr.t)
    : string
    =
    let k : string =
      list
        ~args:
          (style_args
             ~name:"Constr_kinds"
             ~style:(Some (collection_style Record))
             ())
        string
        (List.filter_map
           (fun (kind, isKind) -> if isKind then Some kind else None)
           (list_of_constr_kinds x))
    in
    let x : string = constr env sigma x in
    Utils.Strfy.record [ "constr", x; "kinds", k ]
  ;;

  let econstr env sigma ?(args : style_args = style_args ()) (x : EConstr.t)
    : string
    =
    pp (Printer.pr_econstr_env env sigma x)
  ;;

  let econstr_rel_decl
        env
        sigma
        ?(args : style_args = style_args ())
        (x : EConstr.rel_declaration)
    : string
    =
    pp (Printer.pr_erel_decl env sigma x)
  ;;

  let econstr_type
        env
        sigma
        ?(args : style_args = style_args ())
        ((name, x, ty, tys) : string * EConstr.t * EConstr.t * EConstr.t array)
    : string
    =
    let name : string = Printf.sprintf "%s Type Arguments" name in
    let tys : string =
      array
        ~args:(style_args ~name ~style:(Some (collection_style Record)) ())
        (econstr env sigma)
        tys
    in
    let x : string = econstr env sigma x in
    let ty : string = econstr env sigma ty in
    Utils.Strfy.record [ "econstr", x; "type", ty; "args", tys ]
  ;;

  let econstr_types
        env
        sigma
        ?(args : style_args = style_args ())
        (x : EConstr.types)
    : string
    =
    let oops (k : string) : string =
      Printf.sprintf
        "Rocq_utils.Strfy.econstr_types, unimplemented kind_of_type %s for:\n\
        \ %s"
        k
        (econstr env sigma ~args x)
    in
    match EConstr.kind_of_type sigma x with
    | AtomicType (ty, tys) -> econstr_type env sigma ~args ("Atomic", x, ty, tys)
    | CastType (_ty1, _ty2) -> oops "CastType"
    | LetInType (_name_binder_annot, _t1, _t2, _t3) -> oops "LetInType"
    | ProdType (_name_binder_annot, _t1, _t2) -> oops "LetInType"
    | SortType _sorts -> oops "SortType"
  ;;

  let econstr_kind
        env
        sigma
        ?(args : style_args = style_args ())
        (x : EConstr.t)
    : string
    =
    let k : string =
      list
        ~args:
          (style_args
             ~name:"EConstr_kinds"
             ~style:(Some (collection_style Record))
             ())
        string
        (List.filter_map
           (fun (kind, isKind) -> if isKind then Some kind else None)
           (list_of_econstr_kinds sigma x))
    in
    let x : string = econstr env sigma x in
    Utils.Strfy.record [ "econstr", x; "kinds", k ]
  ;;

  let name_id ?(args : style_args = style_args ()) : Names.Id.t -> string =
    Names.Id.to_string
  ;;

  let global ?(args : style_args = style_args ()) : Names.GlobRef.t -> string =
    fun (x : Names.GlobRef.t) -> pp (Printer.pr_global x)
  ;;

  let concl env sigma ?(args : style_args = style_args ())
    : EConstr.constr -> string
    =
    econstr_types ~args:(nest args) env sigma
  ;;

  let erel _env sigma ?(args : style_args = style_args ())
    : EConstr.ERelevance.t -> string
    =
    fun (x : EConstr.ERelevance.t) ->
    if EConstr.ERelevance.is_irrelevant sigma x
    then "irrelevant"
    else "relevant"
  ;;

  let hyp env sigma ?(args : style_args = style_args ()) (x : hyp) : string =
    let name : string = name_id ~args (Context.Named.Declaration.get_id x) in
    let rel : string =
      erel env sigma ~args (Context.Named.Declaration.get_relevance x)
    in
    let tys : string =
      econstr_types
        env
        sigma
        ~args:(nest args)
        (Context.Named.Declaration.get_type x)
    in
    Utils.Strfy.record [ "name", name; "rel", rel; "tys", tys ]
  ;;

  let goal ?(args : style_args = style_args ()) (x : Proofview.Goal.t) : string =
    let env : Environ.env = Proofview.Goal.env x in
    let sigma : Evd.evar_map = Proofview.Goal.sigma x in
    let concl = concl env sigma ~args:(nest args) (Proofview.Goal.concl x) in
    let hyps : string =
      list
        ~args:
          (style_args
             ~name:"Hypotheses"
             ~style:(Some (collection_style Record))
             ())
        (hyp env sigma)
        (Proofview.Goal.hyps x)
    in
    Utils.Strfy.record [ "concl", concl; "hyps", hyps ]
  ;;
end

type cache =
  { the_prev : Names.Id.Set.t
  ; the_next : Names.Id.t
  }

let the_cache : cache option ref = ref None
let the_default_next () : Names.Id.t = Names.Id.of_string "UnifEvar0"

let the_prev () : Names.Id.Set.t =
  Option.cata (fun x -> x.the_prev) Names.Id.Set.empty !the_cache
;;

let the_next () : Names.Id.t =
  Namegen.next_ident_away
    (Option.cata (fun x -> x.the_next) (the_default_next ()) !the_cache)
    (the_prev ())
;;

exception CouldNotGetNextFreshEvarName of unit

let get_next_evar
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (a_type : EConstr.t)
  : Evd.evar_map * EConstr.t
  =
  match Evarutil.next_evar_name sigma (Namegen.IntroFresh (the_next ())) with
  | None -> raise (CouldNotGetNextFreshEvarName ())
  | Some name ->
    the_cache
    := Some { the_prev = Names.Id.Set.add name (the_prev ()); the_next = name };
    let naming = Namegen.IntroFresh name in
    Evarutil.new_evar ~naming env sigma a_type
;;

type evar_source =
  | TypeOf of EConstr.t
  | OfType of EConstr.t

let get_next (env : Environ.env) (sigma : Evd.evar_map)
  : evar_source -> Evd.evar_map * EConstr.t
  = function
  | TypeOf a_term ->
    let sigma, type_of_a_term = type_of_econstr env sigma a_term in
    get_next_evar env sigma type_of_a_term
  | OfType a_type -> get_next_evar env sigma a_type
;;
