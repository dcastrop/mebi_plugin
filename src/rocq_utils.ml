module C : Hashtbl.S with type key = Constr.t = Hashtbl.Make (struct
    type t = Constr.t

    let equal : t -> t -> bool = Constr.equal
    let hash : t -> int = Constr.hash
  end)

(*****************************************************************************)

(** [kind_pair] are the arguments of [AtomicType (ty, tys)] returned by e.g., [EConstr.kind_of_type]
*)
type 'a kind_pair = 'a * 'a array

exception
  Rocq_utils_EConstrIsNot_Atomic of
    (Evd.evar_map * EConstr.t * EConstr.kind_of_type)

exception Rocq_utils_EConstrIsNotA_Type of (Evd.evar_map * EConstr.t * string)

let econstr_to_atomic (sigma : Evd.evar_map) (x : EConstr.t)
  : EConstr.t kind_pair
  =
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
  ( Constr.t
    , Constr.types
    , Sorts.t
    , UVars.Instance.t
    , Sorts.relevance )
    Constr.kind_of_term

exception Rocq_utils_ConstrIsNot_App of (Constr.t * constr_kind)

let constr_to_app (x : Constr.t) : Constr.t kind_pair =
  match Constr.kind x with
  | App (ty, tys) -> ty, tys
  | k -> raise (Rocq_utils_ConstrIsNot_App (x, k))
;;

(*****************************************************************************)

type econstr_kind =
  ( EConstr.t
    , EConstr.t
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

exception
  Rocq_utils_EConstrIsNot_App of (Evd.evar_map * EConstr.t * econstr_kind)

let econstr_to_app (sigma : Evd.evar_map) (x : EConstr.t) : EConstr.t kind_pair =
  match EConstr.kind sigma x with
  | App (ty, tys) -> ty, tys
  | k -> raise (Rocq_utils_EConstrIsNot_App (sigma, x, k))
;;

(*****************************************************************************)

type lambda_triple =
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot * EConstr.t * EConstr.t

exception
  Rocq_utils_EConstrIsNot_Lambda of (Evd.evar_map * EConstr.t * econstr_kind)

let econstr_to_lambda (sigma : Evd.evar_map) (x : EConstr.t) : lambda_triple =
  match EConstr.kind sigma x with
  | Lambda (binder, types, constr) -> binder, types, constr
  | k -> raise (Rocq_utils_EConstrIsNot_App (sigma, x, k))
;;

(*****************************************************************************)

type hyp = (EConstr.t, EConstr.t, Evd.erelevance) Context.Named.Declaration.pt

exception
  Rocq_utils_HypIsNot_Atomic of (Evd.evar_map * hyp * EConstr.kind_of_type)

let hyp_to_atomic (sigma : Evd.evar_map) (h : hyp) : EConstr.t kind_pair =
  let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
  try econstr_to_atomic sigma h_ty with
  | Rocq_utils_EConstrIsNot_Atomic (sigma, h_ty, k) ->
    raise (Rocq_utils_HypIsNot_Atomic (sigma, h, k))
;;

(*****************************************************************************)

type ind_constr = Constr.rel_context * Constr.t
type constr_decl = Constr.rel_declaration
type econstr_decl = EConstr.rel_declaration

let get_econstr_decls (ctx : Constr.rel_context) : econstr_decl list =
  List.map EConstr.of_rel_decl ctx
;;

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

let list_of_econstr_kinds sigma (x : EConstr.t) : (string * bool) list =
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
  ; "Sort", EConstr.isSort sigma x
  ; "Type", EConstr.isType sigma x
  ; "Var", EConstr.isVar sigma x
  ]
;;

let list_of_econstr_kinds_of_type sigma (x : EConstr.t) : (string * bool) list =
  [ ( "SortType"
    , try
        match EConstr.kind_of_type sigma x with
        | SortType _ -> true
        | _ -> false
      with
      | Failure _ -> false )
  ; ( "CastType"
    , try
        match EConstr.kind_of_type sigma x with
        | CastType _ -> true
        | _ -> false
      with
      | Failure _ -> false )
  ; ( "ProdType"
    , try
        match EConstr.kind_of_type sigma x with
        | ProdType _ -> true
        | _ -> false
      with
      | Failure _ -> false )
  ; ( "LetInType"
    , try
        match EConstr.kind_of_type sigma x with
        | LetInType _ -> true
        | _ -> false
      with
      | Failure _ -> false )
  ; ( "AtomicType "
    , try
        match EConstr.kind_of_type sigma x with
        | AtomicType _ -> true
        | _ -> false
      with
      | Failure _ -> false )
  ]
;;

let list_of_kinds
      sigma
      (f : Evd.evar_map -> 'a -> (string * bool) list)
      (x : 'a)
  : string list
  =
  List.filter_map (function y, true -> Some y | _, false -> None) (f sigma x)
;;

(*****************************************************************************)

let get_decl_type_of_constr (x : constr_decl) : EConstr.t =
  Context.Rel.Declaration.get_type x |> EConstr.of_constr
;;

let get_decl_type_of_econstr (x : econstr_decl) : EConstr.t =
  Context.Rel.Declaration.get_type x
;;

(** [get_ind_ty ind mib] *)
let get_ind_ty
      (ind : Names.inductive)
      (mib : Declarations.mutual_inductive_body)
  : EConstr.t
  =
  EConstr.mkIndU (ind, EConstr.EInstance.make mib.mind_univ_hyps)
;;

(*****************************************************************************)

let type_of_econstr_rel ?(substl : EConstr.t list option) (t : econstr_decl)
  : EConstr.t
  =
  let ty : EConstr.t = get_decl_type_of_econstr t in
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

  let constr env sigma (x : Constr.t) : string =
    pp (Printer.pr_constr_env env sigma x)
  ;;

  let constr_opt env sigma ?(args : style_args = style_args ())
    : Constr.t option -> string
    =
    option (Utils.Strfy.Of (constr env sigma))
  ;;

  let constr_rel_decl
        env
        sigma
        ?(args : style_args = style_args ())
        (x : constr_decl)
    : string
    =
    pp (Printer.pr_rel_decl env sigma x)
  ;;

  let constr_rel_context env sigma (x : Constr.rel_context) : string =
    pp (Printer.pr_rel_context env sigma x)
  ;;

  (*****************************************************************************)

  let ind_constr enc sigma ((x, y) : ind_constr) : string =
    let x : string = constr_rel_context enc sigma x in
    let y : string = constr enc sigma y in
    Utils.Strfy.record [ "constr", y; "rel context", x ]
  ;;

  let ind_constrs env sigma (xs : ind_constr array) : string =
    Utils.Strfy.array (Of (ind_constr env sigma)) xs
  ;;

  (*****************************************************************************)

  let constr_kind env sigma ?(args : style_args = style_args ()) (x : Constr.t)
    : string
    =
    let k : string =
      list
        ~args:
          { args with
            name = Some "Constr_kinds"
          ; style = Some (collection_style Record)
          }
        (Args string)
        (List.filter_map
           (fun (kind, isKind) -> if isKind then Some kind else None)
           (list_of_constr_kinds x))
    in
    let x : string = constr env sigma x in
    Utils.Strfy.record [ "constr", x; "kinds", k ]
  ;;

  (*****************************************************************************)

  let econstr env sigma (x : EConstr.t) : string =
    pp (Printer.pr_econstr_env env sigma x)
  ;;

  let feconstr env sigma : EConstr.t Utils.Strfy.to_string =
    Of (econstr env sigma)
  ;;

  (*****************************************************************************)

  let econstr_rel_decl env sigma (x : econstr_decl) : string =
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
        (feconstr env sigma)
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
        (econstr env sigma x)
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
          { args with
            name = Some "EConstr_kinds"
          ; style = Some (collection_style Record)
          }
        (Args string)
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
          { args with
            name = Some "Hypotheses"
          ; style = Some (collection_style Record)
          }
        (Args (hyp env sigma))
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

let get_fresh_evar
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (original : evar_source)
  : Evd.evar_map * EConstr.t
  =
  get_next env sigma original
;;

(*********************************************************)

let subst_of_decl (substl : EConstr.Vars.substl) x : EConstr.t =
  let ty : EConstr.t = Context.Rel.Declaration.get_type x in
  EConstr.Vars.substl substl ty
;;

(** [mk_ctx_subst ?substl x] returns a new [evar] made from the type of [x], using any [substl] provided.
    @param ?substl
      is a list of substitutions, (* TODO: provided so that collisions don't occur? *)
    @param x
      corresponds to a (* TODO: universally? *) quantified term of a constructor.
    @return a new [evar] for [x]. *)
let mk_ctx_subst
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (substl : EConstr.Vars.substl)
      (x : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt)
  : Evd.evar_map * EConstr.t
  =
  let subst : EConstr.t = subst_of_decl substl x in
  Evarutil.new_evar env sigma subst
;;

(** [mk_ctx_substl acc ts] makes an [evar] for each term declaration in [ts].
    @param acc
      contains the substitutions accumulated so far, and is returned once [ts=[]]
    @param ts
      is an [EConstr.rel_declaration list] (obtained from the context of a constructor).
    @return [acc] of [evars] once [ts] is empty. *)
let rec mk_ctx_substl
          (env : Environ.env)
          (sigma : Evd.evar_map)
          (acc : EConstr.Vars.substl)
  :  ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
  -> Evd.evar_map * EConstr.Vars.substl
  = function
  | [] -> sigma, acc
  | t :: ts ->
    let sigma, vt = mk_ctx_subst env sigma acc t in
    mk_ctx_substl env sigma (vt :: acc) ts
;;

(** returns tuple list of [(binding_name * evar)] -- TODO: map these to the [_UNBOUND_REL_X] and
*)
let map_decl_evar_pairs (xs : econstr_decl list) (ys : EConstr.Vars.substl)
  : (EConstr.t * Names.Name.t) list
  =
  List.combine ys (List.map Context.Rel.Declaration.get_name xs)
;;

(***********************************************************************)

exception ConstructorArgsExpectsArraySize3 of unit

type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

let constructor_args (args : EConstr.t array) : constructor_args =
  if Int.equal (Array.length args) 3
  then { lhs = args.(0); act = args.(1); rhs = args.(2) }
  else raise (*TODO:err*) (ConstructorArgsExpectsArraySize3 ())
;;

exception Rocq_utils_InvalidLtsArgLength of int
exception Rocq_utils_InvalidLtsTermKind of Constr.t

(** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
    @param ?substl
      is a list of substitutions applied to the terms prior to being returned.
    @param term
      must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
    @return a triple of [lhs_term, action, rhs_term]. *)
let extract_args ?(substl : EConstr.Vars.substl = []) (term : Constr.t)
  : constructor_args
  =
  match Constr.kind term with
  | App (_name, args) ->
    if Array.length args == 3
    then (
      let args = EConstr.of_constr_array args in
      let args = Array.map (EConstr.Vars.substl substl) args in
      let args = constructor_args args in
      (* let* () = debug_extract_args _name args in *)
      args)
    else raise (Rocq_utils_InvalidLtsArgLength (Array.length args))
  | _ -> raise (Rocq_utils_InvalidLtsTermKind term)
;;

exception Rocq_utils_CouldNotExtractBinding of unit

let unpack_constr_args ((_, tys) : Constr.t kind_pair)
  : Constr.t * Constr.t * Constr.t
  =
  try tys.(0), tys.(1), tys.(2) with
  (* NOTE: in case [tys.(_)] is out of bounds. *)
  | Not_found -> raise (Rocq_utils_CouldNotExtractBinding ())
;;
