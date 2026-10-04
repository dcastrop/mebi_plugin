(* Debug and Trace stay off for this module regardless of the user-facing
   configuration. See Logger.Scoped. *)
module Log = Logger.Scoped (struct
    let overrides = [ Output.Kind.Debug, false; Output.Kind.Trace, false ]
  end)

(** [kind_pair] are the arguments of [AtomicType (ty, tys)] returned by e.g., [EConstr.kind_of_type]
*)
type 'a kind_pair = 'a * 'a array

exception
  Rocq_utils_EConstrIsNot_Atomic of
    (Evd.evar_map * EConstr.t * EConstr.kind_of_type)

exception Rocq_utils_EConstrIsNotA_Type of (Evd.evar_map * EConstr.t * string)

(* See the [.mli]. *)
let econstr_to_atomic (sigma : Evd.evar_map) (x : EConstr.t)
  : EConstr.t kind_pair
  =
  try
    match EConstr.kind_of_type sigma x with
    | AtomicType (ty, tys) -> ty, tys
    | k -> raise (Rocq_utils_EConstrIsNot_Atomic (sigma, x, k))
  with
  | Failure e ->
    (* Logger.debug ~__FUNCTION__ e; *)
    raise (Rocq_utils_EConstrIsNotA_Type (sigma, x, e))
;;

type constr_kind =
  ( Constr.t
    , Constr.types
    , Sorts.Quality.t
    , UVars.Instance.t
    , Sorts.relevance )
    Constr.kind_of_term

exception
  Rocq_utils_ConstrIsNot_App of
    (Constr.t
    * ( Constr.t
        , Constr.t
        , Sorts.t
        , UVars.Instance.t
        , Sorts.relevance )
        Constr.kind_of_term)

(* See the [.mli]. *)
let constr_to_app (x : Constr.t) : Constr.t kind_pair =
  match Constr.kind x with
  | App (ty, tys) -> ty, tys
  | k -> raise (Rocq_utils_ConstrIsNot_App (x, k))
;;

type econstr_kind =
  ( EConstr.t
    , EConstr.t
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

exception
  Rocq_utils_EConstrIsNot_App of (Evd.evar_map * EConstr.t * econstr_kind)

(* See the [.mli]. *)
let econstr_to_app (sigma : Evd.evar_map) (x : EConstr.t) : EConstr.t kind_pair =
  match EConstr.kind sigma x with
  | App (ty, tys) -> ty, tys
  | k -> raise (Rocq_utils_EConstrIsNot_App (sigma, x, k))
;;

type lambda_triple =
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot * EConstr.t * EConstr.t

exception
  Rocq_utils_EConstrIsNot_Lambda of (Evd.evar_map * EConstr.t * econstr_kind)

(* See the [.mli]. *)
let econstr_to_lambda (sigma : Evd.evar_map) (x : EConstr.t) : lambda_triple =
  match EConstr.kind sigma x with
  | Lambda (binder, types, constr) -> binder, types, constr
  | k -> raise (Rocq_utils_EConstrIsNot_App (sigma, x, k))
;;

type hyp = (EConstr.t, EConstr.t, Evd.erelevance) Context.Named.Declaration.pt

exception
  Rocq_utils_HypIsNot_Atomic of (Evd.evar_map * hyp * EConstr.kind_of_type)

(* See the [.mli]. *)
let hyp_to_atomic (sigma : Evd.evar_map) (h : hyp) : EConstr.t kind_pair =
  let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
  try econstr_to_atomic sigma h_ty with
  | Rocq_utils_EConstrIsNot_Atomic (sigma, h_ty, k) ->
    raise (Rocq_utils_HypIsNot_Atomic (sigma, h, k))
;;

type ind_constr = Constr.rel_context * Constr.t
type constr_decl = Constr.rel_declaration
type econstr_decl = EConstr.rel_declaration

(* See the [.mli]. *)
let get_econstr_decls (ctx : Constr.rel_context) : econstr_decl list =
  List.map EConstr.of_rel_decl ctx
;;

(* See the [.mli]. *)
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

(* See the [.mli]. *)
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

(* See the [.mli]. *)
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

(* See the [.mli]. *)
let list_of_kinds
      sigma
      (f : Evd.evar_map -> 'a -> (string * bool) list)
      (x : 'a)
  : string list
  =
  List.filter_map (function y, true -> Some y | _, false -> None) (f sigma x)
;;

(* See the [.mli]. *)
let get_decl_type_of_constr (x : constr_decl) : EConstr.t =
  Log.trace __FUNCTION__;
  Context.Rel.Declaration.get_type x |> EConstr.of_constr
;;

(* See the [.mli]. *)
let get_decl_type_of_econstr (x : econstr_decl) : EConstr.t =
  Log.trace __FUNCTION__;
  Context.Rel.Declaration.get_type x
;;

(* See the [.mli]. *)
let get_ind_ty
      (ind : Names.inductive)
      (mib : Declarations.mutual_inductive_body)
  : EConstr.t
  =
  Log.trace __FUNCTION__;
  EConstr.mkIndU (ind, EConstr.EInstance.make mib.mind_univ_hyps)
;;

(* See the [.mli]. *)
let type_of_econstr_rel ?(substl : EConstr.t list option) (t : econstr_decl)
  : EConstr.t
  =
  let ty : EConstr.t = get_decl_type_of_econstr t in
  match substl with None -> ty | Some substl -> EConstr.Vars.substl substl ty
;;

(* See the [.mli]. *)
let type_of_econstr env sigma (x : EConstr.t) : Evd.evar_map * EConstr.t =
  Typing.type_of env sigma x
;;

module Strfy = struct
  (****** ROCQ **********************)

  let pp ?(clean : bool = true) (x : Pp.t) : string =
    let s = Pp.string_of_ppcmds x in
    if clean then Utils.clean_string s else s
  ;;

  let name_id : Names.Id.t -> string = Names.Id.to_string

  let name : Names.Name.t -> string = function
    | Anonymous -> "Anonymous"
    | Name x -> name_id x
  ;;

  let global : Names.GlobRef.t -> string =
    fun (x : Names.GlobRef.t) -> pp (Printer.pr_global x)
  ;;

  let evar : Evar.t -> string = fun (x : Evar.t) -> pp (Evar.print x)

  let evar' env sigma (x : Evar.t) : string =
    pp (Printer.pr_existential_key env sigma x)
  ;;

  let constr env sigma (x : Constr.t) : string =
    pp (Printer.pr_constr_env env sigma x)
  ;;

  let constr_opt env sigma (x : Constr.t option) : string =
    Utils.option_fstr (constr env sigma) x
  ;;

  let constr_rel_decl env sigma (x : constr_decl) : string =
    pp (Printer.pr_rel_decl env sigma x)
  ;;

  let constr_rel_context env sigma (x : Constr.rel_context) : string =
    pp (Printer.pr_rel_context env sigma x)
  ;;

  let ind_constr enc sigma ((x, y) : ind_constr) : string = "TODO: ind_constr"

  let ind_constrs env sigma (xs : ind_constr array) : string =
    "TODO: ind_constrs"
  ;;

  let constr_kind env sigma (x : Constr.t) : string = "TODO: constr_kind"

  let econstr env sigma (x : EConstr.t) : string =
    pp (Printer.pr_econstr_env env sigma x)
  ;;

  let econstr_rel_decl env sigma (x : econstr_decl) : string =
    pp (Printer.pr_erel_decl env sigma x)
  ;;

  let econstr_type
        env
        sigma
        ((name, x, ty, tys) : string * EConstr.t * EConstr.t * EConstr.t array)
    : string
    =
    "TODO: econstr_type"
  ;;

  let econstr_types env sigma (x : EConstr.types) : string =
    "TODO: econstr_types"
  ;;

  let econstr_kind env sigma (x : EConstr.t) : string = "TODO: econstr_kind"
  let concl env sigma : EConstr.constr -> string = econstr_types env sigma

  let erel _env sigma : EConstr.ERelevance.t -> string =
    fun (x : EConstr.ERelevance.t) ->
    if EConstr.ERelevance.is_irrelevant sigma x
    then "irrelevant"
    else "relevant"
  ;;

  let hyp_name (x : hyp) : string = name_id (Context.Named.Declaration.get_id x)

  let hyp_value env sigma (x : hyp) : string =
    Context.Named.Declaration.get_value x
    |> Utils.option_fstr (econstr env sigma)
  ;;

  let hyp_type env sigma (x : hyp) : string =
    econstr env sigma (Context.Named.Declaration.get_type x)
  ;;

  let hyp env sigma (x : hyp) : string = "TODO: hyp"
  let goal (x : Proofview.Goal.t) : string = "TODO: goal"
end

(* The counter behind {!the_next}. Evar names are only ever required to be
   fresh: the counter never goes back, so a name is never handed out twice
   in a session. This produces the same [UnifEvar0], [UnifEvar1], ...
   sequence as the previous cache, which kept the set of every name issued
   and asked [Namegen.next_ident_away] for one not in it. That call restarts
   its search from the base name whenever the candidate is taken -- which it
   always was -- so each new name probed every name before it. With about 80
   evars per state, extracting [Proc/Test4]'s first 600 states took 34s, of
   which ~33s was this. See ASSISTED-CHANGES.md, 2026-10-01 (backlog item
   B3). *)
let the_counter : int ref = ref 0

(* See the [.mli]. *)
let the_next () : Names.Id.t =
  let n : int = !the_counter in
  incr the_counter;
  Names.Id.of_string (Printf.sprintf "UnifEvar%i" n)
;;

exception CouldNotGetNextFreshEvarName of unit

(* See the [.mli]. *)
let get_next_evar
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (a_type : EConstr.t)
  : Evd.evar_map * EConstr.t
  =
  match Evarutil.next_evar_name (Namegen.IntroFresh (the_next ())) with
  | None -> raise (CouldNotGetNextFreshEvarName ())
  | Some (name, _) ->
    let naming = Namegen.IntroFresh name in
    Evarutil.new_evar ~naming env sigma a_type
;;

type evar_source =
  | TypeOf of EConstr.t
  | OfType of EConstr.t

(* See the [.mli]. *)
let get_next (env : Environ.env) (sigma : Evd.evar_map)
  : evar_source -> Evd.evar_map * EConstr.t
  = function
  | TypeOf a_term ->
    let sigma, type_of_a_term = type_of_econstr env sigma a_term in
    get_next_evar env sigma type_of_a_term
  | OfType a_type -> get_next_evar env sigma a_type
;;

(* See the [.mli]. *)
let get_fresh_evar
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (original : evar_source)
  : Evd.evar_map * EConstr.t
  =
  get_next env sigma original
;;

(* See the [.mli]. *)
let subst_of_decl (substl : EConstr.Vars.substl) x : EConstr.t =
  let ty : EConstr.t = Context.Rel.Declaration.get_type x in
  EConstr.Vars.substl substl ty
;;

(* See the [.mli]. *)
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

(* See the [.mli]. *)
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

(* See the [.mli]. *)
let map_decl_evar_pairs (xs : econstr_decl list) (ys : EConstr.Vars.substl)
  : (EConstr.t * Names.Name.t) list
  =
  List.combine ys (List.map Context.Rel.Declaration.get_name xs)
;;

exception ConstructorArgsExpectsArraySize3 of unit

type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

(* See the [.mli]. *)
let constructor_args (args : EConstr.t array) : constructor_args =
  if Int.equal (Array.length args) 3
  then { lhs = args.(0); act = args.(1); rhs = args.(2) }
  else raise (*TODO:err*) (ConstructorArgsExpectsArraySize3 ())
;;

exception Rocq_utils_InvalidLtsArgLength of int
exception Rocq_utils_InvalidLtsTermKind of Constr.t

(* See the [.mli]. *)
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
      args)
    else raise (Rocq_utils_InvalidLtsArgLength (Array.length args))
  | _ -> raise (Rocq_utils_InvalidLtsTermKind term)
;;

exception Rocq_utils_CouldNotExtractBinding of unit

(* See the [.mli]. *)
let unpack_constr_args ((_, tys) : Constr.t kind_pair)
  : Constr.t * Constr.t * Constr.t
  =
  try tys.(0), tys.(1), tys.(2) with
  (* NOTE: in case [tys.(_)] is out of bounds. *)
  | Not_found -> raise (Rocq_utils_CouldNotExtractBinding ())
;;

(* See the [.mli]. *)
let econstr_to_constrexpr env sigma : EConstr.t -> Constrexpr.constr_expr =
  Constrextern.extern_constr ~flags:(PrintingFlags.current ()) env sigma
;;

(* See the [.mli]. *)
let constrexpr_to_econstr env sigma
  : Constrexpr.constr_expr -> Evd.evar_map * EConstr.t
  =
  Constrintern.interp_constr_evars env sigma
;;

(* See the [.mli]. *)
let econstr_to_constr ?(abort_on_undefined_evars : bool = false) sigma
  : EConstr.t -> Constr.t
  =
  EConstr.to_constr ~abort_on_undefined_evars sigma
;;

(* See the [.mli]. *)
let econstr_to_constr_opt sigma : EConstr.t -> Constr.t option =
  EConstr.to_constr_opt sigma
;;

(* See the [.mli]. *)
let globref_to_econstr env : Names.GlobRef.t -> EConstr.t =
  fun x -> EConstr.of_constr (UnivGen.constr_of_monomorphic_global env x)
;;

(* See the [.mli]. *)
let is_constant sigma (x : EConstr.t) (c : unit -> EConstr.t) : bool =
  match Constr.kind (econstr_to_constr sigma x) with
  | App (x, _) -> Constr.equal x (econstr_to_constr sigma (c ()))
  | _ -> false
;;

(* See the [.mli]. *)
let libnames_to_globrefs (xs : Libnames.qualid list) : Names.GlobRef.t list =
  List.map Nametab.global xs
;;

(* See the [.mli]. *)
let extract_benchmark_args
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (x : Constrexpr.constr_expr)
  : Evd.evar_map * Constrexpr.constr_expr list
  =
  let sigma, x = constrexpr_to_econstr env sigma x in
  let x = econstr_to_constrexpr env sigma x in
  sigma, [ x ]
;;
