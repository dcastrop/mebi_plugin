type hyp = (EConstr.t, EConstr.t, Evd.erelevance) Context.Named.Declaration.pt

type constr_kind =
  ( EConstr.t
    , EConstr.t
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

type ind_constr = Constr.rel_context * Constr.t
type ind_constrs = ind_constr array
type econstr_decl = EConstr.rel_declaration
type econstr_decls = econstr_decl list

(*****************************************************************************)

let type_of_econstr_rel
      ?(substl : EConstr.t list option)
      (t : EConstr.rel_declaration)
  : EConstr.t
  =
  let ty : EConstr.t = Context.Rel.Declaration.get_type t in
  match substl with None -> ty | Some substl -> EConstr.Vars.substl substl ty
;;

module Strfy = struct
  open Utils
  open Utils.Strfy

  (**********************************)
  (****** ROCQ **********************)
  (**********************************)

  let pp ?(clean : bool = true) (x : Pp.t) : string =
    let s = Pp.string_of_ppcmds x in
    if clean then Utils.clean_string s else s
  ;;

  let evar : Evar.t -> string = fun (x : Evar.t) -> pp (Evar.print x)

  let evar' env sigma : Evar.t -> string =
    fun (x : Evar.t) -> pp (Printer.pr_existential_key env sigma x)
  ;;

  let constr env sigma : Constr.t -> string =
    fun (x : Constr.t) -> pp (Printer.pr_constr_env env sigma x)
  ;;

  let constr_opt env sigma : Constr.t option -> string =
    fun (x : Constr.t option) -> option (constr env sigma) x
  ;;

  let constr_rel_decl env sigma : Constr.rel_declaration -> string =
    fun (x : Constr.rel_declaration) -> pp (Printer.pr_rel_decl env sigma x)
  ;;

  let constr_rel_context env sigma : Constr.rel_context -> string =
    fun (x : Constr.rel_context) -> pp (Printer.pr_rel_context env sigma x)
  ;;

  let constr_kind ?(indent : int = 0) env sigma : Constr.t -> string =
    fun (x : Constr.t) ->
    list
      ~indent:(indent + 1)
      ~use:("{", "}")
      str
      [ tuple ~is_keyval:true ~indent str (constr env sigma) ("constr", x)
      ; list
          ~label:"Constr kinds"
          ~indent:(indent + 1)
          str
          (List.filter_map
             (fun (kind, isKind) -> if isKind then Some kind else None)
             (Utils.list_of_constr_kinds x))
      ]
  ;;

  let econstr env sigma : EConstr.t -> string =
    fun (x : EConstr.t) -> pp (Printer.pr_econstr_env env sigma x)
  ;;

  let econstr_rel_decl env sigma : EConstr.rel_declaration -> string =
    fun (x : EConstr.rel_declaration) -> pp (Printer.pr_erel_decl env sigma x)
  ;;

  let econstr_types ?(indent : int = 0) env sigma : EConstr.types -> string =
    fun (x : EConstr.types) ->
    match EConstr.kind_of_type sigma x with
    | AtomicType (ty, tys) ->
      Printf.sprintf
        "%s => \n%s%s"
        (econstr env sigma ty)
        (str_tabs indent)
        (list
           ~force_newline:true
           ~label:"Type Arguments"
           ~indent
           (econstr env sigma)
           (Array.to_list tys))
    | CastType (_tys, _ty) -> "TODO: CastType"
    | LetInType (_name_binder_annot, _t1, _t2, _t3) -> "TODO: LetInType"
    | ProdType (_name_binder_annot, _t1, _t2) -> "TODO: ProdType"
    | SortType _sorts -> "TODO: SortType"
  ;;

  let econstr_kind ?(indent : int = 0) env sigma : EConstr.t -> string =
    fun (x : EConstr.t) ->
    list
      ~indent:(indent + 1)
      ~use:("{", "}")
      str
      [ tuple ~is_keyval:true ~indent str (econstr env sigma) ("econstr", x)
      ; list
          ~label:"EConstr kinds"
          ~indent:(indent + 1)
          str
          (List.filter_map
             (fun (kind, isKind) -> if isKind then Some kind else None)
             (Utils.list_of_econstr_kinds sigma x))
      ]
  ;;

  let name_id : Names.Id.t -> string = Names.Id.to_string

  let global : Names.GlobRef.t -> string =
    fun (x : Names.GlobRef.t) -> pp (Printer.pr_global x)
  ;;

  let concl ?(indent : int = 0) env sigma : EConstr.constr -> string =
    fun (x : EConstr.constr) -> econstr_types ~indent:(indent + 1) env sigma x
  ;;

  let erel _env sigma : EConstr.ERelevance.t -> string =
    fun (x : EConstr.ERelevance.t) ->
    if EConstr.ERelevance.is_irrelevant sigma x
    then "irrelevant"
    else "relevant"
  ;;

  let hyp ?(force_newline : bool = false) ?(indent : int = 0) env sigma
    : hyp -> string
    =
    fun (x : hyp) ->
    list
      ~force_newline:true
      ~indent:(indent + 1)
      ~use:("{", "}")
      (tuple ~is_keyval:true ~indent:(indent + 0) str str)
      [ "name", name_id (Context.Named.Declaration.get_id x)
      ; "rel", erel env sigma (Context.Named.Declaration.get_relevance x)
      ; ( "tys"
        , econstr_types
            ~indent:(indent + 2)
            env
            sigma
            (Context.Named.Declaration.get_type x) )
      ]
  ;;

  let goal ?(indent : int = 0) : Proofview.Goal.t -> string =
    fun (x : Proofview.Goal.t) ->
    let env : Environ.env = Proofview.Goal.env x in
    let sigma : Evd.evar_map = Proofview.Goal.sigma x in
    let concl_str =
      concl env sigma ~indent:(indent + 2) (Proofview.Goal.concl x)
    in
    let hyps_str =
      Printf.sprintf
        "\n%s%s"
        (str_tabs (indent + 3))
        (list
           ~force_newline:true
           ~label:"Hypotheses"
           ~indent:(indent + 3)
           (hyp ~force_newline:true ~indent:(indent + 3) env sigma)
           (Proofview.Goal.hyps x))
    in
    Printf.sprintf
      "%s%s"
      (str_tabs indent)
      (list
         ~force_newline:true
         ~indent:(indent + 2)
         ~use:("{", "}")
         (tuple ~is_keyval:true str str)
         [ "concl", concl_str; "hyps", hyps_str ])
  ;;
end
