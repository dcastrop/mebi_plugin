let str_tabs = Utils.str_tabs

(* new line sep *)
let nlsep ?(force_newline : bool = false) ?(indent : int = 0) () : string =
  if force_newline then Printf.sprintf "\n%s" (str_tabs indent) else ""
;;

(**********************************)
(****** OCAML *********************)
(**********************************)

let list
      ?(force_newline : bool = false)
      ?(label : string = "List")
      ?(indent : int = 0)
      ?(use : string * string = "[", "]")
      (strfy : 'a -> string)
  : 'a list -> string
  = function
  | [] ->
    let lhs, rhs = use in
    Printf.sprintf "%s %s (%s empty)" lhs rhs label
  | h :: [] ->
    let lhs, rhs = use in
    let sep = nlsep ~force_newline ~indent () in
    let suffix =
      if use = ("[", "]") then Printf.sprintf "(%s: 1)" label else ""
    in
    Printf.sprintf "%s %s %s%s %s" lhs (strfy h) sep rhs suffix
  | h :: t ->
    let lhs, rhs = use in
    let sep = nlsep ~force_newline ~indent () in
    let len = List.length t + 1 in
    let suffix =
      if use = ("[", "]") then Printf.sprintf "(%s: %i)" label len else ""
    in
    let lstr =
      List.fold_left
        (fun (acc : string) (e : 'a) ->
          Printf.sprintf "%s%s; %s" acc sep (strfy e))
        (strfy h)
        t
    in
    Printf.sprintf "%s %s %s%s %s" lhs lstr sep rhs suffix
;;

let str : string -> string = fun (x : string) -> x
let int : int -> string = fun (x : int) -> Printf.sprintf "%i" x
let bool : bool -> string = fun (x : bool) -> Printf.sprintf "%b" x

let option (f : 'a -> string) : 'a option -> string = function
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" (f x)
;;

let tuple
      ?(force_newline : bool = false)
      ?(is_keyval : bool = false)
      ?(indent : int = 0)
      (f : 'a -> string)
      (g : 'b -> string)
  : 'a * 'b -> string
  =
  fun ((a, b) : 'a * 'b) ->
  let sep, con, lhs, rhs =
    match force_newline, is_keyval with
    | true, false -> nlsep ~force_newline:true ~indent (), ",", "( ", ")"
    | _, false -> nlsep ~force_newline ~indent (), ",", "( ", " )"
    | _, true -> nlsep ~force_newline ~indent (), ":", "", ""
  in
  Printf.sprintf "%s%s%s%s %s%s%s" lhs (f a) sep con (g b) sep rhs
;;

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
  if EConstr.ERelevance.is_irrelevant sigma x then "irrelevant" else "relevant"
;;

type hyp =
  ( EConstr.constr
    , EConstr.types
    , EConstr.ERelevance.t )
    Context.Named.Declaration.pt

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

(**********************************)
(****** MODEL *********************)
(**********************************)

let enc : Mebi_setup.Enc.t -> string = Mebi_setup.Enc.to_string

let enc_econstr_pair ?(indent : int = 0) env sigma
  : Mebi_setup.Enc.t * EConstr.t -> string
  =
  fun (x : Mebi_setup.Enc.t * EConstr.t) ->
  Printf.sprintf
    "%s%s"
    (str_tabs indent)
    (tuple
       ~force_newline:true
       ~indent
       str
       str
       ( tuple ~is_keyval:true str enc ("encoding", fst x)
       , tuple ~is_keyval:true str (econstr env sigma) ("econstr", snd x) ))
;;

let enc_econstr_opt_pair ?(indent : int = 0) env sigma
  : Mebi_setup.Enc.t option * EConstr.t -> string
  =
  fun (x : Mebi_setup.Enc.t option * EConstr.t) ->
  Printf.sprintf
    "%s%s"
    (str_tabs indent)
    (tuple
       ~indent
       str
       str
       ( tuple ~is_keyval:true str (option enc) ("encoding", fst x)
       , tuple ~is_keyval:true str (econstr env sigma) ("econstr", snd x) ))
;;

let coq_info ?(indent : int = 0) : Model.Info.Coq.t -> string =
  fun (enc, (dec, names)) ->
  let sep = nlsep ~force_newline:(Bool.not (List.is_empty names)) ~indent () in
  Printf.sprintf
    "(%s => %s%s, [%s])"
    (Mebi_setup.Enc.to_string enc)
    dec
    sep
    (list ~indent:(indent + 1) str names)
;;

let info ?(indent : int = 0) : Model.Info.t -> string = fun x -> ""

let state ?(indent : int = 0) : Model.State.t -> string =
  fun x ->
  Printf.sprintf
    "%s%s"
    (str_tabs indent)
    (tuple
       ~indent
       str
       str
       ( tuple ~is_keyval:true ~indent str enc ("encState", fst x)
       , tuple ~is_keyval:true ~indent str (option str) ("decState", snd x) ))
;;

let states ?(indent : int = 0) : Model.States.t -> string =
  fun x ->
  list
    ~force_newline:true
    ~indent
    ~label:"States"
    state
    (Model.States.to_list x)
;;

let action_label ?(indent : int = 0) : Model.Action.Label.t -> string =
  fun x -> enc (fst x)
;;

let action_metadata ?(indent : int = 0) : Model.Action.MetaData.t -> string =
  fun x -> list ~label:"MetaData" str x
;;

let rec action_annotation_pair ?(indent : int = 0)
  : Model.Action.annotation_pair -> string
  =
  fun x -> tuple ~indent state action x

and action_annotation ?(indent : int = 0) : Model.Action.annotation -> string =
  fun x -> list action_annotation_pair x

and action_annotations ?(indent : int = 0) : Model.Action.annotations -> string =
  fun x -> list ~label:"Annotations" action_annotation x

and action ?(indent : int = 0) : Model.Action.t -> string =
  fun (x : Model.Action.t) ->
  Printf.sprintf
    "%s%s"
    (str_tabs indent)
    (list
       ~force_newline:true
       ~indent:(indent + 1)
       ~use:("{", "}")
       str
       [ tuple
           ~is_keyval:true
           ~indent:(indent + 1)
           str
           action_label
           ("encLabel", x.label)
       ; tuple
           ~is_keyval:true
           ~indent:(indent + 1)
           str
           (option str)
           ("decLabel", fst (snd x.label))
       ; tuple
           ~is_keyval:true
           ~indent:(indent + 1)
           str
           (option bool)
           ("silent", snd (snd x.label))
       ; tuple
           ~is_keyval:true
           ~indent:(indent + 1)
           str
           action_metadata
           ("metaData", x.meta)
       ; tuple
           ~is_keyval:true
           ~indent:(indent + 1)
           str
           str
           ("annotations", "TODO")
         (* action_annotations
            ("annotations", x.annos) *)
       ])
;;
(* Mebi_setup.Enc.to_string (fst x) *)
(* Printf.sprintf
    "( encState: %s%s, decState: %s )"
    (Mebi_setup.Enc.to_string x.)
    sep
    (option str (snd x)) *)
