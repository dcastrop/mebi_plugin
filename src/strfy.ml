open Utils
open Utils.Strfy

(**********************************)
(****** MODEL *********************)
(**********************************)

let ind_constr ?(indent : int = 0) env sigma ((ctx, tm) : Rocq_utils.ind_constr)
  : string
  =
  let ctx_str : string =
    tuple
      ~is_keyval:true
      ~indent
      str
      (Rocq_utils.Strfy.constr_rel_context env sigma)
      ("context", ctx)
  in
  let tm_str : string =
    tuple
      ~is_keyval:true
      ~indent
      str
      (Rocq_utils.Strfy.constr env sigma)
      ("term   ", tm)
  in
  list
    ~force_newline:true
    ~indent:(indent + 1)
    ~use:("{", "}")
    str
    [ ctx_str; tm_str ]
;;

let ind_constrs env sigma : Rocq_utils.ind_constrs -> string =
  array ~force_newline:true ~label:"constructors" (ind_constr env sigma)
;;

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
       , tuple
           ~is_keyval:true
           str
           (Rocq_utils.Strfy.econstr env sigma)
           ("econstr", snd x) ))
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
       , tuple
           ~is_keyval:true
           str
           (Rocq_utils.Strfy.econstr env sigma)
           ("econstr", snd x) ))
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
  fun x -> list ~label:"MetaData" Mebi_constr.Tree.pstr x
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

let constr_tree ?(indent : int = 0) : Mebi_constr.Tree.t -> string =
  let rec tree_to_string : Mebi_constr.Tree.t -> string = function
    | Node ((lts_enc, ctor_index), rhs_tree_list) ->
      Printf.sprintf
        "(%s:%i) %s"
        (enc lts_enc)
        ctor_index
        (list
           ~force_newline:true
           ~indent:(indent + 1)
           tree_to_string
           rhs_tree_list)
  in
  tree_to_string
;;

let lts_constr ?(indent : int = 0) env sigma : Mebi_constr.t -> string =
  fun ((action, destination, tree) : Mebi_constr.t) ->
  list
    ~force_newline:true
    ~indent
    str
    [ tuple
        ~is_keyval:true
        str
        (Rocq_utils.Strfy.econstr env sigma)
        ("action", action)
    ; tuple
        ~is_keyval:true
        str
        (Rocq_utils.Strfy.econstr env sigma)
        ("destination", destination)
    ; tuple ~is_keyval:true str (constr_tree ~indent:(indent + 1)) ("tree", tree)
    ]
;;
