type t =
  { mebi_info : mebi_info list option
  ; rocq_info : rocq_info list option
  ; weak_info : string list option
  }

and mebi_info =
  { is_complete : bool
  ; is_merged : bool
  ; bounds : bound_config
  }

and bound_config =
  { bound : int
  ; bound_for : boundable
  }

and boundable =
  | States
  | Transitions

and rocq_info =
  { enc : Mebi_setup.Enc.t
  ; pp : string (* ; constructor_names : string list *)
  ; constructors : rocq_constructor list
  }

and rocq_constructor =
  { index : int
  ; name : string
  ; bindings : Rocq_bindings.t
  }

(* and rocq_constructor_bindings =
  | No_Bindings
  | Use_Bindings of
      (binding_cmaps * (binding_args -> EConstr.t Tactypes.explicit_bindings))

and binding_cmaps =
  { cfrom : constructor_map option
  ; clabel : constructor_map option
  ; cgoto : constructor_map option
  }

and constructor_map = binding_extractor Rocq_utils.C.t

and binding_args =
  { from : Model_state.t
  ; label : Model_label.t
  ; goto : Model_state.t
  }

and binding_extractor = Names.Name.t * binding_instructions

and binding_instructions =
  | Undefined
  | Done
  | Arg of
      { root : Constr.t
      ; index : int
      ; cont : binding_instructions
      } *)

open Utils.Strfy

let mebi_info_to_string ?(args : style_args = style_args ()) (x : mebi_info)
  : string
  =
  let is_complete : string = bool ~args x.is_complete in
  let is_merged : string = bool ~args x.is_merged in
  let bound : string = int ~args x.bounds.bound in
  let bound_for : string =
    string
      ~args
      (match x.bounds.bound_for with
       | States -> "States"
       | Transitions -> "Transitions")
  in
  record
    ~args
    [ "is_complete", is_complete
    ; "is_merged", is_merged
    ; "bound", bound
    ; "bound_for", bound_for
    ]
;;

let mebi_info_list_option_to_string ?(args : style_args = style_args ())
  : mebi_info list option -> string
  = function
  | None -> "None"
  | Some alist -> list (Args mebi_info_to_string) alist
;;

let rocq_constructor_to_string
      ?(args : style_args = style_args ())
      ?(envsigma : (Environ.env * Evd.evar_map) option = None)
      (x : rocq_constructor)
  : string
  =
  let index : string = int x.index in
  let name : string = x.name in
  let bindings : string = Rocq_bindings.to_string ~envsigma x.bindings in
  record ~args [ "index", index; "name", name; "bindings", bindings ]
;;

let rocq_info_to_string
      ?(args : style_args = style_args ())
      ?(envsigma : (Environ.env * Evd.evar_map) option = None)
      (x : rocq_info)
  : string
  =
  let enc : string = Mebi_setup.Enc.to_string x.enc in
  let pp : string = x.pp in
  let constructor_names : string =
    list (Args (rocq_constructor_to_string ~envsigma)) x.constructors
  in
  record ~args [ "enc", enc; "pp", pp; "constructor names", constructor_names ]
;;

let rocq_info_list_option_to_string
      ?(args : style_args = style_args ())
      ?(envsigma : (Environ.env * Evd.evar_map) option = None)
  : rocq_info list option -> string
  = function
  | None -> "None"
  | Some alist -> list (Args (rocq_info_to_string ~envsigma)) alist
;;

let to_string ?(args : style_args = record_args ()) (x : t) : string =
  let mebi_info : string = mebi_info_list_option_to_string x.mebi_info in
  let rocq_info : string = rocq_info_list_option_to_string x.rocq_info in
  let weak_info : string =
    Option.cata
      (list ~args:{ args with name = Some "Weak Info" } (Args string))
      "None"
      x.weak_info
  in
  record
    ~args
    [ "mebi_info", mebi_info; "rocq_info", rocq_info; "weak_info", weak_info ]
;;
