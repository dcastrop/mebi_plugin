type t =
  { mebi_info : mebi_info list option
  ; rocq_info : rocq_info list option
  ; weak_info : string list option
  }

and mebi_info =
  { is_complete : bool
  ; is_merged : bool
  ; bound : int
  }

and rocq_info =
  { enc : Mebi_setup.Enc.t
  ; pp : string
  ; constructor_names : string list
  }

open Utils.Strfy

let mebi_info_to_string ?(args : style_args = style_args ()) (x : mebi_info)
  : string
  =
  let is_complete : string = bool ~args x.is_complete in
  let is_merged : string = bool ~args x.is_merged in
  let bound : string = int ~args x.bound in
  record
    ~args
    [ "is_complete", is_complete; "is_merged", is_merged; "bound", bound ]
;;

let mebi_info_list_option_to_string ?(args : style_args = style_args ())
  : mebi_info list option -> string
  = function
  | None -> "None"
  | Some alist -> list mebi_info_to_string alist
;;

let rocq_info_to_string ?(args : style_args = style_args ()) (x : rocq_info)
  : string
  =
  let enc : string = Mebi_setup.Enc.to_string x.enc in
  let pp : string = x.pp in
  let constructor_names : string = list string x.constructor_names in
  record ~args [ "enc", enc; "pp", pp; "constructor names", constructor_names ]
;;

let rocq_info_list_option_to_string ?(args : style_args = style_args ())
  : rocq_info list option -> string
  = function
  | None -> "None"
  | Some alist -> list rocq_info_to_string alist
;;

let to_string ?(args : style_args = record_args ()) (x : t) : string =
  let mebi_info : string = mebi_info_list_option_to_string x.mebi_info in
  let rocq_info : string = rocq_info_list_option_to_string x.rocq_info in
  let weak_info : string =
    Option.cata
      (list ~args:{ args with name = Some "Weak Info" } string)
      "None"
      x.weak_info
  in
  record
    ~args
    [ "mebi_info", mebi_info; "rocq_info", rocq_info; "weak_info", weak_info ]
;;
