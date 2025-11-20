type t =
  { is_complete : bool
  ; is_merged : bool
  ; bound : int
  ; num_terminals : int
  ; num_labels : int
  ; num_states : int
  ; num_edges : int
  ; rocq_info : rocq_info list option
  ; weak_info : string list option
  }

and rocq_info =
  { enc : Mebi_setup.Enc.t
  ; pp : string
  ; constructor_names : string list
  }

let rec to_string
          ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
          (x : t)
  : string
  =
  let open Utils.Strfy in
  let is_complete : string = bool ~args x.is_complete in
  let is_merged : string = bool ~args x.is_merged in
  let bound : string = int ~args x.bound in
  let num_terminals : string = int ~args x.num_terminals in
  let num_labels : string = int ~args x.num_labels in
  let num_states : string = int ~args x.num_states in
  let num_edges : string = int ~args x.num_edges in
  let rocq_info : string = "" in
  let weak_info : string =
    Option.cata
      (list ~args:{ args with name = Some "Weak Info" } string)
      "None"
      x.weak_info
  in
  record
    ~args
    [ "is_complete", is_complete
    ; "is_merged", is_merged
    ; "bound", bound
    ; "num_terminals", num_terminals
    ; "num_labels", num_labels
    ; "num_states", num_states
    ; "num_edges", num_edges
    ; "rocq_info", rocq_info
    ; "weak_info", weak_info
    ]

and rocq_info_list_option_to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
  : rocq_info list option -> string
  = function
  | None -> "None"
  | Some alist ->
    let open Utils.Strfy in
    list rocq_info_to_string alist

and rocq_info_to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      (x : rocq_info)
  : string
  =
  let enc : string = Mebi_setup.Enc.to_string x.enc in
  let pp : string = x.pp in
  let open Utils.Strfy in
  let constructor_names : string = list string x.constructor_names in
  record ~args [ "enc", enc; "pp", pp; "constructor names", constructor_names ]
;;
