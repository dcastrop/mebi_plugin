let is_unit_option (override : unit option) : bool =
  match override with
  | None -> false
  | Some () -> true
;;

(** used to store info on if a model is complete or the number of bounds required *)
type model_info =
  { is_complete : bool
  ; bound : int
  ; num_states : int
  ; num_edges : int
  }

let is_complete (m : model_info option) : bool option =
  match m with
  | None -> None
  | Some i -> Some i.is_complete
;;

module PStr = struct
  let model_info (m : model_info option) : string =
    match m with
    | None -> "None"
    | Some i ->
      Printf.sprintf
        "{| is complete: %b; bound: %i; num states: %i; num edges: %i |}"
        i.is_complete
        i.bound
        i.num_states
        i.num_edges
  ;;
end

module Logging = struct
  type output_modes =
    | Coq of unit
    | OCaml of unit

  type output_kind =
    | Normal of unit
    | Details of unit
    | Debug of unit
    | Warning of unit

  let pstr_output_kind_head (mode : output_modes) (kind : output_kind) : string =
    match mode with
    | Coq () ->
      (match kind with
       | Normal () -> ""
       | Details () -> "" (*"( Details )"*)
       | Debug () -> ""
       | Warning () -> "")
    | OCaml () ->
      (match kind with
       | Normal () -> ""
       | Details () -> "( Details )"
       | Debug () -> "( > Debug )"
       | Warning () -> "( Warning )")
  ;;

  type output_options =
    { mutable output_enabled : bool
    ; mutable show_normal_output : bool
    ; mutable show_detailed_output : bool
    ; mutable show_debug_output : bool
    ; mutable show_warning_output : bool
    }
  (*
     module DebugScope =Stack.Make (struct
     type t = string

     let equal (t1 : string) (t2 : string) = String.equal t1 t2
     end) *)

  (** [Logging.params]
      - [mode] determines if the print occurs via [Printf.printf] for [OCaml] or for [Coq], via [Feedback.msg_info] or similar.
      - [kind] specifies the nature of the message to be printed.
      - [options] specifies how different kinds of messages should be shown, in general.
      - [override] specifies that the output should be shown, regardless of [kind], ignoring [options]. *)
  type params =
    { mode : output_modes
    ; mutable kind : output_kind
    ; mutable options : output_options
    ; mutable scope : string Stack.t
    ; mutable override : unit option
    }

  (**  *)
  let default_params ?(mode : output_modes = OCaml ()) () : params =
    let kind : output_kind = Normal ()
    and options : output_options =
      { output_enabled = true
      ; show_normal_output = true
      ; show_detailed_output = true
      ; show_debug_output = false
      ; show_warning_output = true
      }
    and scope : string Stack.t = Stack.create ()
    and override : unit option = None in
    { mode; kind; options; scope; override }
  ;;

  let log_options (options : output_options) (params : params) : unit =
    params.options <- options
  ;;

  let log_kind (kind : output_kind) (params : params) : unit =
    params.kind <- kind
  ;;

  let override (params : params) : params =
    match params with
    | { mode; kind; options; scope; override } ->
      { mode; kind; options; scope; override = Some () }
  ;;

  let push_scope (to_push : string) (params : params) : unit =
    Stack.push to_push params.scope
  ;;

  let peek_scope (to_push : string) (params : params) : string =
    Stack.top params.scope
  ;;

  let pop_scope (to_push : string) (params : params) : string =
    Stack.pop params.scope
  ;;

  let pstr_scope (scope : string Stack.t) : string =
    Stack.fold
      (fun (acc : string) (level : string) -> Printf.sprintf "%s.%s" level acc)
      ""
      scope
  ;;

  let is_output_kind_enabled (params : params) : bool =
    match params with
    | { kind; options; override; _ } ->
      is_unit_option override
      ||
      if options.output_enabled
      then (
        match kind with
        | Normal () -> options.show_normal_output
        | Details () -> options.show_detailed_output
        | Debug () -> options.show_debug_output
        | Warning () -> options.show_warning_output)
      else false
  ;;

  let log ?(params : params = default_params ()) (to_log : string) : unit =
    match params with
    | { mode; kind; options; scope; override; _ } ->
      if is_output_kind_enabled params
      then (
        let msg_to_log : string =
          Printf.sprintf
            "%s%s %s\n"
            (if is_unit_option override then "!!!>>>" else "")
            (pstr_output_kind_head mode kind)
            to_log
        in
        match mode with
        | Coq () ->
          (match kind with
           | Normal () -> Feedback.msg_notice (Pp.str msg_to_log)
           | Details () -> Feedback.msg_info (Pp.str msg_to_log)
           | Debug () -> Feedback.msg_debug (Pp.str msg_to_log)
           | Warning () -> Feedback.msg_warning (Pp.str msg_to_log))
        | OCaml () ->
          Printf.printf
            "%s%s\n"
            (if Stack.is_empty scope
             then ""
             else Printf.sprintf "%s :: \n" (pstr_scope scope))
            msg_to_log)
  ;;

  module Log = struct
    let override ?(params : params = default_params ()) (to_log : string) : unit
      =
      let stashed_kind = params.kind in
      let stashed_override = params.override in
      log
        ~params:
          (params.kind <- Normal ();
           params.override <- Some ();
           params)
        to_log;
      params.override <- stashed_override;
      params.kind <- stashed_kind
    ;;

    let normal ?(params : params = default_params ()) (to_log : string) : unit =
      let stashed_kind = params.kind in
      log
        ~params:
          (params.kind <- Normal ();
           params)
        to_log;
      params.kind <- stashed_kind
    ;;

    let warning ?(params : params = default_params ()) (to_log : string) : unit =
      let stashed_kind = params.kind in
      let stashed_override = params.override in
      log
        ~params:
          (params.kind <- Warning ();
           params.override <- Some ();
           params)
        to_log;
      params.kind <- stashed_kind;
      params.override <- stashed_override
    ;;

    let debug ?(params : params = default_params ()) (to_log : string) : unit =
      let stashed_kind = params.kind in
      log
        ~params:
          (params.kind <- Debug ();
           params)
        to_log;
      params.kind <- stashed_kind
    ;;

    let details ?(params : params = default_params ()) (to_log : string) : unit =
      let stashed_kind = params.kind in
      log
        ~params:
          (params.kind <- Details ();
           params)
        to_log;
      params.kind <- stashed_kind
    ;;
  end
end

module Formatting = struct
  (* make new param that wraps around logging, stating the tab level and such *)
  type params =
    { tabs : int
    ; no_leading_tab : bool
    ; params : Logging.params
    }

  let default_params
    ?(params : Logging.params option)
    ?(mode : Logging.output_modes = OCaml ())
    ()
    : params
    =
    let params' =
      match params with
      | None -> Logging.default_params ~mode ()
      | Some params' -> params'
    in
    { tabs = 0; no_leading_tab = true; params = params' }
  ;;

  type pstr_params =
    | Log of Logging.params
    | Fmt of params

  let handle_params (params : pstr_params) : params =
    match params with
    | Fmt _params -> _params
    | Log _logging_params -> default_params ~params:_logging_params ()
  ;;
end

module Params = struct
  type fmt = Formatting.params
  type log = Logging.params
  type pstr = Formatting.pstr_params

  module Default = struct
    let fmt = Formatting.default_params
    let log = Logging.default_params
  end

  let handle = Formatting.handle_params
end

let inc_tab ?(by : int = 1) (params : Params.fmt) : Params.fmt =
  { tabs = params.tabs + by
  ; no_leading_tab =
      params.no_leading_tab (* ; non_repetative = params.non_repetative *)
  ; params = params.params
  }
;;

let dec_tab ?(by : int = 1) (params : Params.fmt) : Params.fmt =
  { tabs = (if params.tabs - by < 0 then 0 else params.tabs + by)
  ; no_leading_tab =
      params.no_leading_tab (* ; non_repetative = params.non_repetative *)
  ; params = params.params
  }
;;

let no_tab (params : Params.fmt) : Params.fmt =
  { tabs = 0
  ; no_leading_tab =
      params.no_leading_tab (* ; non_repetative = params.non_repetative *)
  ; params = params.params
  }
;;

let no_leading_tab (_no_leading_tab : bool) (params : Params.fmt) : Params.fmt =
  { tabs = params.tabs
  ; no_leading_tab =
      _no_leading_tab (* ; non_repetative = params.non_repetative *)
  ; params = params.params
  }
;;

(** [print ?show to_print] is a wrapper for [Printf.printf].
    @param ?show determines if [to_print] is outputted. *)
let print ?(show : bool = false) (to_print : string) : unit =
  match show with
  | true -> Printf.printf "%s" to_print
  | false -> ()
;;

(** [default_indent_val] is the default number of spaces to use perindent in [to_string]. *)
let default_indent_val = 2

(** [str_tabs ?size n] is [n] number of [?size]d spaces. *)
let rec str_tabs ?(size : int = default_indent_val) (n : int) : string =
  (*** [tab num] is [n'] number of spaces. *)
  let rec tab (n' : int) : string =
    if n' > 0 then Printf.sprintf " %s" (tab (n' - 1)) else ""
  in
  if n > 0
  then Printf.sprintf "%s%s" (tab size) (str_tabs ~size (n - 1))
  else ""
;;

(** [get_key_of_val tbl v] is a reverse-lookup in [tbl] for the key of value [v]. *)
let get_key_of_val (tbl : ('a, 'b) Hashtbl.t) (v : 'b) : 'a option =
  match
    List.find_opt
      (fun ((_key, value) : 'a * 'b) -> v == value)
      (List.of_seq (Hashtbl.to_seq tbl))
  with
  | None -> None
  | Some (key, _value) -> Some key
;;

(** [new_int_counter] returns a function that when called, will return the value of a counter and then increment it by 1, starting from 0. *)
let new_int_counter ?(start : int = 0) () : unit -> int =
  let id_counter : int ref = ref start in
  let get_and_incr_counter () : int =
    let to_return = !id_counter in
    id_counter := to_return + 1;
    to_return
  in
  get_and_incr_counter
;;
