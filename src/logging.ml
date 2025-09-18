(** *)
type output_modes =
  | Coq of unit
  | OCaml of unit

let default_mode : output_modes = Coq ()
let the_output_mode : output_modes ref = ref default_mode

(** *)
let show_debug_messages : bool ref = ref true

let show_detailed_messages : bool ref = ref true

type output_kind =
  | Notice of unit
  | Details of unit
  | Debug of unit
  | Warning of unit

type output_options =
  { mutable output_enabled : bool
  ; mutable show_notice_enabled : bool
  ; mutable show_debug_enabled : bool
  ; mutable show_detailed_enabled : bool
  ; mutable show_warning_enabled : bool
  }

let default_output_options : output_options =
  { output_enabled = true
  ; show_notice_enabled = true
  ; show_debug_enabled = true
  ; show_detailed_enabled = true
  ; show_warning_enabled = true
  }
;;

let the_output_options : output_options ref = ref default_output_options

type params =
  { mutable options : output_options
  ; mutable override : bool
  }

let the_params : params ref =
  ref { options = !the_output_options; override = true }
;;

let is_output_kind_enabled (kind : output_kind) : bool =
  !the_params.options.output_enabled
  &&
  match kind with
  | Notice () -> !the_params.options.show_notice_enabled
  | Debug () -> !the_params.options.show_debug_enabled
  | Details () -> !the_params.options.show_detailed_enabled
  | Warning () -> !the_params.options.show_warning_enabled
;;

(** only used when not in [Coq ()] mode *)
let message_prefix (kind : output_kind) : string =
  match kind with
  | Notice () -> "( Notice ) "
  | Details () -> "( Details ) "
  | Debug () -> "( Debug ) "
  | Warning () -> "( Warning ) "
;;

let log (kind : output_kind) (s : string) : unit =
  if is_output_kind_enabled kind
  then (
    match !the_output_mode with
    | Coq () ->
      let msg : string = Printf.sprintf "%s" s in
      (match kind with
       | Notice () -> Feedback.msg_notice (Pp.str msg)
       | Details () -> Feedback.msg_info (Pp.str msg)
       | Debug () -> Feedback.msg_debug (Pp.str msg)
       | Warning () -> Feedback.msg_warning (Pp.str msg))
    | OCaml () -> Printf.printf "%s%s\n" (message_prefix kind) s)
;;

module Log = struct
  let override (s : string) : unit =
    let stash_override = !the_params.override in
    !the_params.override <- true;
    log (Notice ()) s;
    !the_params.override <- stash_override
  ;;

  let notice (s : string) : unit = log (Notice ()) s
  let details (s : string) : unit = log (Details ()) s
  let debug (s : string) : unit = log (Debug ()) s
  let warning (s : string) : unit = log (Warning ()) s
end

(** *)
let get_output_mode : unit =
  match !the_output_mode with
  | Coq () -> Log.override "Output mode is for Coq."
  | OCaml () -> Log.override "Output mode is for OCaml."
;;

let get_show_debug_messages : unit =
  Log.override
    (if !show_debug_messages
     then "Debug messages will be shown."
     else "Debug messages set to be hidden.")
;;

let get_show_detailed_messages : unit =
  Log.override
    (if !show_detailed_messages
     then "Detailed messages will be shown. (where possible)"
     else "Detailed messages set to be hidden.")
;;

(** *)
let set_output_mode (m : output_modes) : unit =
  the_output_mode := m;
  get_output_mode
;;

let set_show_debug_messages (b : bool) : unit =
  show_debug_messages := b;
  get_show_debug_messages
;;

let set_show_detailed_messages (b : bool) : unit =
  show_detailed_messages := b;
  get_show_detailed_messages
;;
