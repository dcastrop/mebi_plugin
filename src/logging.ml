(** *)
type output_mode =
  | Coq of unit
  | OCaml of unit

let default_mode : output_mode = Coq ()

type output_kind =
  | Notice
  | Details
  | Debug
  | Warning

type output_options =
  { mutable output_enabled : bool
  ; mutable show_notice_enabled : bool
  ; mutable show_debug_enabled : bool
  ; mutable show_details_enabled : bool
  ; mutable show_warning_enabled : bool
  }

let default_output_enabled : bool = true
let default_show_notice_enabled : bool = true
let default_show_debug_enabled : bool = false
let default_show_details_enabled : bool = false
let default_show_warning_enabled : bool = true

let default_output_options : output_options =
  { output_enabled = default_output_enabled
  ; show_notice_enabled = default_show_notice_enabled
  ; show_debug_enabled = default_show_debug_enabled
  ; show_details_enabled = default_show_details_enabled
  ; show_warning_enabled = default_show_warning_enabled
  }
;;

type params =
  { mutable mode : output_mode
  ; options : output_options
  ; mutable override : bool
  }

let the_params : params ref =
  ref
    { mode = default_mode; options = default_output_options; override = false }
;;

let reset_output_enabled () : unit =
  !the_params.options.output_enabled <- default_output_enabled
;;

let reset_show_notice_enabled () : unit =
  !the_params.options.show_notice_enabled <- default_show_notice_enabled
;;

let reset_show_debug_enabled () : unit =
  !the_params.options.show_debug_enabled <- default_show_debug_enabled
;;

let reset_show_details_enabled () : unit =
  !the_params.options.show_details_enabled <- default_show_details_enabled
;;

let reset_show_warning_enabled () : unit =
  !the_params.options.show_warning_enabled <- default_show_warning_enabled
;;

let is_output_kind_enabled (kind : output_kind) : bool =
  !the_params.options.output_enabled
  &&
  match kind with
  | Notice -> !the_params.options.show_notice_enabled
  | Debug -> !the_params.options.show_debug_enabled
  | Details -> !the_params.options.show_details_enabled
  | Warning -> !the_params.options.show_warning_enabled
;;

(** only used when not in [Coq ()] mode *)
let message_prefix (kind : output_kind) : string =
  match kind with
  | Notice -> "( Notice ) "
  | Details -> "( Details ) "
  | Debug -> "( Debug ) "
  | Warning -> "( Warning ) "
;;

let log (kind : output_kind) (s : string) : unit =
  if is_output_kind_enabled kind
  then (
    match !the_params.mode with
    | Coq () ->
      let msg : string = Printf.sprintf "%s" s in
      (match kind with
       | Notice -> Feedback.msg_notice (Pp.str msg)
       | Details -> Feedback.msg_info (Pp.str msg)
       | Debug -> Feedback.msg_debug (Pp.str msg)
       | Warning -> Feedback.msg_warning (Pp.str msg))
    | OCaml () -> Printf.printf "%s%s\n" (message_prefix kind) s)
;;

module Log = struct
  let override (s : string) : unit =
    let stash_override = !the_params.override in
    !the_params.override <- true;
    log Notice s;
    !the_params.override <- stash_override
  ;;

  let notice (s : string) : unit = log Notice s
  let details (s : string) : unit = log Details s
  let debug (s : string) : unit = log Debug s
  let warning (s : string) : unit = log Warning s
end

let is_show_debug_enabled () : bool = !the_params.options.show_debug_enabled
let is_show_details_enabled () : bool = !the_params.options.show_details_enabled

(** *)
let get_output_mode () : unit =
  match !the_params.mode with
  | Coq () -> Log.override "Output mode is for Coq."
  | OCaml () -> Log.override "Output mode is for OCaml."
;;

(** *)
let set_output_mode (m : output_mode) : unit =
  !the_params.mode <- m;
  get_output_mode ()
;;

let set_show_debug (b : bool) : unit =
  !the_params.options.show_debug_enabled <- b
;;

let set_show_details (b : bool) : unit =
  !the_params.options.show_details_enabled <- b
;;

let enable_output () : unit = !the_params.options.output_enabled <- true
let disable_output () : unit = !the_params.options.output_enabled <- false
