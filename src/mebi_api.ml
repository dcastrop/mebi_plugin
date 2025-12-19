(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

type bound_config = Model_info.bound_config
and boundable = Model_info.boundable

let default_bound_config () : bound_config = { bound = 50; bound_for = States }

(***********************************************************************)

type weak_config =
  { enabled : bool
  ; args : weak_args option
  ; encs : weak_encs option
  }

and weak_args =
  { arg1 : weak_arg option
  ; arg2 : weak_arg option
  }

and weak_arg =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

and weak_encs =
  { enc1 : weak_enc option
  ; enc2 : weak_enc option
  }

and weak_enc = Mebi_weak.t

let default_weak_config () : weak_config =
  { enabled = false; args = None; encs = None }
;;

(***********************************************************************)

type fail_config =
  { incomplete : bool
  ; not_bisim : bool
  }

let default_fail_config () : fail_config =
  { incomplete = true; not_bisim = true }
;;

(***********************************************************************)

type debug_config =
  { dump_to_json : bool
  ; debug : bool
  }

let default_debug_config () : debug_config =
  { dump_to_json = false; debug = false }
;;

(***********************************************************************)

type t =
  { bounds : bound_config
  ; weak_mode : weak_config
  ; fail_if : fail_config
  ; debug : debug_config
  ; output : Output_config.t
  }

let default_config () : t =
  { bounds = default_bound_config ()
  ; weak_mode = default_weak_config ()
  ; fail_if = default_fail_config ()
  ; debug = default_debug_config ()
  ; output = Output_config.default ()
  }
;;

(***********************************************************************)

let the_config : t ref = ref (default_config ())
let reset_config () : unit = the_config := default_config ()

let reset_bounds () : unit =
  the_config := { !the_config with bounds = default_bound_config () }
;;

let reset_weak_mode () : unit =
  the_config := { !the_config with weak_mode = default_weak_config () }
;;

let reset_fail_if () : unit =
  the_config := { !the_config with fail_if = default_fail_config () }
;;

let reset_debug () : unit =
  the_config := { !the_config with debug = default_debug_config () }
;;

let reset_output () : unit =
  the_config := { !the_config with output = Output_config.default () }
;;

(***********************************************************************)

let set_bound (bound : int) : unit =
  the_config := { !the_config with bounds = { !the_config.bounds with bound } }
;;

let set_bound_for (bound_for : boundable) : unit =
  the_config
  := { !the_config with bounds = { !the_config.bounds with bound_for } }
;;

let set_bounds (bound : int) (bound_for : boundable) : unit =
  the_config := { !the_config with bounds = { bound; bound_for } }
;;

let get_bounds () : bound_config = !the_config.bounds

(***********************************************************************)

let set_weak_mode_enabled (enabled : bool) : unit =
  the_config
  := { !the_config with weak_mode = { !the_config.weak_mode with enabled } }
;;

let set_weak_args (args : weak_args option) : unit =
  the_config
  := { !the_config with weak_mode = { !the_config.weak_mode with args } }
;;

let set_weak_arg1 (arg1 : weak_arg option) : unit =
  the_config
  := { !the_config with
       weak_mode =
         { !the_config.weak_mode with
           args =
             (match !the_config.weak_mode.args with
              | None -> Some { arg1; arg2 = None }
              | Some { arg2; _ } -> Some { arg1; arg2 })
         }
     }
;;

let set_weak_arg2 (arg2 : weak_arg option) : unit =
  the_config
  := { !the_config with
       weak_mode =
         { !the_config.weak_mode with
           args =
             (match !the_config.weak_mode.args with
              | None -> Some { arg1 = None; arg2 }
              | Some { arg1; _ } -> Some { arg1; arg2 })
         }
     }
;;

let get_weak_arg1 () : weak_arg option =
  match !the_config.weak_mode.args with None -> None | Some args -> args.arg1
;;

let get_weak_arg2 () : weak_arg option =
  match !the_config.weak_mode.args with None -> None | Some args -> args.arg2
;;

let set_weak_encs (encs : weak_encs option) : unit =
  the_config
  := { !the_config with weak_mode = { !the_config.weak_mode with encs } }
;;

let get_weak_enc1 () : weak_enc option =
  match !the_config.weak_mode.encs with None -> None | Some encs -> encs.enc1
;;

let get_weak_enc2 () : weak_enc option =
  match !the_config.weak_mode.encs with
  | None -> None
  | Some encs ->
    (match encs.enc2 with None -> encs.enc1 | Some enc2 -> Some enc2)
;;

let set_weak_mode
      (enabled : bool)
      (args : weak_args option)
      (encs : weak_encs option)
  : unit
  =
  the_config := { !the_config with weak_mode = { enabled; args; encs } }
;;

let is_in_weak_mode () : bool = !the_config.weak_mode.enabled

(** NOTE: we will never just have [arg2], so suffice to just check [arg1] *)
let has_weak_args () : bool =
  match !the_config.weak_mode.args with Some { arg1 = Some _; _ } | _ -> false
;;

let load_weak_arg : weak_arg -> Mebi_weak.t Mebi_wrapper.mm =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  function
  | Option label_tref ->
    Log.trace ~__FUNCTION__ "Option label_tref";
    let* label_enc : Enc.t = Mebi_utils.encode_constrexpr label_tref in
    let* _ = decode label_enc in
    Log.thing ~__FUNCTION__ Debug "label enc" label_enc (Of Enc.to_string);
    return (Mebi_weak.Option label_enc)
  | Custom (tau_tref, label_ref) ->
    Log.trace ~__FUNCTION__ "Custom (tau_tref, label_ref)";
    let* tau_enc : Enc.t = Mebi_utils.encode_constrexpr tau_tref in
    let* label_enc : Enc.t = Mebi_utils.encode_ref label_ref in
    Log.thing ~__FUNCTION__ Debug "tau enc" tau_enc (Of Enc.to_string);
    Log.thing ~__FUNCTION__ Debug "label enc" label_enc (Of Enc.to_string);
    let* _ = decode tau_enc in
    let* _ = decode label_enc in
    return (Mebi_weak.Custom (tau_enc, label_enc))
;;

let load_weak_arg_opt : weak_arg option -> Mebi_weak.t option Mebi_wrapper.mm =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  function
  | None -> return None
  | Some x ->
    let* x = load_weak_arg x in
    return (Some x)
;;

let load_weak_encs () : unit Mebi_wrapper.mm =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  let* enc1 : weak_enc option = load_weak_arg_opt (get_weak_arg1 ()) in
  let* enc2 : weak_enc option = load_weak_arg_opt (get_weak_arg2 ()) in
  set_weak_encs (Some { enc1; enc2 });
  return ()
;;

let load_weak_args () : unit Mebi_wrapper.mm =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper in
  if is_in_weak_mode ()
  then load_weak_encs ()
  else (
    Log.trace ~__FUNCTION__ "Not in weak mode";
    if has_weak_args ()
    then (
      Log.warning "(Not in weak mode -- clearing weak config)";
      reset_weak_mode ());
    return ())
;;

(***********************************************************************)

let set_fail_ifincomplete (incomplete : bool) : unit =
  the_config
  := { !the_config with fail_if = { !the_config.fail_if with incomplete } }
;;

let set_fail_ifnotbisim (not_bisim : bool) : unit =
  the_config
  := { !the_config with fail_if = { !the_config.fail_if with not_bisim } }
;;

let set_fail_if (incomplete : bool) (not_bisim : bool) : unit =
  the_config := { !the_config with fail_if = { incomplete; not_bisim } }
;;

let fail_if_incomplete () : bool = !the_config.fail_if.incomplete
let fail_if_not_bisim () : bool = !the_config.fail_if.not_bisim

(***********************************************************************)

let set_dump_to_json (dump_to_json : bool) : unit =
  the_config
  := { !the_config with debug = { !the_config.debug with dump_to_json } }
;;

let set_debug (debug : bool) : unit =
  the_config := { !the_config with debug = { !the_config.debug with debug } }
;;

let set_debug_config (dump_to_json : bool) (debug : bool) : unit =
  the_config := { !the_config with debug = { dump_to_json; debug } }
;;

(***********************************************************************)

let set_output_enabled (enabled : bool) : unit =
  the_config
  := { !the_config with output = { !the_config.output with enabled } }
;;

let set_output_level (x : Output_kind.level) (y : bool) : unit =
  let f = !Output_kind.default_level in
  let debug, info, notice, warning, error =
    f Debug, f Info, f Notice, f Warning, f Error
  in
  let debug, info, notice, warning, error =
    match x with
    | Debug -> y, info, notice, warning, error
    | Info -> debug, y, notice, warning, error
    | Notice -> debug, info, y, warning, error
    | Warning -> debug, info, notice, y, error
    | Error -> debug, info, notice, warning, y
  in
  let new_f
        ?(debug : bool = debug)
        ?(info : bool = info)
        ?(notice : bool = notice)
        ?(warning : bool = warning)
        ?(error : bool = error)
    : Output_kind.level -> bool
    = function
    | Debug -> debug
    | Info -> info
    | Notice -> notice
    | Warning -> warning
    | Error -> error
  in
  Output_kind.default_level := new_f
;;

let set_output_special (x : Output_kind.special) (y : bool) : unit =
  let f = !Output_kind.default_special in
  let trace, result, show = f Trace, f Result, f Show in
  let trace, result, show =
    match x with
    | Trace -> y, result, show
    | Result -> trace, y, show
    | Show -> trace, result, y
  in
  let new_f
        ?(trace : bool = trace)
        ?(result : bool = result)
        ?(show : bool = show)
    : Output_kind.special -> bool
    = function
    | Trace -> trace
    | Result -> result
    | Show -> show
  in
  Output_kind.default_special := new_f
;;

let set_output_config
      (enabled : bool)
      ((a, b) : Output_kind.level * bool)
      ((x, y) : Output_kind.special * bool)
  : unit
  =
  set_output_enabled enabled;
  set_output_level a b;
  set_output_special x y
;;
