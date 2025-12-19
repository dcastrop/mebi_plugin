(* open Logging *)

(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

(**********************)
(** Bounds ************)
(**********************)

(** [bound] is the total number of states to be allowed when building an LTS. *)
let default_bound : int = 10

(** If snd is none, then fst is used for both. *)
let default_bounds : int * int option = default_bound, None

let the_bounds : (int * int option) ref = ref default_bounds
let reset_bounds () : unit = the_bounds := default_bounds
let fst_bound () : int = fst !the_bounds
let snd_bound () : int = match !the_bounds with b, None -> b | _, Some a -> a

let printout_bounds_str () : string =
  match !the_bounds with
  | b, None ->
    Printf.sprintf
      "Bound set to: %i. (%sno secondary bound established)"
      b
      (if b = default_bound then "default, " else "")
  | b, Some a ->
    Printf.sprintf
      "Bound for first term set to: %i%s.\nBound for second term set to: %i%s."
      b
      (if b = default_bound then " (default)" else "")
      a
      (if a = default_bound then " (default)" else "")
;;

let printout_bounds () : unit = Log.notice (printout_bounds_str ())

let set_bounds (b : int * int option) : unit =
  Log.debug "params.set_bounds";
  the_bounds := b;
  Log.debug (printout_bounds_str ())
;;

(**********************)
(** Fail if incomplete*)
(**********************)

let default_fail_if_incomplete : bool = true
let the_fail_if_incomplete : bool ref = ref default_fail_if_incomplete

let reset_fail_if_incomplete () : unit =
  the_fail_if_incomplete := default_fail_if_incomplete
;;

let printout_fail_if_incomplete_str () : string =
  if !the_fail_if_incomplete
  then "Fail if incomplete."
  else "Will not fail if incomplete."
;;

let printout_fail_if_incomplete () : unit =
  Log.notice (printout_fail_if_incomplete_str ())
;;

let set_fail_if_incomplete (b : bool) : unit =
  Log.debug "params.set_fail_if_incomplete";
  the_fail_if_incomplete := b;
  Log.debug (printout_fail_if_incomplete_str ())
;;

(**********************)
(** Fail if not bisim *)
(**********************)

let default_fail_if_not_bisim : bool = true
let the_fail_if_not_bisim : bool ref = ref default_fail_if_not_bisim

let reset_fail_if_not_bisim () : unit =
  the_fail_if_not_bisim := default_fail_if_not_bisim
;;

let printout_fail_if_not_bisim_str () : string =
  if !the_fail_if_not_bisim
  then "Fail if terms not Bisimilar."
  else "Will not fail if terms are not Bisimilar."
;;

let printout_fail_if_not_bisim () : unit =
  Log.notice (printout_fail_if_not_bisim_str ())
;;

let set_fail_if_not_bisim (b : bool) : unit =
  Log.debug "params.set_fail_if_not_bisim";
  the_fail_if_not_bisim := b;
  printout_fail_if_not_bisim ()
;;

(**********************)
(** File Dump *********)
(**********************)

let default_dump_to_file : bool = true
let the_dump_to_file : bool ref = ref default_dump_to_file
let reset_dump_to_file () : unit = the_dump_to_file := default_dump_to_file

let printout_dump_to_file_str () : string =
  if !the_dump_to_file then "File dumps enabled." else "File dumps disabled."
;;

let printout_dump_to_file () : unit = Log.notice (printout_dump_to_file_str ())

let set_dump_to_file (b : bool) : unit =
  Log.debug "params.set_dump_to_file";
  the_dump_to_file := b;
  printout_dump_to_file ()
;;

(**********************)
(** Messages **********)
(**********************)

(* *)
let reset_show_any () : unit = Logging.reset_output_enabled ()

let printout_show_any_str () : string =
  if is_output_enabled ()
  then "Notice messages will be shown."
  else "Notice messages set to be hidden."
;;

let printout_show_any () : unit = Log.override (printout_show_any_str ())

let set_show_any (b : bool) : unit =
  Log.debug "params.set_show_any";
  Logging.set_output_enabled b;
  printout_show_any ()
;;

(* *)
let reset_show_notice () : unit = Logging.reset_show_notice_enabled ()

let printout_show_notice_str () : string =
  if is_notice_enabled ()
  then "Notice messages will be shown."
  else "Notice messages set to be hidden."
;;

let printout_show_notice () : unit = Log.notice (printout_show_notice_str ())

let set_show_notice (b : bool) : unit =
  Log.debug "params.set_show_notice";
  Logging.set_show_notice b;
  printout_show_notice ()
;;

(* *)
let reset_show_debug () : unit = Logging.reset_show_debug_enabled ()

let printout_show_debug_str () : string =
  if is_debug_enabled ()
  then "Debug messages will be shown."
  else "Debug messages set to be hidden."
;;

let printout_show_debug () : unit = Log.notice (printout_show_debug_str ())

let set_show_debug (b : bool) : unit =
  Log.debug "params.set_show_debug";
  Logging.set_show_debug b;
  printout_show_debug ()
;;

(* *)
let reset_show_details () : unit = Logging.reset_show_details_enabled ()

let printout_show_details_str () : string =
  if is_details_enabled ()
  then "Detailed messages will be shown. (where possible)"
  else "Detailed messages set to be hidden."
;;

let printout_show_details () : unit = Log.notice (printout_show_details_str ())

let set_show_details (b : bool) : unit =
  Log.debug "params.set_show_details";
  Logging.set_show_details b;
  printout_show_details ()
;;

(* *)
let reset_show_result () : unit = Logging.reset_show_result_enabled ()

let printout_show_result_str () : string =
  if is_result_enabled ()
  then "Result messages will be shown."
  else "Result messages set to be hidden."
;;

let printout_show_result () : unit = Log.notice (printout_show_result_str ())

let set_show_result (b : bool) : unit =
  Log.debug "params.set_show_result";
  Logging.set_show_result b;
  printout_show_result ()
;;

(* *)
let reset_show_warning () : unit = Logging.reset_show_warning_enabled ()

let printout_show_warning_str () : string =
  if is_warning_enabled ()
  then "Result messages will be shown."
  else "Result messages set to be hidden."
;;

let printout_show_warning () : unit = Log.notice (printout_show_warning_str ())

let set_show_warning (b : bool) : unit =
  Log.debug "params.set_show_warning";
  Logging.set_show_warning b;
  printout_show_warning ()
;;

(**********************)
(** Weak Mode *********)
(**********************)

let default_weak_mode : bool = false
let the_weak_mode : bool ref = ref default_weak_mode
let reset_weak_mode () : unit = the_weak_mode := default_weak_mode

let printout_weak_mode_str () : string =
  Printf.sprintf
    "Currently in %s mode."
    (if !the_weak_mode then "weak" else "strong")
;;

let printout_weak_mode () : unit = Log.notice (printout_weak_mode_str ())

let set_weak_mode (b : bool) : unit =
  Log.debug "params.set_weak_mode";
  the_weak_mode := b;
  printout_weak_mode ()
;;

(**********************)
(** Weak Type Args ****)
(**********************)

module WeakArgs = struct
  type t =
    | OptionConstr of Constrexpr.constr_expr
    | CustomConstr of Constrexpr.constr_expr * Libnames.qualid
end

(** If snd is none, then fst is used for both. *)
let default_weak_type_args : WeakArgs.t option * WeakArgs.t option = None, None

let the_weak_type_args : (WeakArgs.t option * WeakArgs.t option) ref =
  ref default_weak_type_args
;;

let reset_weak_type_args () : unit =
  the_weak_type_args := default_weak_type_args
;;

(**********************)
(** Weak Type Kind ****)
(**********************)

(* type weak_type_kind =
   | Kind_Option
   | Kind_Custom

   let weak_type_kind_default : weak_type_kind option = None
   let weak_type_kind : weak_type_kind option ref = ref weak_type_kind_default
   let reset_weak_type_kind () : unit = weak_type_kind := weak_type_kind_default

   let is_weak_type_kind_option () : bool =
   match !weak_type_kind with
   | None -> false
   | Some w -> (match w with Kind_Option -> true | _ -> false)
   ;;

   let is_weak_type_kind_custom () : bool =
   match !weak_type_kind with
   | None -> false
   | Some w -> (match w with Kind_Custom -> true | _ -> false)
   ;; *)

(* must be re-obtained for each mebi_wrapper.run (...) *)
module WeakEnc = struct
  type t =
    | OptionConstr of Mebi_wrapper.Enc.t
    | CustomConstr of Mebi_wrapper.Enc.t * Mebi_wrapper.Enc.t

  (* let is_option (x : t) : bool =
     match x with OptionConstr _ -> true | _ -> false
     ;; *)

  let to_string (x : t) : string Mebi_wrapper.mm =
    Log.debug "params.WeakEnc.to_string";
    let open Mebi_wrapper in
    match x with
    | OptionConstr label_enc ->
      return
        (Printf.sprintf
           "TODO: OptionConstr %s => %s"
           (Enc.to_string label_enc)
           (* (decode_to_string label_enc) *)
           "TODO")
    | CustomConstr (tau_enc, label_enc) ->
      return
        (Printf.sprintf
           "TODO: CustomConstr %s %s"
           (Mebi_wrapper.Enc.to_string tau_enc)
           (Mebi_wrapper.Enc.to_string label_enc))
  ;;

  let eq x y : bool =
    Log.debug "params.WeakEnc.eq";
    match x, y with
    | OptionConstr x, OptionConstr y -> Mebi_wrapper.Enc.equal x y
    | CustomConstr (x1, x2), CustomConstr (y1, y2) ->
      Mebi_wrapper.Enc.equal x1 y1 && Mebi_wrapper.Enc.equal x2 y2
    | _, _ -> false
  ;;
end

(** If snd is none, then fst is used for both. *)
let default_weak_types : WeakEnc.t option * WeakEnc.t option = None, None

let the_weak_types : (WeakEnc.t option * WeakEnc.t option) ref =
  ref default_weak_types
;;

let fst_weak_type () : WeakEnc.t option = fst !the_weak_types

let snd_weak_type () : WeakEnc.t option =
  match !the_weak_types with _, Some a -> Some a | a, None -> a
;;

let reset_weak_types () : unit =
  Log.debug "params.reset_weak_types";
  the_weak_types := default_weak_types;
  reset_weak_type_args ()
;;

let printout_fst_weak_type_str () : string Mebi_wrapper.mm =
  Log.debug "params.printout_fst_weak_type_str";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  match fst !the_weak_types with
  | None -> return "Weak 1 is None (unset)"
  | Some w ->
    let* s = WeakEnc.to_string w in
    return (Printf.sprintf "Weak 1 is %s" s)
;;

let printout_fst_weak_type () : unit Mebi_wrapper.mm =
  Log.debug "params.printout_fst_weak_type";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  let* s = printout_fst_weak_type_str () in
  Log.notice s;
  return ()
;;

let printout_snd_weak_type_str () : string Mebi_wrapper.mm =
  Log.debug "params.printout_snd_weak_type_str";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  match snd !the_weak_types with
  | None -> return "Weak 2 is None (unset, will use Weak 1)"
  | Some w ->
    let* s = WeakEnc.to_string w in
    return (Printf.sprintf "Weak 1 is %s" s)
;;

let printout_snd_weak_type () : unit Mebi_wrapper.mm =
  Log.debug "params.printout_snd_weak_type";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  let* s = printout_snd_weak_type_str () in
  Log.notice s;
  return ()
;;

let printout_weak_types_str () : string Mebi_wrapper.mm =
  Log.debug "params.printout_weak_types_str";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  match snd !the_weak_types with
  | None ->
    (match fst !the_weak_types with
     | None -> return (Printf.sprintf "Weak Params: None (unset)")
     | Some w ->
       let* s = WeakEnc.to_string w in
       return (Printf.sprintf "Weak Params: %s" s))
  | Some _ ->
    let* x = printout_fst_weak_type_str () in
    let* y = printout_snd_weak_type_str () in
    return (Printf.sprintf "Weak Params:\n- %s\n- %s" x y)
;;

let printout_weak_types () : unit Mebi_wrapper.mm =
  Log.debug "params.printout_weak_types";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  let* s = printout_weak_types_str () in
  Log.notice s;
  return ()
;;

let weak_type_arg_to_kind (t : WeakArgs.t) : WeakEnc.t Mebi_wrapper.mm =
  Log.debug "params.weak_type_arg_to_kind";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  let open Mebi_utils in
  match t with
  | OptionConstr label_tref ->
    let* label_enc : Enc.t = encode_constrexpr label_tref in
    let* _ = decode label_enc in
    return (WeakEnc.OptionConstr label_enc)
  | CustomConstr (tau_tref, label_ref) ->
    let* tau_enc : Enc.t = encode_constrexpr tau_tref in
    let* label_enc : Enc.t = encode_ref label_ref in
    let* _ = decode tau_enc in
    let* _ = decode label_enc in
    return (WeakEnc.CustomConstr (tau_enc, label_enc))
;;

let weak_type_arg_to_kind_opt (t : WeakArgs.t option)
  : WeakEnc.t option Mebi_wrapper.mm
  =
  Log.debug "params.weak_type_arg_to_kind_opt";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  match t with
  | Some x ->
    let* y = weak_type_arg_to_kind x in
    return (Some y)
  | None -> return None
;;

let set_fst_weak_type_arg (t : WeakArgs.t) : unit =
  Log.debug "params.set_fst_weak_type_arg";
  the_weak_type_args := Some t, snd !the_weak_type_args
;;

let set_snd_weak_type_arg (t : WeakArgs.t) : unit =
  Log.debug "params.set_snd_weak_type_arg";
  the_weak_type_args := fst !the_weak_type_args, Some t
;;

let set_weak_types_args (t : WeakArgs.t * WeakArgs.t option) : unit =
  Log.debug "params.set_weak_types_args";
  set_fst_weak_type_arg (fst t);
  (match t with _, Some b -> set_snd_weak_type_arg b | _, None -> ());
  Log.notice "Set the Weak params."
;;

(**********************)
(** Run ***************)
(**********************)

let obtain_weak_kinds_from_args () : unit Mebi_wrapper.mm =
  Log.debug "params.obtain_weak_kinds_from_args";
  (* reset_weak_type_kind (); *)
  let open Mebi_wrapper in
  if !the_weak_mode
  then (
    let open Mebi_wrapper.Syntax in
    let* the_fst = weak_type_arg_to_kind_opt (fst !the_weak_type_args) in
    let* the_snd = weak_type_arg_to_kind_opt (snd !the_weak_type_args) in
    the_weak_types := the_fst, the_snd;
    return ())
  else (
    match !the_weak_type_args with
    | None, None -> return ()
    | _, _ ->
      Log.warning "(Not in weak mode, resetting weak params.)";
      reset_weak_types ();
      return ())
;;

let get_fst_params () : int * WeakEnc.t option = fst_bound (), fst_weak_type ()
let get_snd_params () : int * WeakEnc.t option = snd_bound (), snd_weak_type ()

(**********************)
(** All ***************)
(**********************)

let printout_all () : unit Mebi_wrapper.mm =
  Log.debug "params.printout_all";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  (* let* _ = obtain_weak_kinds_from_args () in *)
  let* s = printout_weak_types_str () in
  Log.notice
    (Printf.sprintf
       "Current plugin configuration:\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n\
        %s\n"
       (printout_bounds_str ())
       (printout_fail_if_incomplete_str ())
       (printout_fail_if_not_bisim_str ())
       (printout_dump_to_file_str ())
       (printout_show_any_str ())
       (printout_show_notice_str ())
       (printout_show_debug_str ())
       (printout_show_details_str ())
       (printout_show_result_str ())
       (printout_show_warning_str ())
       (printout_weak_mode_str ())
       s);
  (* printout_weak_types () *) return ()
;;

let reset_all () : unit Mebi_wrapper.mm =
  Log.debug "params.reset_all";
  reset_bounds ();
  reset_fail_if_incomplete ();
  reset_fail_if_not_bisim ();
  reset_dump_to_file ();
  reset_show_any ();
  reset_show_notice ();
  reset_show_debug ();
  reset_show_details ();
  reset_show_result ();
  reset_show_warning ();
  reset_weak_mode ();
  reset_weak_types ();
  Log.notice "Reset all plugin params.";
  printout_all ()
;;
