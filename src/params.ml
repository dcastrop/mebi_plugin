open Logging

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
  the_bounds := b;
  Log.debug (printout_bounds_str ())
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
  the_dump_to_file := b;
  Log.debug (printout_dump_to_file_str ())
;;

(**********************)
(** Messages **********)
(**********************)

let reset_show_debug () : unit = Logging.reset_show_debug_enabled ()

let printout_show_debug_str () : string =
  if is_show_debug_enabled ()
  then "Debug messages will be shown."
  else "Debug messages set to be hidden."
;;

let printout_show_debug () : unit = Log.notice (printout_show_debug_str ())

let set_show_debug (b : bool) : unit =
  Logging.set_show_debug b;
  Log.debug (printout_show_debug_str ())
;;

let reset_show_details () : unit = Logging.reset_show_details_enabled ()

let printout_show_details_str () : string =
  if is_show_details_enabled ()
  then "Detailed messages will be shown. (where possible)"
  else "Detailed messages set to be hidden."
;;

let printout_show_details () : unit = Log.notice (printout_show_details_str ())

let set_show_details (b : bool) : unit =
  Logging.set_show_details b;
  Log.debug (printout_show_details_str ())
;;

(**********************)
(** Weak Mode *********)
(**********************)

let default_weak_mode : bool = true
let the_weak_mode : bool ref = ref default_weak_mode
let reset_weak_mode () : unit = the_weak_mode := default_weak_mode

let printout_weak_mode_str () : string =
  Printf.sprintf
    "Currently in %s mode."
    (if !the_weak_mode then "weak" else "strong")
;;

let printout_weak_mode () : unit = Log.notice (printout_weak_mode_str ())

let set_weak_mode (b : bool) : unit =
  the_weak_mode := b;
  Log.debug (printout_weak_mode_str ())
;;

(**********************)
(** Weak Type Args ****)
(**********************)

module WeakArgs = struct
  type t =
    | OptionRef of Libnames.qualid
    | OptionConstr of Constrexpr.constr_expr
    | CustomRef of Libnames.qualid * Libnames.qualid
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

(* must be re-obtained for each mebi_wrapper.run (...) *)
module WeakKind = struct
  type t =
    | OptionRef of (Mebi_wrapper.E.t * Names.GlobRef.t)
    | OptionConstr of Mebi_wrapper.E.t
    | CustomRef of
        (Mebi_wrapper.E.t * Names.GlobRef.t)
        * (Mebi_wrapper.E.t * Names.GlobRef.t)
    | CustomConstr of Mebi_wrapper.E.t * (Mebi_wrapper.E.t * Names.GlobRef.t)

  let to_string (x : t) : string Mebi_wrapper.mm =
    Log.debug "params.WeakKind.to_string";
    let open Mebi_wrapper in
    match x with
    | OptionRef (label_enc, label_gref) ->
      return
        (Printf.sprintf
           "TODO: OptionRef %s => %s"
           (E.to_string label_enc)
           (decode_to_string label_enc))
    | OptionConstr label_enc ->
      return
        (Printf.sprintf
           "TODO: OptionConstr %s => %s"
           (E.to_string label_enc)
           (decode_to_string label_enc))
    | CustomRef ((tau_enc, tau_gref), (label_enc, label_gref)) ->
      return
        (Printf.sprintf
           "TODO: CustomRef %s %s"
           (Mebi_wrapper.E.to_string tau_enc)
           (Mebi_wrapper.E.to_string label_enc))
    | CustomConstr (tau_enc, (label_enc, label_gref)) ->
      return
        (Printf.sprintf
           "TODO: CustomConstr %s %s"
           (Mebi_wrapper.E.to_string tau_enc)
           (Mebi_wrapper.E.to_string label_enc))
  ;;

  let eq x y : bool =
    Log.debug "params.WeakKind.eq";
    match x, y with
    | OptionRef (x, _), OptionRef (y, _) -> Mebi_wrapper.E.eq x y
    | OptionConstr x, OptionConstr y -> Mebi_wrapper.E.eq x y
    | CustomRef ((x1, _), (x2, _)), CustomRef ((y1, _), (y2, _)) ->
      Mebi_wrapper.E.eq x1 y1 && Mebi_wrapper.E.eq x2 y2
    | CustomConstr (x1, (x2, _)), CustomConstr (y1, (y2, _)) ->
      Mebi_wrapper.E.eq x1 y1 && Mebi_wrapper.E.eq x2 y2
    | _, _ -> false
  ;;
end

(** If snd is none, then fst is used for both. *)
let default_weak_types : WeakKind.t option * WeakKind.t option = None, None

let the_weak_types : (WeakKind.t option * WeakKind.t option) ref =
  ref default_weak_types
;;

let fst_weak_type () : WeakKind.t option = fst !the_weak_types

let snd_weak_type () : WeakKind.t option =
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
    let* s = WeakKind.to_string w in
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
    let* s = WeakKind.to_string w in
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
       let* s = WeakKind.to_string w in
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

let weak_type_arg_to_kind (t : WeakArgs.t) : WeakKind.t Mebi_wrapper.mm =
  Log.debug "params.weak_type_arg_to_kind";
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  let open Mebi_utils in
  match t with
  | OptionRef label_ref ->
    let* (label_enc : E.t) = encode_ref label_ref in
    let* _ = decode label_enc in
    return (WeakKind.OptionRef (label_enc, ref_to_glob label_ref))
  | OptionConstr label_tref ->
    let* (label_enc : E.t) = encode_tref label_tref in
    let* _ = decode label_enc in
    return (WeakKind.OptionConstr label_enc)
  | CustomRef (tau_ref, label_ref) ->
    let* (tau_enc : E.t) = encode_ref tau_ref in
    let* (label_enc : E.t) = encode_ref label_ref in
    let* _ = decode tau_enc in
    let* _ = decode label_enc in
    return
      (WeakKind.CustomRef
         ((tau_enc, ref_to_glob tau_ref), (label_enc, ref_to_glob label_ref)))
  | CustomConstr (tau_tref, label_ref) ->
    let* (tau_enc : E.t) = encode_tref tau_tref in
    let* (label_enc : E.t) = encode_ref label_ref in
    let* _ = decode tau_enc in
    let* _ = decode label_enc in
    return (WeakKind.CustomConstr (tau_enc, (label_enc, ref_to_glob label_ref)))
;;

let weak_type_arg_to_kind_opt (t : WeakArgs.t option)
  : WeakKind.t option Mebi_wrapper.mm
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
  match t with _, Some b -> set_snd_weak_type_arg b | _, None -> ()
;;

(**********************)
(** Run ***************)
(**********************)

let obtain_weak_kinds_from_args () : unit Mebi_wrapper.mm =
  Log.debug "params.obtain_weak_kinds_from_args";
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

let get_fst_params () : int * WeakKind.t option = fst_bound (), fst_weak_type ()
let get_snd_params () : int * WeakKind.t option = snd_bound (), snd_weak_type ()

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
       "Current plugin configuration:\n%s\n%s\n%s\n%s\n%s\n%s\n"
       (printout_bounds_str ())
       (printout_dump_to_file_str ())
       (printout_show_debug_str ())
       (printout_show_details_str ())
       (printout_weak_mode_str ())
       s);
  (* printout_weak_types () *) return ()
;;

let reset_all () : unit Mebi_wrapper.mm =
  Log.debug "params.reset_all";
  reset_bounds ();
  reset_dump_to_file ();
  reset_show_debug ();
  reset_show_details ();
  reset_weak_mode ();
  reset_weak_types ();
  Log.notice "Reset all plugin params.";
  printout_all ()
;;

(**********************)
(** Encoding only *****)
(**********************)

module WeakKindEnc = struct
  type t =
    | OptionRef of Mebi_wrapper.E.t
    | OptionConstr of Mebi_wrapper.E.t
    | CustomRef of Mebi_wrapper.E.t * Mebi_wrapper.E.t
    | CustomConstr of Mebi_wrapper.E.t * Mebi_wrapper.E.t

  let of_full (x : WeakKind.t) : t =
    Log.debug "params.WeakKindEnv.of_full";
    match x with
    | OptionRef (x, _) -> OptionRef x
    | OptionConstr x -> OptionConstr x
    | CustomRef ((x1, _), (x2, _)) -> CustomRef (x1, x2)
    | CustomConstr (x1, (x2, _)) -> CustomConstr (x1, x2)
  ;;

  let to_string (x : t) : string =
    Log.debug "params.WeakKindEnv.to_string";
    let open Mebi_wrapper in
    match x with
    | OptionRef label_enc ->
      Printf.sprintf "OptionRef %s" (E.to_string label_enc)
    | OptionConstr label_enc ->
      Printf.sprintf "OptionConstr %s" (E.to_string label_enc)
    | CustomRef (tau_enc, label_enc) ->
      Printf.sprintf
        "CustomRef %s %s"
        (Mebi_wrapper.E.to_string tau_enc)
        (Mebi_wrapper.E.to_string label_enc)
    | CustomConstr (tau_enc, label_enc) ->
      Printf.sprintf "CustomConstr %s" (Mebi_wrapper.E.to_string tau_enc)
  ;;

  let eq x y : bool =
    Log.debug "params.WeakKindEnv.eq";
    match x, y with
    | OptionRef x, OptionRef y -> Mebi_wrapper.E.eq x y
    | OptionConstr x, OptionConstr y -> Mebi_wrapper.E.eq x y
    | CustomRef (x1, x2), CustomRef (y1, y2) ->
      Mebi_wrapper.E.eq x1 y1 && Mebi_wrapper.E.eq x2 y2
    | CustomConstr (x1, x2), CustomConstr (y1, y2) ->
      Mebi_wrapper.E.eq x1 y1 && Mebi_wrapper.E.eq x2 y2
    | _, _ -> false
  ;;
end
