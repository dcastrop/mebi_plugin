open Logging

(**********************)
(** Bounds ************)
(**********************)

(** [bound] is the total number of states to be allowed when building an LTS. *)
let default_bound : int = 10

(** Allows for a secondary bound to be specified. *)
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
(** Weak Mode *********)
(**********************)

(**********************)
(** Check All *********)
(**********************)

let printout_all () : unit =
  Log.notice
    (Printf.sprintf
       "Current plugin configuration:\n%s\n%s\n%s\n%s\n%s\n%s\n"
       (printout_bounds_str ())
       (printout_dump_to_file_str ())
       (printout_show_debug_str ())
       (printout_show_details_str ())
       (printout_weak_mode_str ())
       (* (printout_weak_type_str ()) *)
       "TODO: WEAK TYPE")
;;
