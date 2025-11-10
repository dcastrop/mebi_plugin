open Logging

module Scope = struct
  let default_show_debug_scope : bool = true
  let show_debug_scope : bool ref = ref default_show_debug_scope

  let the_counter_funs : (((unit -> int) * (unit -> int)) * int ref) ref =
    ref (Utils.new_int_counter ())
  ;;

  let incr () : int = (fst (fst !the_counter_funs)) ()
  let decr () : int = (snd (fst !the_counter_funs)) ()
  let getvalue () : int = !(snd !the_counter_funs)

  let msg
        ?(prefix : string = "")
        ?(infix : string = "")
        ?(suffix : string = "")
        ?(info : string option = None)
        (value : int)
        (msg : string)
    : unit
    =
    if !show_debug_scope
    then
      Log.debug
        (Printf.sprintf
           "[ %i%s %s%s ] %s"
           value
           (Utils.suffix prefix)
           msg
           (Utils.suffix infix)
           (Utils.infix suffix));
    match info with None -> () | Some s -> Log.info s
  ;;

  let return
        ?(prefix : string = "")
        ?(infix : string = "")
        ?(suffix : string = "")
        ?(info : string option = None)
        ()
    : unit
    =
    msg ~prefix ~infix ~suffix ~info (getvalue ()) "** RETURN"
  ;;

  let start
        ?(prefix : string = "")
        ?(infix : string = "")
        ?(suffix : string = "")
        ?(info : string option = None)
        ()
    : unit
    =
    msg ~prefix ~infix ~suffix ~info (incr ()) "-> START"
  ;;

  let close
        ?(prefix : string = "")
        ?(infix : string = "")
        ?(suffix : string = "")
        ?(info : string option = None)
        ()
    : unit
    =
    msg ~prefix ~infix ~suffix ~info (decr ()) "## CLOSE"
  ;;
end
