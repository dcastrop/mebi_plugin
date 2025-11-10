open Logging

module Scope = struct
  let default_show_debug_scope : bool = true
  let show_debug_scope : bool ref = ref default_show_debug_scope
  let scope_counter : int ref = ref 0

  let the_counter_funs : ((unit -> int) * (unit -> int)) ref =
    ref (Utils.new_int_counter ())
  ;;

  let incr () : int = (fst !the_counter_funs) ()
  let decr () : int = (snd !the_counter_funs) ()

  let start s () : unit =
    Log.debug
      (Printf.sprintf
         "[ %i -> START%s ]-----------------"
         (incr ())
         (Utils.suffix s))
  ;;

  let close s () : unit =
    Log.debug
      (Printf.sprintf
         "[ %i ## CLOSE%s ]-----------------"
         (decr ())
         (Utils.suffix s))
  ;;
end
