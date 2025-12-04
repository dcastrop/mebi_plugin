open Logging

module Control = struct
  exception ExitDevelopmentTest of unit

  let get_fresh_counter () : ((unit -> int) * (unit -> int)) * int ref =
    Utils.new_int_counter ()
  ;;

  let default_target : int = 1

  let the_counter_funs : ((((unit -> int) * (unit -> int)) * int ref) * int) ref
    =
    ref (get_fresh_counter (), default_target)
  ;;

  let set_new_target (new_target : int) : unit =
    the_counter_funs := get_fresh_counter (), new_target
  ;;

  let incr () : int = (fst (fst (fst !the_counter_funs))) ()
  let decr () : int = (snd (fst (fst !the_counter_funs))) ()
  let getvalue () : int = !(snd (fst !the_counter_funs))
  let gettarget () : int = snd !the_counter_funs

  (** record this call, then check if we've reached the end condition -- raise exception if so.
  *)
  let tick () : unit =
    match Int.compare (incr ()) (gettarget ()) with
    | 1 -> raise (ExitDevelopmentTest ())
    | _ -> ()
  ;;
end

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

type 'a to_string =
  | A of (?args:Utils.Strfy.style_args -> 'a -> string)
  | B of ('a -> string)

let thing
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      (prefix : string)
      (x : 'a)
  : 'a to_string -> unit
  = function
  | A f -> Log.debug (Printf.sprintf "%s: %s" prefix (f ~args x))
  | B f -> Log.debug (Printf.sprintf "%s: %s" prefix (f x))
;;

let option (prefix : string) (x : 'a option) (f : 'a to_string) : unit =
  (* NOTE: wrap in fun to avoid printing both *)
  let y : unit -> unit =
    Option.cata
      (fun (x : 'a) -> fun () -> thing (Printf.sprintf "%s Some" prefix) x f)
      (fun () -> thing prefix "None" (A Utils.Strfy.string))
      x
  in
  y ()
;;
