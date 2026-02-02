let default_encoding () : (module Encoding.SEncoding) =
  (module Encoding.Int : Encoding.SEncoding)
;;

let default_context () : (module Rocq_context.SRocq_context) =
  (module Rocq_context.Default : Rocq_context.SRocq_context)
;;

(***********************************************************************)

(* TODO: failif, output *)

(***********************************************************************)

type fail_flags =
  { mutable empty : bool
  ; mutable incomplete : bool
  ; mutable non_bisimilar : bool
  }

let the_fail_flags_default : fail_flags =
  { empty = true; incomplete = true; non_bisimilar = true }
;;

let the_fail_flags : fail_flags ref = ref the_fail_flags_default
let reset_the_fail_flags () : unit = the_fail_flags := the_fail_flags_default

let set_fail_flag_empty (empty : bool) : unit =
  the_fail_flags := { !the_fail_flags with empty }
;;

let set_fail_flag_incomplete (incomplete : bool) : unit =
  the_fail_flags := { !the_fail_flags with incomplete }
;;

let set_fail_flag_non_bisimilar (non_bisimilar : bool) : unit =
  the_fail_flags := { !the_fail_flags with non_bisimilar }
;;

(***********************************************************************)

type bounds_args =
  | States of int
  | Transitions of int

let default_bounds : bounds_args = States 100
let the_bounds_args : bounds_args ref = ref default_bounds
let reset_bounds_args () : unit = the_bounds_args := default_bounds
let set_the_bounds_args (x : bounds_args) : unit = the_bounds_args := x

(***********************************************************************)

type weak_args =
  { a : weak_arg option
  ; b : weak_arg option
  }

and weak_arg =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

let the_weak_args : weak_args ref option ref = ref None
let reset_weak_args () : unit = the_weak_args := None

let set_the_weak_args (a : weak_arg option) (b : weak_arg option) : unit =
  the_weak_args := Some (ref { a; b })
;;

let get_the_weak_arg1 () : weak_arg option =
  match !the_weak_args with None -> None | Some x -> !x.a
;;

let get_the_weak_arg2 () : weak_arg option =
  match !the_weak_args with None -> None | Some x -> !x.b
;;

let set_the_weak_arg1 (x : weak_arg) : unit =
  the_weak_args := Some (ref { a = Some x; b = get_the_weak_arg2 () })
;;

let set_the_weak_arg2 (x : weak_arg) : unit =
  the_weak_args := Some (ref { a = get_the_weak_arg1 (); b = Some x })
;;
