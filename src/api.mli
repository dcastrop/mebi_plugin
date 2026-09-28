(** Per-message-kind output settings live in [Logger]; what remains here is the
    part that was never about logging. [config_output] and [set_output] forward
    to [Logger.configure]. *)
type output_config =
  { mutable decode_results : bool
  ; mutable dump_results : bool
  }

val output_config_default : output_config
val the_output_config : output_config ref
val reset_output_config : unit -> unit
val config_output : bool -> Output.Kind.t -> unit
val output_config_decode_results : bool -> unit
val output_config_dump_results : bool -> unit
val set_output : bool -> string -> unit

(* *)
val make_enc : (module Encoding.Packed.PackedS) -> (module Encoding.S)
val make_enc_int : unit -> (module Encoding.S)

type fail_flags =
  { mutable empty : bool
  ; mutable incomplete : bool
  ; mutable non_bisimilar : bool
  }

val the_fail_flags_default : fail_flags
val the_fail_flags : fail_flags ref
val reset_the_fail_flags : unit -> unit
val set_fail_flag_empty : bool -> unit
val set_fail_flag_incomplete : bool -> unit
val set_fail_flag_non_bisimilar : bool -> unit

type bounds_args =
  | States of int
  | Transitions of int

val default_bounds : bounds_args
val the_bounds_args : bounds_args ref
val reset_bounds_args : unit -> unit
val set_the_bounds_args : bounds_args -> unit

type weak_args =
  { a : weak_arg option
  ; b : weak_arg option
  }

and weak_arg =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

val the_weak_args : weak_args ref option ref
val reset_weak_args : unit -> unit
val set_the_weak_args : weak_arg option -> weak_arg option -> unit
val get_the_weak_arg1 : unit -> weak_arg option
val get_the_weak_arg2 : unit -> weak_arg option
val set_the_weak_arg1 : weak_arg -> unit
val set_the_weak_arg2 : weak_arg -> unit
val reset_all : unit -> unit
