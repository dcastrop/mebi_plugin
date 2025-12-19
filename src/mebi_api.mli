type bound_config = Model_info.bound_config
and boundable = Model_info.boundable

val default_bound_config : unit -> bound_config

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

val default_weak_config : unit -> weak_config

type fail_config =
  { incomplete : bool
  ; not_bisim : bool
  }

val default_fail_config : unit -> fail_config

type debug_config =
  { dump_to_json : bool
  ; debug : bool
  }

val default_debug_config : unit -> debug_config

type t =
  { bounds : bound_config
  ; weak_mode : weak_config
  ; fail_if : fail_config
  ; debug : debug_config
  ; output : Output_config.t
  }

val default_config : unit -> t
val the_config : t ref
val reset_config : unit -> unit
val reset_bounds : unit -> unit
val reset_weak_mode : unit -> unit
val reset_fail_if : unit -> unit
val reset_debug : unit -> unit
val reset_output : unit -> unit
val set_bound : int -> unit
val set_bound_for : boundable -> unit
val set_bounds : int -> boundable -> unit
val get_bounds : unit -> bound_config
val set_weak_mode_enabled : bool -> unit
val set_weak_args : weak_args option -> unit
val set_weak_arg1 : weak_arg option -> unit
val set_weak_arg2 : weak_arg option -> unit
val get_weak_arg1 : unit -> weak_arg option
val get_weak_arg2 : unit -> weak_arg option
val set_weak_encs : weak_encs option -> unit
val get_weak_enc1 : unit -> weak_enc option
val get_weak_enc2 : unit -> weak_enc option
val set_weak_mode : bool -> weak_args option -> weak_encs option -> unit
val is_in_weak_mode : unit -> bool
val has_weak_args : unit -> bool
val load_weak_arg : weak_arg -> Mebi_weak.t Mebi_wrapper.mm
val load_weak_arg_opt : weak_arg option -> Mebi_weak.t option Mebi_wrapper.mm
val load_weak_encs : unit -> unit Mebi_wrapper.mm
val load_weak_args : unit -> unit Mebi_wrapper.mm
val set_fail_ifincomplete : bool -> unit
val set_fail_ifnotbisim : bool -> unit
val set_fail_if : bool -> bool -> unit
val fail_if_incomplete : unit -> bool
val fail_if_not_bisim : unit -> bool
val set_dump_to_json : bool -> unit
val set_debug : bool -> unit
val set_debug_config : bool -> bool -> unit
val set_output_enabled : bool -> unit
val set_output_level : Feedback.level -> bool -> unit
val set_output_special : Output_kind.special -> bool -> unit

val set_output_config
  :  bool
  -> Feedback.level * bool
  -> Output_kind.special * bool
  -> unit
