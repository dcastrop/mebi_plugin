
val default_bound : int
val get_bound : unit -> unit 
val set_bound : int -> unit Mebi_wrapper.mm

val get_dump_to_file_flag : unit -> unit 
val set_dump_to_file_flag : bool -> unit Mebi_wrapper.mm

val get_show_debug_flag : unit -> unit 
val set_show_debug_flag : bool -> unit Mebi_wrapper.mm

val get_show_details_flag : unit -> unit 
val set_show_details_flag : bool -> unit Mebi_wrapper.mm

val get_weak_mode : unit -> unit 
val set_weak_mode : bool -> unit Mebi_wrapper.mm

type weak_action_arg =
  | OptionRef of Libnames.qualid
  | OptionConstr of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

val get_weak_type : unit -> unit  Mebi_wrapper.mm
val set_weak_type : weak_action_arg -> unit Mebi_wrapper.mm

val check_all : unit -> unit Mebi_wrapper.mm

type model_kind =
  | LTS
  | FSM

type coq_model = Constrexpr.constr_expr * Libnames.qualid
type make_model = model_kind * coq_model

type command_kind =
  | Help of Mebi_help.help_kind
  | MakeModel of make_model
  | SaturateModel of coq_model
  | MinimizeModel of coq_model
  | CheckBisimilarity of (coq_model * coq_model)
  | Info of unit

val run :  command_kind-> Libnames.qualid list-> unit Mebi_wrapper.mm 
