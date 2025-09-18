
val default_bound : int
val get_bound : unit -> unit Mebi_wrapper.mm
val set_bound : int -> unit Mebi_wrapper.mm

val get_dump_to_file_flag : unit -> unit Mebi_wrapper.mm
val set_dump_to_file_flag : bool -> unit Mebi_wrapper.mm

val get_show_debug_flag : unit -> unit Mebi_wrapper.mm
val set_show_debug_flag : bool -> unit Mebi_wrapper.mm

val get_weak_mode : unit -> unit Mebi_wrapper.mm
val set_weak_mode : bool -> unit Mebi_wrapper.mm

type weak_action_kinds = | Option of Constrexpr.constr_expr | Custom of Constrexpr.constr_expr * Libnames.qualid
val get_weak_type : unit -> unit Mebi_wrapper.mm
val set_weak_type : weak_action_kinds -> unit Mebi_wrapper.mm




type output_kind =
  | Check of unit
  | Show of unit
  | Dump of string option

type term_params = bool * int * Constrexpr.constr_expr

type lts_params =
  term_params
  * (Constrexpr.constr_expr * Libnames.qualid) option

type multi_lts_params = lts_params * Libnames.qualid

type run_kind =
  | LTS of lts_params * Libnames.qualid option
  | FSM of lts_params * Libnames.qualid option
  | Minim of lts_params
  | Merge of (multi_lts_params * multi_lts_params)
  | Bisim of (multi_lts_params * multi_lts_params)

type run_params = run_kind * Libnames.qualid list

val vernac : output_kind -> run_params -> unit Mebi_wrapper.mm