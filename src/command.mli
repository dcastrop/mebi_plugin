val set_dump_to_file_flag : bool -> unit Mebi_wrapper.mm
val set_show_debug_flag : bool -> unit Mebi_wrapper.mm

val default_bound : int


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