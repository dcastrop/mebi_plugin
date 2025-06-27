type output_kind =
  | Check of unit
  | Show of unit
  | Dump of string option

val get_name : ?default:string -> output_kind -> string

type result_kind =
  | LTS of Lts.t
  | FSM of Fsm.t
  | Minim of Fsm.pair
  | Merge of (Fsm.pair * Fsm.t)
  | Alg of Algorithms.result

type term_params = bool * int * Constrexpr.constr_expr
type weak_params = Constrexpr.constr_expr * Libnames.qualid
type lts_params = term_params * weak_params option
type multi_lts_params = lts_params * Libnames.qualid

type run_kind =
  | LTS of lts_params * Libnames.qualid option
  | FSM of lts_params * Libnames.qualid option
  | Minim of lts_params
  | Merge of (multi_lts_params * multi_lts_params)
  | Bisim of (multi_lts_params * multi_lts_params)

type run_params = run_kind * Libnames.qualid list
