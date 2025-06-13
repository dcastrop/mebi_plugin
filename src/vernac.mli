type output_kind =
  | Check of unit
  | Show of unit
  | Dump of string option

type result_kind =
  | LTS of Lts.lts
  | FSM of Fsm.fsm
  | Minim of (Fsm.fsm * Fsm.fsm)
  | Merge of (Fsm.fsm * Fsm.fsm * Fsm.fsm)
  | Bisim of Bisimilarity.result_kind

type term_params = bool * int * Constrexpr.constr_expr
type weak_params = Constrexpr.constr_expr * Libnames.qualid
type lts_params = term_params * weak_params option
type multi_lts_params = lts_params * Libnames.qualid

type run_kind =
  | LTS of lts_params
  | FSM of lts_params
  | Minim of lts_params
  | Merge of (multi_lts_params * multi_lts_params)
  | Bisim of (multi_lts_params * multi_lts_params)

type run_params = run_kind * Libnames.qualid list
