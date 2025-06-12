type output_kind =
  | Check of unit
  | Show of unit
  | Dump of string option

type result_kind =
  | LTS of Lts.lts
  | FSM of Fsm.fsm
  | Minim of (Fsm.fsm * Fsm.fsm)
  | Merge of (Fsm.fsm * Fsm.fsm * Fsm.fsm)
  | Bisim of Bisimilarity.result

type term_params = bool * int * Constrexpr.constr_expr

type run_kind =
  | LTS of term_params
  | FSM of term_params
  | Minim of term_params
  | Merge of (term_params * term_params)
  | Bisim of (term_params * term_params)

type run_params = run_kind * Libnames.qualid list
