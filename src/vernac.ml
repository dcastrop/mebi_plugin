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

(** [fail_if_incomplete * bounds * initial_term] *)
type term_params = bool * int * Constrexpr.constr_expr

(** [term_params * primary_lts] *)
type term_lts_params = (term_params * Libnames.qualid)

type run_kind =
  | LTS of term_params
  | FSM of term_params
  | Minim of term_params
  | Merge of (term_lts_params * term_lts_params) 
  | Bisim of (term_lts_params * term_lts_params) 

type run_params = run_kind * Libnames.qualid list
