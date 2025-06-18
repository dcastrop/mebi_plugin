type output_kind =
  | Check of unit
  | Show of unit
  | Dump of string option

let get_name ?(default : string = "unnamed") (o : output_kind) : string =
  match o with
  | Check () -> default
  | Show () -> default
  | Dump name -> (match name with None -> default | Some name -> name)
;;

type result_kind =
  | LTS of Lts.t
  | FSM of Fsm.t
  | Minim of Fsm.pair
  | Merge of (Fsm.pair * Fsm.t)
  | Bisim of Bisimilarity.result_kind

(** [fail_if_incomplete * bounds * initial_term] *)
type term_params = bool * int * Constrexpr.constr_expr

(** [silent_transition * type_of_actions] *)
type weak_params = Constrexpr.constr_expr * Libnames.qualid

(** [(term_params * weak_params) * ]*)
type lts_params = term_params * weak_params option

type multi_lts_params = lts_params * Libnames.qualid

type run_kind =
  | LTS of lts_params
  | FSM of lts_params
  | Minim of lts_params
  | Merge of (multi_lts_params * multi_lts_params)
  | Bisim of (multi_lts_params * multi_lts_params)

type run_params = run_kind * Libnames.qualid list
