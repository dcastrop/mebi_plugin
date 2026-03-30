module type S = sig
  type weak
  type 'a mm
  type lts
  type fsm
  type bisimilarity
  type result

  val build_lts
    :  ?weak:weak option
    -> Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> lts mm

  val build_fsm
    :  ?weak:weak option
    -> Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> fsm mm

  type t =
    | MakeLTS of rocq_args
    | MakeFSM of rocq_args
    | Saturate of rocq_args
    | Minimize of rocq_args
    | Merge of rocq_pair
    | CheckBisim of rocq_pair

  and rocq_args = Constrexpr.constr_expr * Libnames.qualid

  and rocq_pair =
    { a : rocq_args
    ; b : rocq_args
    }

  val do_make_lts
    :  Constrexpr.constr_expr * Libnames.qualid
    -> Libnames.qualid list
    -> bisimilarity option mm

  val do_make_fsm
    :  Constrexpr.constr_expr * Libnames.qualid
    -> Libnames.qualid list
    -> bisimilarity option mm

  val do_saturate
    :  Constrexpr.constr_expr * Libnames.qualid
    -> Libnames.qualid list
    -> bisimilarity option mm

  val do_minimize
    :  Constrexpr.constr_expr * Libnames.qualid
    -> Libnames.qualid list
    -> bisimilarity option mm

  val build_fsms
    :  rocq_args
    -> rocq_args
    -> Libnames.qualid list
    -> (fsm * fsm) mm

  val do_merge : rocq_pair -> Libnames.qualid list -> bisimilarity option mm
  val fail_if_not_bisim : result -> unit

  val do_check_bisim
    :  rocq_pair
    -> Libnames.qualid list
    -> bisimilarity option mm

  val run : Libnames.qualid list -> t -> bisimilarity option mm
end

module Make (Log : Logger.S) (W : Wrapper.S) :
  S
  with type weak = W.Weak.t
   and type 'a mm = 'a W.M.mm
   and type lts = W.Model.LTS.t
   and type fsm = W.Model.FSM.t
   and type bisimilarity = W.Model.Bisimilarity.t
   and type result = W.Model.Bisimilarity.Result.t
