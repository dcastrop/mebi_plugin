val cmd_bounded_lts
  :  ?params:Utils.logging_params
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_bounded_lts_to_fsm
  :  ?params:Utils.logging_params
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_merge_fsm_from_lts
  :  ?params:Utils.logging_params
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_bisim_ks90_using_fsm
  :  ?params:Utils.logging_params
  -> Fsm.fsm
  -> Fsm.fsm
  -> unit Mebi_monad.t

val cmd_bisim_ks90_using_lts_to_fsm
  :  ?params:Utils.logging_params
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t
