val build_fsm
  :  ?debug:bool
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> Fsm.fsm Mebi_monad.t

val bounded_lts
  :  ?debug:bool
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val bisim_ks90
  :  ?debug:bool
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t
