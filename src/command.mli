val bounded_lts
  :  ?debug:bool
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val fsm_bisim_ks90 : ?debug:bool -> Fsm.fsm -> Fsm.fsm -> unit Mebi_monad.t

val lts_bisim_ks90
  :  ?debug:bool
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t
