val cmd_bounded_lts
  :  ?show_details:bool
  -> ?show_debug:bool
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_bounded_lts_to_fsm
  :  ?show_details:bool
  -> ?show_debug:bool
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_bisim_ks90_using_fsm
  :  ?show_details:bool
  -> ?show_debug:bool
  -> Fsm.fsm
  -> Fsm.fsm
  -> unit Mebi_monad.t

val cmd_bisim_ks90_using_lts_to_fsm
  :  ?show_details:bool
  -> ?show_debug:bool
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t
