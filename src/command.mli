module Vernac : sig
  val show_lts
    :  ?params:Utils.Logging.params
    -> ?bound:int
    -> Constrexpr.constr_expr_r CAst.t
    -> Names.GlobRef.t list
    -> unit Mebi_monad.t

  val show_fsm
    :  ?params:Utils.Logging.params
    -> ?bound:int
    -> Constrexpr.constr_expr_r CAst.t
    -> Names.GlobRef.t list
    -> unit Mebi_monad.t

  val show_minim
    :  ?params:Utils.Logging.params
    -> ?bound:int
    -> Constrexpr.constr_expr_r CAst.t
    -> Names.GlobRef.t list
    -> unit Mebi_monad.t

  val show_merged_fsm
    :  ?params:Utils.Logging.params
    -> ?bound:int
    -> Constrexpr.constr_expr_r CAst.t
    -> Constrexpr.constr_expr_r CAst.t
    -> Names.GlobRef.t list
    -> unit Mebi_monad.t

  val show_bisim
    :  ?params:Utils.Logging.params
    -> ?bound:int
    -> Constrexpr.constr_expr_r CAst.t
    -> Constrexpr.constr_expr_r CAst.t
    -> Names.GlobRef.t list
    -> unit Mebi_monad.t
end

(* val cmd_bounded_lts
  :  ?params:Utils.Params.log
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_bounded_layered_lts
  :  ?params:Utils.Params.log
  -> Names.GlobRef.t list
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_bounded_lts_to_fsm
  :  ?params:Utils.Params.log
  -> Names.GlobRef.t
  -> Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_merge_fsm_from_lts
  :  ?params:Utils.Params.log
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_bisim_ks90_using_fsm
  :  ?params:Utils.Params.log
  -> Fsm.fsm
  -> Fsm.fsm
  -> unit Mebi_monad.t

val cmd_bisim_ks90_using_lts_to_fsm
  :  ?params:Utils.Params.log
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_minim_ks90_using_fsm
  :  ?params:Utils.Params.log
  -> Fsm.fsm
  -> unit Mebi_monad.t

val cmd_minim_ks90_using_lts_to_fsm
  :  ?params:Utils.Params.log
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t

val cmd_minim_ks90_using_lts_to_fsm_to_lts
  :  ?params:Utils.Params.log
  -> Names.GlobRef.t * Constrexpr.constr_expr_r CAst.t
  -> unit Mebi_monad.t *)
