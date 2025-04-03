module Vernac : sig
  module LTS : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> Lts.lts Mebi_monad.t

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t
  end

  module FSM : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> Fsm.fsm Mebi_monad.t

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t
  end

  module Minim : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> (Fsm.fsm * Fsm.fsm) Mebi_monad.t

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t
  end

  module Merged : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> (Fsm.fsm * Fsm.fsm * Fsm.fsm) Mebi_monad.t

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t
  end

  module Bisim : sig
    exception UnexpectedResultKind of Bisimilarity.result

    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> (Fsm.fsm * Fsm.fsm * Bisimilarity.result) Mebi_monad.t

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_monad.t
  end
end
