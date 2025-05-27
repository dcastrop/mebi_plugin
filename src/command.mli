module Vernac : sig
  module LTS : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> ?equiv:Names.GlobRef.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> Lts.lts Mebi_internalize.W.mm

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?equiv:Names.GlobRef.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> ?equiv:Names.GlobRef.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm
  end

  module FSM : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> ?equiv:Names.GlobRef.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> Fsm.fsm Mebi_internalize.W.mm

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?equiv:Names.GlobRef.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> ?equiv:Names.GlobRef.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm
  end

  module Minim : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> (Fsm.fsm * Fsm.fsm) Mebi_internalize.W.mm

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm
  end

  module Merged : sig
    val build
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> (Fsm.fsm * Fsm.fsm * Fsm.fsm) Mebi_internalize.W.mm

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm
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
      -> (Fsm.fsm * Fsm.fsm * Bisimilarity.result) Mebi_internalize.W.mm

    val show
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm

    val dump
      :  ?params:Utils.Logging.params
      -> ?bound:int
      -> ?name:string
      -> Constrexpr.constr_expr_r CAst.t
      -> Constrexpr.constr_expr_r CAst.t
      -> Names.GlobRef.t list
      -> unit Mebi_internalize.W.mm
  end
end
