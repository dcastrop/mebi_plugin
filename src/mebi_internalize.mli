type internals =
  { int_enc : (EConstr.t, int) Hashtbl.t
  ; coq_enc : (int, EConstr.t) Hashtbl.t
  ; context : Mebi_monad.coq_context
  }
