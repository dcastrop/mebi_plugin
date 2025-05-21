open Mebi_monad
(* open Mebi_monad.Monad_syntax *)

type internals =
  { int_enc : (EConstr.t, int) Hashtbl.t
  ; coq_enc : (int, EConstr.t) Hashtbl.t
  ; context : coq_context
  }
