exception NothingToDo
exception NotImplemented

module type S = sig
  type enc
  type node
  type tree
  type trees

  module Tactic : Proof_solver_tactic.S

  module W :
    Results.S
    with type enc = enc
     and type node = node
     and type tree = tree
     and type trees = trees

  module ProofState :
    Proof_solver_statem.S
    with type enc = enc
     and type node = node
     and type state = W.Model.State.t
     and type label = W.Model.Label.t
     and type annotation = W.Model.Annotation.t
     and type transition = W.Model.Transition.t

  module Step : (_ : Proof_solver_wrapper.Args) ->
    Proof_solver_step.S with type tactic = Tactic.t

  val get_updated_pstate : unit Proofview.tactic -> Declare.Proof.t
  val step : Declare.Proof.t -> Declare.Proof.t
end

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t

type t = { solver : (module S) }

val reset_the_cache : unit -> unit

exception NoCachedModules

val make : (module Encoding.S) -> unit -> t ref
val is_done : unit -> bool

val init
  :  ?enc:(unit -> (module Encoding.S))
  -> Declare.Proof.t
  -> Libnames.qualid list
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Declare.Proof.t

val step : Declare.Proof.t -> Declare.Proof.t
val solve : ?bound:int -> Declare.Proof.t -> Declare.Proof.t
