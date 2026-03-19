module type S = sig
  include Wrapper.S

  val the_result : Decode.bisimilarity ref option ref

  exception NoResultFound

  val get_the_result : unit -> Model.Bisimilarity.t
  val get_fsm_a : ?saturated:bool -> unit -> Model.FSM.t
  val get_fsm_b : ?saturated:bool -> unit -> Model.FSM.t

  exception CannotOverrideResult of Model.Bisimilarity.t

  val set_the_result : Model.Bisimilarity.t -> unit

  exception BisimilarityResultNotFound

  val check_bisimilarity
    :  Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit

  val get_bisimilar_partition : unit -> Model.Partition.t

  val get_bisimilar_states
    :  ?pi:Model.Partition.t
    -> Model.State.t
    -> Model.States.t

  val are_states_bisimilar : Model.State.t -> Model.State.t -> bool

  (* val get_candidates : Model.State.t -> Model.Label.t -> Model.EdgeMap.t' -> Model.State.t -> Model.States.t *)
end

module Make
    (Log : Logger.S)
     (* (W : Wrapper.S) :
        S
        with type enc = W.enc
        and type node = W.node
        and type tree = W.tree
        and type trees = W.trees *)
    (Ctx : Rocq_context.S)
    (Enc : Encoding.S) :
  S
  with module M.Ctx = Ctx
   and type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t
(* val make
   :  ?log:(unit -> (module Logger.S))
   -> ?enc:((module Logger.S) -> (module Encoding.S))
   -> ?ctx:(module Rocq_context.S)
   -> unit
   -> (module S) *)
