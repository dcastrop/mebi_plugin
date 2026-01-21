exception Mebi_proof_Hypothesis_Hyp of (Rocq_utils.hyp *EConstr.t  Rocq_utils.kind_pair)
exception Mebi_proof_Hypothesis_HTy of EConstr.t Rocq_utils.kind_pair

module type HTY_TYPE = sig
  type t

  val of_hty : Proofview.Goal.t -> EConstr.t Rocq_utils.kind_pair -> t
  val opt_of_hty : Proofview.Goal.t -> EConstr.t Rocq_utils.kind_pair -> t option
  val hty_is_a : Proofview.Goal.t -> EConstr.t Rocq_utils.kind_pair -> bool
end

module type HTY_S = sig
  type t

  val of_hty : Proofview.Goal.t ->EConstr.t Rocq_utils.kind_pair -> t
end

module MakeHTy : (_ : HTY_S) -> HTY_TYPE

module type HYP_TYPE = sig
  type t

  val hty_is_a : Proofview.Goal.t -> EConstr.t Rocq_utils.kind_pair -> bool
  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
  val opt_of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t option
  val hyp_is_a : Proofview.Goal.t -> Rocq_utils.hyp -> bool
end

module type HYP_S = sig
  module HTy : HTY_TYPE

  type t

  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
end

module MakeHyp : (_ : HYP_S) -> HYP_TYPE
module Make : (_ : HTY_S) -> HYP_TYPE
