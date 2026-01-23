exception
  Mebi_proof_Hypothesis_Hyp of
    (Rocq_utils.hyp * Evd.econstr Rocq_utils.kind_pair)

exception Mebi_proof_Hypothesis_HTy of Evd.econstr Rocq_utils.kind_pair

module type HTY_TYPE = sig
  type t

  val of_hty : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> t

  val opt_of_hty
    :  Proofview.Goal.t
    -> Evd.econstr Rocq_utils.kind_pair
    -> t option

  val hty_is_a : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> bool
end

module type HTY_S = sig
  type t

  val of_hty : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> t
end

module MakeHTy : (HTy : HTY_S) -> sig
  type t = HTy.t

  val of_hty : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> t

  val opt_of_hty
    :  Proofview.Goal.t
    -> Evd.econstr Rocq_utils.kind_pair
    -> t option

  val hty_is_a : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> bool
end

module type HYP_TYPE = sig
  type t

  val hty_is_a : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> bool
  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
  val opt_of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t option
  val hyp_is_a : Proofview.Goal.t -> Rocq_utils.hyp -> bool
end

module type HYP_S = sig
  module HTy : HTY_TYPE

  type t

  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
end

module MakeHyp : (Hyp : HYP_S) -> sig
  type t = Hyp.t

  val hty_is_a : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> bool
  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
  val opt_of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t option
  val hyp_is_a : Proofview.Goal.t -> Rocq_utils.hyp -> bool
end

module Make : (HTy : HTY_S) -> sig
  type t = HTy.t

  val hty_is_a : Proofview.Goal.t -> Evd.econstr Rocq_utils.kind_pair -> bool
  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
  val opt_of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t option
  val hyp_is_a : Proofview.Goal.t -> Rocq_utils.hyp -> bool
end
