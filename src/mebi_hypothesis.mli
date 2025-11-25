type atomic_pair = Evd.econstr * Evd.econstr array

exception
  Mebi_proof_HypIsNot_Atomic of
    (Evd.evar_map * Rocq_utils.hyp * EConstr.kind_of_type)

val hyp_to_atomic : Evd.evar_map -> Rocq_utils.hyp -> atomic_pair

exception
  Mebi_proof_Hypothesis_Hyp of (Evd.evar_map * Rocq_utils.hyp * atomic_pair)

exception Mebi_proof_Hypothesis_HTy of (Evd.evar_map * atomic_pair)

module type HTY_TYPE = sig
  type t

  val of_hty : Evd.evar_map -> atomic_pair -> t
  val opt_of_hty : Evd.evar_map -> atomic_pair -> t option
  val hty_is_a : Evd.evar_map -> atomic_pair -> bool
end

module type HTY_S = sig
  type t

  val of_hty : Evd.evar_map -> atomic_pair -> t
end

module MakeHTy : (_ : HTY_S) -> HTY_TYPE

module type HYP_TYPE = sig
  type t

  val hty_is_a : Evd.evar_map -> atomic_pair -> bool
  val of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t
  val opt_of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t option
  val hyp_is_a : Evd.evar_map -> Rocq_utils.hyp -> bool
end

module type HYP_S = sig
  module HTy : HTY_TYPE

  type t

  val of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t
end

module MakeHyp : (_ : HYP_S) -> HYP_TYPE
module Make : (_ : HTY_S) -> HYP_TYPE
