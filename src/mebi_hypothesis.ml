(** We abstract into modules [Cofix], [Invertible] and [TransOpt] the hypothesis relevant to us.
*)

exception Mebi_proof_Hypothesis_Hyp of (Rocq_utils.hyp * Rocq_utils.kind_pair)
exception Mebi_proof_Hypothesis_HTy of Rocq_utils.kind_pair

module type HTY_TYPE = sig
  type t

  val of_hty : Proofview.Goal.t -> Rocq_utils.kind_pair -> t
  val opt_of_hty : Proofview.Goal.t -> Rocq_utils.kind_pair -> t option
  val hty_is_a : Proofview.Goal.t -> Rocq_utils.kind_pair -> bool
end

module type HTY_S = sig
  type t

  val of_hty : Proofview.Goal.t -> Rocq_utils.kind_pair -> t
end

module MakeHTy (HTy : HTY_S) : HTY_TYPE = struct
  include HTy

  let opt_of_hty (gl : Proofview.Goal.t) (p : Rocq_utils.kind_pair)
    : HTy.t option
    =
    try Some (of_hty gl p) with Mebi_proof_Hypothesis_HTy _ -> None
  ;;

  let hty_is_a (gl : Proofview.Goal.t) (p : Rocq_utils.kind_pair) : bool =
    Option.has_some (opt_of_hty gl p)
  ;;
end

module type HYP_TYPE = sig
  type t

  (* val of_hty : Evd.evar_map -> Rocq_utils.kind_pair -> t *)
  (* val opt_of_hty : Evd.evar_map -> Rocq_utils.kind_pair -> t option *)
  val hty_is_a : Proofview.Goal.t -> Rocq_utils.kind_pair -> bool
  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
  val opt_of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t option
  val hyp_is_a : Proofview.Goal.t -> Rocq_utils.hyp -> bool
end

module type HYP_S = sig
  module HTy : HTY_TYPE

  type t

  val of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t
end

module MakeHyp (Hyp : HYP_S) : HYP_TYPE = struct
  type t = Hyp.t

  let hty_is_a : Proofview.Goal.t -> Rocq_utils.kind_pair -> bool =
    Hyp.HTy.hty_is_a
  ;;

  let of_hyp : Proofview.Goal.t -> Rocq_utils.hyp -> t = Hyp.of_hyp

  let opt_of_hyp (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : t option =
    try Some (of_hyp gl h) with Mebi_proof_Hypothesis_Hyp _ -> None
  ;;

  let hyp_is_a (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : bool =
    Option.has_some (opt_of_hyp gl h)
  ;;
end

module Make (HTy : HTY_S) : HYP_TYPE = MakeHyp (struct
    module HTy : HTY_TYPE = MakeHTy (HTy)

    type t = HTy.t

    let of_hyp (gl : Proofview.Goal.t) (h : Rocq_utils.hyp) : t =
      let sigma : Evd.evar_map = Proofview.Goal.sigma gl in
      try HTy.of_hty gl (Rocq_utils.hyp_to_atomic sigma h) with
      | Mebi_proof_Hypothesis_HTy p -> raise (Mebi_proof_Hypothesis_Hyp (h, p))
    ;;
  end)
