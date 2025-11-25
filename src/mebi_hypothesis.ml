(** We abstract into modules [Cofix], [Invertible] and [TransOpt] the hypothesis relevant to us.
*)

(** [atomic_pair] are the arguments of [AtomicType (ty, tys)] returned by [EConstr.kind_of_type]
*)
type atomic_pair = EConstr.t * EConstr.t array

exception
  Mebi_proof_HypIsNot_Atomic of
    (Evd.evar_map * Rocq_utils.hyp * EConstr.kind_of_type)

let hyp_to_atomic (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : atomic_pair =
  let h_ty : EConstr.t = Context.Named.Declaration.get_type h in
  match EConstr.kind_of_type sigma h_ty with
  | AtomicType (ty, tys) -> ty, tys
  | k -> raise (Mebi_proof_HypIsNot_Atomic (sigma, h, k))
;;

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

module MakeHTy (HTy : HTY_S) : HTY_TYPE = struct
  include HTy

  let opt_of_hty (sigma : Evd.evar_map) (p : atomic_pair) : HTy.t option =
    try Some (of_hty sigma p) with Mebi_proof_Hypothesis_HTy _ -> None
  ;;

  let hty_is_a (sigma : Evd.evar_map) (p : atomic_pair) : bool =
    Option.has_some (opt_of_hty sigma p)
  ;;
end

module type HYP_TYPE = sig
  type t

  (* val of_hty : Evd.evar_map -> atomic_pair -> t *)
  (* val opt_of_hty : Evd.evar_map -> atomic_pair -> t option *)
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

module MakeHyp (Hyp : HYP_S) : HYP_TYPE = struct
  type t = Hyp.t

  let hty_is_a : Evd.evar_map -> atomic_pair -> bool = Hyp.HTy.hty_is_a
  let of_hyp : Evd.evar_map -> Rocq_utils.hyp -> t = Hyp.of_hyp

  let opt_of_hyp (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : t option =
    try Some (of_hyp sigma h) with Mebi_proof_Hypothesis_Hyp _ -> None
  ;;

  let hyp_is_a (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : bool =
    Option.has_some (opt_of_hyp sigma h)
  ;;
end

module Make (HTy : HTY_S) : HYP_TYPE = MakeHyp (struct
    module HTy : HTY_TYPE = MakeHTy (HTy)

    type t = HTy.t

    let of_hyp (sigma : Evd.evar_map) (h : Rocq_utils.hyp) : t =
      try HTy.of_hty sigma (hyp_to_atomic sigma h) with
      | Mebi_proof_Hypothesis_HTy (sigma, p) ->
        raise (Mebi_proof_Hypothesis_Hyp (sigma, h, p))
    ;;
  end)
