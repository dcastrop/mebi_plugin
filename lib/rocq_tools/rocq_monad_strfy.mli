module type S = sig
  val constr : Constr.t -> string
  val constr_kind : Constr.t -> string
  val econstr : EConstr.t -> string
  val econstr_kind : EConstr.t -> string
  val econstr_rel_decl : EConstr.rel_declaration -> string
  val hyp_name : Rocq_utils.hyp -> string
  val hyp_type : Rocq_utils.hyp -> string
  val hyp : Rocq_utils.hyp -> string
  val hyp_value : Rocq_utils.hyp -> string
  val econstr_bindings : EConstr.t Tactypes.bindings -> string
end

module Make (M : Rocq_monad.S) : S
