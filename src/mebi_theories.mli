val constants : EConstr.t list ref
val collect_bisimilarity_theories : unit -> EConstr.t list
val c_LTS : unit -> EConstr.t
val c_tau : unit -> EConstr.t
val c_silent : unit -> EConstr.t
val c_silent1 : unit -> EConstr.t
val c_weak : unit -> EConstr.t
val c_wk_some : unit -> EConstr.t
val c_wk_none : unit -> EConstr.t
val c_simF : unit -> EConstr.t
val c_Pack_sim : unit -> EConstr.t
val c_sim_weak : unit -> EConstr.t
val c_weak_sim : unit -> EConstr.t
val c_In_sim : unit -> EConstr.t
val c_out_sim : unit -> EConstr.t
val c_weak_bisim : unit -> EConstr.t
val c_relations : unit -> EConstr.t
val c_clos_refl_trans_1n : unit -> EConstr.t
val c_rt1n_refl : unit -> EConstr.t
val c_rt1n_trans : unit -> EConstr.t
val c_clos_trans_1n : unit -> EConstr.t

type theory_kind =
  | Theories_LTS
  | Theories_tau
  | Theories_silent
  | Theories_silent1
  | Theories_weak
  | Theories_wk_some
  | Theories_wk_none
  | Theories_simF
  | Theories_Pack_sim
  | Theories_sim_weak
  | Theories_weak_sim
  | Theories_In_sim

val match_theory_kind : Evd.evar_map -> Evd.econstr -> theory_kind option
val is_cofix : Names.Id.t -> bool
val to_invert : Evd.evar_map -> EConstr.t array -> bool
