val constants : Evd.econstr list ref

val collect_bisimilarity_theories :
  unit -> Evd.econstr list Mebi_wrapper.mm

val c_LTS : Evd.econstr Mebi_wrapper.mm
val c_tau : Evd.econstr Mebi_wrapper.mm
val c_silent : Evd.econstr Mebi_wrapper.mm
val c_silent1 : Evd.econstr Mebi_wrapper.mm
val c_weak : Evd.econstr Mebi_wrapper.mm
val c_wk_some : Evd.econstr Mebi_wrapper.mm
val c_wk_none : Evd.econstr Mebi_wrapper.mm
val c_simF : Evd.econstr Mebi_wrapper.mm
val c_Pack_sim : Evd.econstr Mebi_wrapper.mm
val c_sim_weak : Evd.econstr Mebi_wrapper.mm
val c_weak_sim : Evd.econstr Mebi_wrapper.mm
val c_In_sim : Evd.econstr Mebi_wrapper.mm
val c_out_sim : Evd.econstr Mebi_wrapper.mm
val c_weak_bisim : Evd.econstr Mebi_wrapper.mm
val c_relations : Evd.econstr Mebi_wrapper.mm
val c_clos_refl_trans_1n : Evd.econstr Mebi_wrapper.mm
val c_rt1n_refl : Evd.econstr Mebi_wrapper.mm
val c_rt1n_trans : Evd.econstr Mebi_wrapper.mm
val c_clos_trans_1n : Evd.econstr Mebi_wrapper.mm
val _package : Names.variable -> unit Proofview.tactic
