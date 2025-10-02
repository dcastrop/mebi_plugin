
val constants : EConstr.t list ref

val collect_bisimilarity_theories :
  unit -> EConstr.t list Mebi_wrapper.mm

val c_LTS : unit -> EConstr.t Mebi_wrapper.mm
val c_tau : unit -> EConstr.t Mebi_wrapper.mm
val c_silent : unit -> EConstr.t Mebi_wrapper.mm
val c_silent1 : unit -> EConstr.t Mebi_wrapper.mm
val c_weak : unit -> EConstr.t Mebi_wrapper.mm
val c_wk_some : unit -> EConstr.t Mebi_wrapper.mm
val c_wk_none : unit -> EConstr.t Mebi_wrapper.mm
val c_simF : unit -> EConstr.t Mebi_wrapper.mm
val c_Pack_sim : unit -> EConstr.t Mebi_wrapper.mm
val c_sim_weak : unit -> EConstr.t Mebi_wrapper.mm
val c_weak_sim : unit -> EConstr.t Mebi_wrapper.mm
val c_In_sim : unit -> EConstr.t Mebi_wrapper.mm
val c_out_sim : unit -> EConstr.t Mebi_wrapper.mm
val c_weak_bisim : unit -> EConstr.t Mebi_wrapper.mm
val _package : Names.variable -> unit Proofview.tactic
