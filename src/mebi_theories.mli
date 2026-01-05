val tactics : unit Proofview.tactic list -> unit Proofview.tactic
val constants : Evd.econstr list ref
val find_reference : string -> string list -> string -> Names.GlobRef.t
val collect_bisimilarity_theories : unit -> Evd.econstr list
val indexed_c : int * Evd.econstr list -> Evd.econstr option
val c_LTS : unit -> Evd.econstr
val c_tau : unit -> Evd.econstr
val c_silent : unit -> Evd.econstr
val c_silent1 : unit -> Evd.econstr
val c_weak : unit -> Evd.econstr
val c_wk_some : unit -> Evd.econstr
val c_wk_none : unit -> Evd.econstr
val c_simF : unit -> Evd.econstr
val c_Pack_sim : unit -> Evd.econstr
val c_sim_weak : unit -> Evd.econstr
val c_weak_sim : unit -> Evd.econstr
val c_In_sim : unit -> Evd.econstr
val c_out_sim : unit -> Evd.econstr
val c_weak_bisim : unit -> Evd.econstr
val c_relations : unit -> Evd.econstr
val c_clos_refl_trans_1n : unit -> Evd.econstr
val c_rt1n_refl : unit -> Evd.econstr
val c_rt1n_trans : unit -> Evd.econstr
val c_clos_trans_1n : unit -> Evd.econstr
val c_option : unit -> Evd.econstr
val c_None : unit -> Evd.econstr
val c_Some : unit -> Evd.econstr
val c_ex : unit -> Evd.econstr
val c_ex_intro : unit -> Evd.econstr
val is_constant : Evd.evar_map -> Evd.econstr -> (unit -> Evd.econstr) -> bool
val is_var : Evd.evar_map -> Evd.econstr -> bool
val is_constr : Evd.evar_map -> Evd.econstr -> bool
val is_type : Evd.evar_map -> Evd.econstr -> bool
val get_hyp_names : Proofview.Goal.t -> Names.Id.Set.t
val next_name_of : Names.Id.Set.t -> Names.variable -> Names.variable
val new_name_of_string : Proofview.Goal.t -> string -> Names.variable
val is_cofix : Names.variable -> bool
val new_cofix_name : Proofview.Goal.t -> Names.variable
val new_H_name : Proofview.Goal.t -> Names.variable
val get_proof_from_pstate : Declare.Proof.t -> Proof.t
val get_partial_proof : Proof.t -> Evd.econstr list
