type hyp =
  ( EConstr.t,
    EConstr.t,
    Evd.erelevance )
  Context.Named.Declaration.pt

val constants : EConstr.t list ref

val find_reference :
  string -> string list -> string -> Names.GlobRef.t

val collect_bisimilarity_theories : unit -> EConstr.t list
val indexed_c : int * EConstr.t list -> EConstr.t option
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
val c_option : unit -> EConstr.t
val c_None : unit -> EConstr.t
val c_Some : unit -> EConstr.t
val c_ex : unit -> EConstr.t
val c_ex_intro : unit -> EConstr.t

val is_constant :
  Evd.evar_map -> EConstr.t -> (unit -> EConstr.t) -> bool

val is_var : Evd.evar_map -> EConstr.t -> bool
val get_hyp_names : Proofview.Goal.t -> Names.Id.Set.t

val next_name_of :
  Names.Id.Set.t -> Names.variable -> Names.variable

val new_name_of_string :
  Proofview.Goal.t -> string -> Names.variable

val is_cofix : Names.variable -> bool
val new_cofix_name : Proofview.Goal.t -> Names.variable
val new_H_name : Proofview.Goal.t -> Names.variable
val get_proof_from_pstate : Declare.Proof.t -> Proof.t
val get_partial_proof : Proof.t -> EConstr.t list
