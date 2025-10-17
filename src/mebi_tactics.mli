val update_proof_by_tactic :
  Declare.Proof.t -> unit Proofview.tactic -> Declare.Proof.t

val update_proof_by_tactics :
  Declare.Proof.t ->
  unit Proofview.tactic list ->
  Declare.Proof.t

val do_inversion : Mebi_theories.hyp -> unit Proofview.tactic
val subst_all : unit -> unit Proofview.tactic
val simplify_all : unit -> unit Proofview.tactic
val simplify_and_subst_all : unit -> unit Proofview.tactic
val the_goals : (int, Proofview.Goal.t) Hashtbl.t ref
val reset_the_goals : unit -> unit
val add_goal : Proofview.Goal.t -> unit
val goal_test : unit -> unit Proofview.tactic
val apply : EConstr.t -> unit Proofview.tactic
val eapply : EConstr.t -> unit Proofview.tactic

val unfold_econstr :
  Proofview.Goal.t -> EConstr.t -> unit Proofview.tactic

val unfold_constrexpr :
  Proofview.Goal.t ->
  Constrexpr.constr_expr ->
  unit Proofview.tactic

val unfold_constrexpr_list :
  Proofview.Goal.t ->
  Constrexpr.constr_expr list ->
  unit Proofview.tactic

val cofix : Proofview.Goal.t -> unit Proofview.tactic
val intros_all : unit -> unit Proofview.tactic

val intro_of_string :
  Proofview.Goal.t -> string -> unit Proofview.tactic
