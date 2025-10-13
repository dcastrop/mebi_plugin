val _update_names :
  unit -> unit Proofview.tactic Mebi_wrapper.mm

val the_goals : (int, Proofview.Goal.t) Hashtbl.t ref
val reset_the_goals : unit -> unit
val add_goal : Proofview.Goal.t -> unit

val update_goals :
  unit -> unit Proofview.tactic Mebi_wrapper.mm

val get_the_goals :
  unit ->
  (int, Proofview.Goal.t) Hashtbl.t Proofview.tactic
  Mebi_wrapper.mm

val pstr_the_goals : unit -> string
val goal_test : unit -> unit Proofview.tactic Mebi_wrapper.mm
val apply : EConstr.t -> unit Proofview.tactic

val apply_mm :
  EConstr.t Mebi_wrapper.mm ->
  unit Proofview.tactic Mebi_wrapper.mm

val eapply : EConstr.t -> unit Proofview.tactic

val eapply_mm :
  EConstr.t Mebi_wrapper.mm ->
  unit Proofview.tactic Mebi_wrapper.mm

val unfold_econstr :
  EConstr.t -> unit Proofview.tactic Mebi_wrapper.mm

val unfold_econstr_mm :
  EConstr.t Mebi_wrapper.mm ->
  unit Proofview.tactic Mebi_wrapper.mm

val unfold_constrexpr :
  Constrexpr.constr_expr ->
  unit Proofview.tactic Mebi_wrapper.mm

val unfold_constrexpr_list :
  Constrexpr.constr_expr list ->
  unit Proofview.tactic Mebi_wrapper.mm

val cofix :
  ?name:Names.variable option ->
  unit ->
  unit Proofview.tactic Mebi_wrapper.mm

val intros_all : unit -> unit Proofview.tactic Mebi_wrapper.mm

val intro_of_string :
  ?track:bool ->
  string ->
  unit Proofview.tactic Mebi_wrapper.mm
