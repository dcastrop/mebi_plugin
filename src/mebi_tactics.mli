val apply_In_sim :
  unit -> unit Proofview.tactic Mebi_wrapper.mm

val apply_Pack_sim :
  unit -> unit Proofview.tactic Mebi_wrapper.mm

val unfold_econstr :
  EConstr.t -> unit Proofview.tactic Mebi_wrapper.mm

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
  string -> unit Proofview.tactic Mebi_wrapper.mm
