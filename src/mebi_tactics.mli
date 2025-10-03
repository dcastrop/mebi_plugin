val apply : Evd.econstr -> unit Proofview.tactic

val apply_mm :
  Evd.econstr Mebi_wrapper.mm ->
  unit Proofview.tactic Mebi_wrapper.mm

  val eapply : Evd.econstr -> unit Proofview.tactic

val eapply_mm :
  Evd.econstr Mebi_wrapper.mm ->
  unit Proofview.tactic Mebi_wrapper.mm

val unfold_econstr :
  Evd.econstr -> unit Proofview.tactic Mebi_wrapper.mm

val unfold_econstr_mm :
  Evd.econstr Mebi_wrapper.mm -> unit Proofview.tactic Mebi_wrapper.mm

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
