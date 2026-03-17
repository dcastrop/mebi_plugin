exception NothingToDo

module type S = sig
  type tactic

  val gl : unit -> Proofview.Goal.t
  val get_concl : unit -> EConstr.t
  val get_hyps : unit -> Rocq_utils.hyp list
  val get_hyp_name : Rocq_utils.hyp -> Names.Id.t
  val get_hyp_names : unit -> Names.Id.Set.t
  val next_name_of : Names.Id.Set.t -> Names.Id.t -> Names.Id.t
  val new_name_of_string : string -> Names.Id.t
  val new_cofix_name : unit -> Names.Id.t
  val new_H_name : unit -> Names.Id.t
  val get_all_cofix_hyp_names : unit -> Names.Id.Set.t
  val get_all_non_cofix_hyp_names : unit -> Names.Id.Set.t

  include Rocq_monad_utils.S

  val log_concl : unit -> unit
  val log_hyps : unit -> unit

  module Theory :
    Proof_solver_theory.S with type enc = enc and type 'a im = 'a mm

  module EConstrSet : sig
    include Set.S with type elt = EConstr.t
  end

  module Tacs : sig
    val inversion : Rocq_utils.hyp -> tactic mm
    val subst_all : unit -> tactic mm
    val simplify_concl : unit -> tactic mm
    val simplify_hyp : Rocq_utils.hyp -> tactic mm
    val simplify_hyps : unit -> tactic mm
    val simplify_all : unit -> tactic mm
    val simplify_and_subst_all : unit -> tactic mm
    val cofix : unit -> tactic mm
    val trivial : ?msg:string -> unit -> tactic mm
    val intros_all : unit -> tactic mm
    val intro_as : string -> tactic mm
    val apply : EConstr.t -> tactic mm
    val apply_Pack_sim : unit -> tactic mm
    val apply_In_sim : unit -> tactic mm
    val apply_wk_none : unit -> tactic mm
    val apply_rt1n_refl : unit -> tactic mm
    val apply_rt1n_trans : unit -> tactic mm
    val eapply : EConstr.t -> tactic mm
    val eapply_wk_some : unit -> tactic mm
    val eapply_rt1n_refl : unit -> tactic mm
    val eapply_rt1n_trans : unit -> tactic mm

    exception CannotUnfoldConstr of Constr.t

    val unfold_constr : ?in_hyp:Rocq_utils.hyp -> Constr.t -> tactic
    val unfold_econstr : ?in_hyp:Rocq_utils.hyp -> EConstr.t -> tactic

    val unfold_constrexpr
      :  ?in_hyp:Rocq_utils.hyp
      -> Constrexpr.constr_expr
      -> tactic

    val unfold_opt_constrexpr_list
      :  ?in_hyp:Rocq_utils.hyp
      -> Constrexpr.constr_expr list
      -> tactic option

    val unfold_silent : unit -> tactic
    val unfold_silent1 : unit -> tactic
  end

  val step : unit -> tactic
end

module type Args = sig
  val gl : Proofview.Goal.t ref
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Tactic : Proof_solver_tactic.S)
    (W :
       Wrapper_results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (ProofState :
       Proof_solver_statem.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type state = W.Model.State.t
        and type label = W.Model.Label.t
        and type annotation = W.Model.Annotation.t
        and type transition = W.Model.Transition.t)
    (TheoryMaker : (I : Rocq_monad_utils.S
                        with type enc = Enc.t
                         and type tree = Enc.Tree.t) ->
       Proof_solver_theory.S
       with type 'a mm = 'a W.M.mm
        and type 'a im = 'a I.mm
        and type enc = Enc.t
        and type fsm = W.Model.FSM.t)
    (X : Args) : S with type tactic = Tactic.t
