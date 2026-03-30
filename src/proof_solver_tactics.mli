module type S = sig
  type 'a mm
  type enc
  type node
  type bindings
  type constructorbindings
  type state
  type label
  type rocqlts
  type tactic
  type econstrset

  val inversion : Rocq_utils.hyp -> tactic mm
  val subst_all : unit -> tactic mm
  val simplify : unit -> tactic mm
  val simplify_concl : unit -> tactic mm
  val simplify_hyp : Rocq_utils.hyp -> tactic mm
  val simplify_hyps : unit -> tactic mm
  val simplify_all : unit -> tactic mm
  val simplify_and_subst_all : unit -> tactic mm
  val cofix : unit -> tactic mm
  val trivial : ?msg:string -> unit -> tactic mm
  val ex_intro : state -> tactic mm
  val split : unit -> tactic mm
  val ex_intro_split : state -> tactic mm
  val intros_all : unit -> tactic mm
  val intro_as : string -> tactic mm
  val apply : Evd.econstr -> tactic mm
  val apply_Pack_sim : unit -> tactic mm
  val apply_In_sim : unit -> tactic mm
  val apply_wk_none : unit -> tactic mm
  val apply_rt1n_refl : unit -> tactic mm
  val apply_rt1n_trans : unit -> tactic mm
  val apply_weak_sim_refl : unit -> tactic mm
  val eapply : Evd.econstr -> tactic mm
  val eapply_wk_some : unit -> tactic mm
  val eapply_rt1n_refl : unit -> tactic mm
  val eapply_rt1n_trans : unit -> tactic mm
  val eapply_rt1n_via : label -> tactic mm

  exception CannotUnfoldConstr of Constr.t

  val unfold_constr : ?in_hyp:Rocq_utils.hyp -> Constr.t -> tactic

  val f_unfold_hyp
    :  (?in_hyp:Rocq_utils.hyp -> 'a -> tactic)
    -> ?in_hyp:Rocq_utils.hyp option
    -> 'a
    -> tactic

  val unfold_econstr : ?in_hyp:Rocq_utils.hyp -> Evd.econstr -> tactic

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
  val do_refl : unit -> tactic mm
  val collect_component_econstrs : Evd.evar_map -> Evd.econstr -> econstrset
  val can_be_unfolded : Evd.evar_map -> Evd.econstr -> bool
  val try_unfold_any : ?in_hyp:Rocq_utils.hyp -> Evd.econstr -> tactic option mm
  val try_unfold_any_of : Evd.econstr list -> tactic option mm

  exception NoRocqLTSFoundWithEnc of enc

  val find_lts : enc -> rocqlts list -> rocqlts

  exception NoConstructorFoundWithIndex of int

  val find_constructor : int -> constructorbindings list -> constructorbindings

  type binding_args =
    { from : Evd.econstr
    ; goto : Evd.econstr option
    ; label : Evd.econstr option
    }

  val get_constructor_bindings
    :  binding_args
    -> bindings
    -> Evd.econstr Tactypes.bindings

  val try_get_constructor_bindings
    :  node
    -> binding_args
    -> Evd.econstr Tactypes.bindings

  val apply_constructor : node -> binding_args -> tactic mm
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Tactic : Proof_solver_tactic.S)
    (W :
       Results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (Iter : Proof_solver_wrapper.S with type enc = Enc.t)
    (Theory :
       Proof_solver_theory.S
       with type 'a mm = 'a W.M.mm
        and type 'a im = 'a Iter.mm
        and type enc = Enc.t
        and type fsm = W.Model.FSM.t) :
  S
  with type 'a mm = 'a Iter.mm
   and type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type bindings = W.Bindings.t
   and type constructorbindings = W.ConstructorBindings.t
   and type state = W.Model.State.t
   and type label = W.Model.Label.t
   and type rocqlts = W.Model.Info.Meta.RocqLTS.t
   and type tactic = Tactic.t
   and type econstrset = Iter.EConstrSet.t
