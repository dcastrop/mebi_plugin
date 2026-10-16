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

  (** [inversion h] is [inversion h]. Raises nothing; the tactic fails (as a
      tactic) when Rocq's does. *)
  val inversion : Rocq_utils.hyp -> tactic mm

  (** [refute_premise h] closes the goal from [h], a premise hypothesis
      known to be false ({!Premise_search.refute_hyp_tac}; backlog I2).
      Raises nothing; the tactic fails (as a tactic) when [h] cannot be refuted.
  *)
  val refute_premise : Rocq_utils.hyp -> tactic mm

  (** [refute_dead h] closes the goal from [h], an LTS step that
      {!Premise_search.dead} says has no instance, by refuting it; if that
      fails, [inversion h], so a wrong verdict costs one step, never the proof.
      Raises nothing; the tactic fails (as a tactic) when the inversion does. *)
  val refute_dead : Rocq_utils.hyp -> tactic mm

  (** [subst_all ()] is [subst]. Raises nothing. *)
  val subst_all : unit -> tactic mm

  (** [simplify ()] is [simpl in *]. Raises nothing. *)
  val simplify : unit -> tactic mm

  (** [simplify_concl ()] is [simpl] on the conclusion. Raises nothing. *)
  val simplify_concl : unit -> tactic mm

  (** [simplify_hyp h] is [simpl in h]. Raises nothing. *)
  val simplify_hyp : Rocq_utils.hyp -> tactic mm

  (** [simplify_hyps ()] is [simpl] in each hypothesis, in turn. Raises
      nothing. *)
  val simplify_hyps : unit -> tactic mm

  (** [simplify_and_subst_all ()] is [simpl in *; subst]. Raises nothing. *)
  val simplify_and_subst_all : unit -> tactic mm

  (** [reflexivity ()] closes an equation premise goal, up to reduction.
      Raises nothing; the tactic fails (as a tactic) when the sides differ. *)
  val reflexivity : unit -> tactic mm

  (** [exact_term p] closes the goal with the proof term [p]. Raises nothing;
      the tactic fails (as a tactic) when [p] does not have the goal's type. *)
  val exact_term : EConstr.t -> tactic mm

  (** [invert_premise h] is [simpl in h; inversion h; clear h; subst],
      for a premise hypothesis that mentions variables it determines (an
      output, such as a target [m] in [succ_rel n m]; backlog I2, stage 2).
      Raises nothing; the tactic fails (as a tactic) when the inversion does. *)
  val invert_premise : Rocq_utils.hyp -> tactic mm

  (** [prove_negation ()] proves a goal [~ P] whose [P] is refutable
      ({!Premise_search.negation_tac}). Raises nothing; the tactic fails (as a
      tactic) when [P] cannot be refuted. *)
  val prove_negation : unit -> tactic mm

  (** [prove_bounded ()] proves a bounded universal premise ([forall k, k < n -> P k]) instance by instance ({!Premise_search.premise_tac}). Raises
      nothing; the tactic fails (as a tactic) when it does not hold. *)
  val prove_bounded : unit -> tactic mm

  (** [cofix ()] is [cofix] with a fresh hypothesis name ([CofixN]). Raises
      nothing. *)
  val cofix : unit -> tactic mm

  (** [mutual_cofix root others] opens a mutual cofixpoint: [root]'s type is
      taken from the goal, [others] names and types the rest of the block, and
      one goal is produced per definition, each with every hypothesis in
      scope. Must be sequenced with {!all_goals} applying the bisimulation's
      constructors in the same tactic: until one is applied, every goal is
      its own hypothesis, and closing it by one is rejected at [Qed]. Raises
      nothing. *)
  val mutual_cofix : Names.Id.t -> (Names.Id.t * Evd.econstr) list -> tactic mm

  (** [all_goals t] runs [t] on every focused goal rather than only the
      first. Raises nothing. *)
  val all_goals : tactic -> tactic

  (** [trivial ?msg ()] is [trivial] ([info_trivial] when [Info] messages
      are on). Raises nothing. *)
  val trivial : ?msg:string -> unit -> tactic mm

  (** [exact_hyp h] closes the goal with the hypothesis [h] itself, where the
      goal is already known to be [h]'s type (cheaper and more certain than
      [trivial] once many coinduction hypotheses are in scope). Raises nothing;
      the tactic fails (as a tactic) when the types differ. *)
  val exact_hyp : Rocq_utils.hyp -> tactic mm

  (** [ex_intro s] is [exists s], the term of the state [s].

      @raise Decoder.S.CouldNotDecode_State
        if [s] stands for no term
        (propagated). *)
  val ex_intro : state -> tactic mm

  (** [split ()] is [split]. Raises nothing. *)
  val split : unit -> tactic mm

  (** [ex_intro_split s] is {!ex_intro} [s], then {!split}. Raises as
      {!ex_intro}. *)
  val ex_intro_split : state -> tactic mm

  (** [intros_all ()] is [intros]. Raises nothing. *)
  val intros_all : unit -> tactic mm

  (** [apply x] is [apply x]. Raises nothing. *)
  val apply : Evd.econstr -> tactic mm

  (** [apply_Pack_sim ()] is [apply Pack_sim] (from [MEBI.Bisimilarity], as
      the other [apply_]/[eapply_] tactics).

      @raise Failure
        if the theory term is not loaded ({!Mebi_theories.get};
        propagated). *)
  val apply_Pack_sim : unit -> tactic mm

  (** [apply_In_sim ()] is [apply In_sim]. *)
  val apply_In_sim : unit -> tactic mm

  (** [apply_wk_none ()] is [apply wk_none]. *)
  val apply_wk_none : unit -> tactic mm

  (** [apply_rt1n_refl ()] is [apply rt1n_refl]. *)
  val apply_rt1n_refl : unit -> tactic mm

  (** [apply_weak_sim_refl ()] is [apply weak_sim_refl]. *)
  val apply_weak_sim_refl : unit -> tactic mm

  (** [apply_Pack_bisim ()] is [apply Pack_bisim]. *)
  val apply_Pack_bisim : unit -> tactic mm

  (** [apply_In_bisim ()] is [apply In_bisim]. *)
  val apply_In_bisim : unit -> tactic mm

  (** [apply_weak_bisimilar_refl ()] is [apply weak_bisimilar_refl]. *)
  val apply_weak_bisimilar_refl : unit -> tactic mm

  (** [eapply x] is [eapply x]. Raises nothing. *)
  val eapply : Evd.econstr -> tactic mm

  (** [eapply_wk_some ()] is [eapply wk_some]. *)
  val eapply_wk_some : unit -> tactic mm

  (** [eapply_rt1n_refl ()] is [eapply rt1n_refl]. *)
  val eapply_rt1n_refl : unit -> tactic mm

  (** [eapply_rt1n_trans ()] is [eapply rt1n_trans]. *)
  val eapply_rt1n_trans : unit -> tactic mm

  (** [eapply_rt1n_via l] is {!eapply_rt1n_trans} if [l] is silent (a
      silent step extends the [weak] closure), else {!eapply_rt1n_refl}. *)
  val eapply_rt1n_via : label -> tactic mm

  (** Raised by {!unfold_constr}: the term is not a constant. *)
  exception CannotUnfoldConstr of Constr.t

  (** [unfold_constr ?in_hyp x] is [unfold x], and also [unfold x in h] for
      [in_hyp = h].

      @raise CannotUnfoldConstr if [x] is not a constant (raised here). *)
  val unfold_constr : ?in_hyp:Rocq_utils.hyp -> Constr.t -> tactic

  (** [f_unfold_hyp f ?in_hyp x] is [f x], or [f ~in_hyp:h x] for
      [in_hyp = Some h]: so callers can pass an option. Raises whatever [f]
      raises. *)
  val f_unfold_hyp
    :  (?in_hyp:Rocq_utils.hyp -> 'a -> tactic)
    -> ?in_hyp:Rocq_utils.hyp option
    -> 'a
    -> tactic

  (** [unfold_econstr ?in_hyp x] is {!unfold_constr} on [x].

      @raise CannotUnfoldConstr as {!unfold_constr} (propagated). *)
  val unfold_econstr : ?in_hyp:Rocq_utils.hyp -> Evd.econstr -> tactic

  (** [unfold_constrexpr ?in_hyp x] is {!unfold_econstr} on [x] interpreted.

      @raise CannotUnfoldConstr
        as {!unfold_constr} (propagated). Also
        Rocq's errors if [x] is ill-formed. *)
  val unfold_constrexpr
    :  ?in_hyp:Rocq_utils.hyp
    -> Constrexpr.constr_expr
    -> tactic

  (** [unfold_opt_constrexpr_list ?in_hyp xs] is the unfolding of each of
      [xs] that is a constant, chained, or [None] if none is. Raises Rocq's
      errors if one of [xs] is ill-formed (propagated). *)
  val unfold_opt_constrexpr_list
    :  ?in_hyp:Rocq_utils.hyp
    -> Constrexpr.constr_expr list
    -> tactic option

  (** [unfold_silent ()] is [unfold silent]. Raises as {!apply_Pack_sim}. *)
  val unfold_silent : unit -> tactic

  (** [do_refl ()] answers with no step: [apply wk_none; unfold silent; apply rt1n_refl]. Raises as {!apply_Pack_sim}.
  *)
  val do_refl : unit -> tactic mm

  (** [collect_component_econstrs sigma x] is the constants [x] mentions, as
      heads or arguments of its applications (and the scrutinee's head of a
      [match]). Raises nothing. *)
  val collect_component_econstrs : Evd.evar_map -> Evd.econstr -> econstrset

  (** [can_be_unfolded sigma x] is whether [x] is a constant whose definition
      can be unfolded: a function, fixpoint, or an alias of another
      definition (e.g. an example term). Raises nothing. *)
  val can_be_unfolded : Evd.evar_map -> Evd.econstr -> bool

  (** [try_unfold_any ?in_hyp x] is an [unfold] of every unfoldable
      constant in [x] that is not a theory term, chained, or [None]. Raises
      nothing when run. *)
  val try_unfold_any : ?in_hyp:Rocq_utils.hyp -> Evd.econstr -> tactic option mm

  (** [try_unfold_any_of xs] is {!try_unfold_any} of each of [xs], chained,
      or [None]. Raises nothing when run. *)
  val try_unfold_any_of : Evd.econstr list -> tactic option mm

  (** Raised by {!find_lts}: no LTS has that encoding. *)
  exception NoRocqLTSFoundWithEnc of enc

  (** [find_lts enc ls] is the LTS of [ls] with the encoding [enc].

      @raise NoRocqLTSFoundWithEnc if there is none (raised here). *)
  val find_lts : enc -> rocqlts list -> rocqlts

  (** Raised by {!find_constructor}: no constructor has that index. *)
  exception NoConstructorFoundWithIndex of int

  (** [find_constructor i cs] is the constructor of [cs] with the index [i].

      @raise NoConstructorFoundWithIndex if there is none (raised here). *)
  val find_constructor : int -> constructorbindings list -> constructorbindings

  (** The terms of the step a constructor is applied to: its source, and
      its target and label when known. *)
  type binding_args =
    { from : Evd.econstr
    ; goto : Evd.econstr option
    ; label : Evd.econstr option
    }

  (** [get_constructor_bindings args b] is the [with] bindings for a
      constructor with binder paths [b], at the step [args]
      ({!Constructor_bindings.S.get}). Raises as that (propagated). *)
  val get_constructor_bindings
    :  binding_args
    -> bindings
    -> Evd.econstr Tactypes.bindings

  (** [try_get_constructor_bindings (lts, i) args] is
      {!get_constructor_bindings} for constructor [i] of the LTS [lts], or no
      bindings if FSM b has no metadata.

      @raise NoRocqLTSFoundWithEnc as {!find_lts} (propagated).
      @raise NoConstructorFoundWithIndex
        as {!find_constructor} (propagated).
        Also raises as {!get_constructor_bindings}. *)
  val try_get_constructor_bindings
    :  node
    -> binding_args
    -> Evd.econstr Tactypes.bindings

  (** Raised by {!apply_constructor} when the focused goal is not a step of
      the constructor's LTS (its bindings cannot be read off it). *)
  exception GoalNotAnLTSStep

  (** [apply_constructor (lts, i) args] is [econstructor] number [i] (from
      0 in the tree, so [i + 1] for Rocq) with the bindings read off [args],
      after which the premise goals that are not LTS steps are moved last and
      their open witnesses chosen.

      @raise GoalNotAnLTSStep
        if the bindings cannot be read off the goal (raised here). Also raises
        as {!try_get_constructor_bindings}. *)
  val apply_constructor : node -> binding_args -> tactic mm
end

module Make
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
