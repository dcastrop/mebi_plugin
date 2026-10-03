(* Structural congruence for [Proc.Layered], made explicit (notes/13,
   2026-10-03).

   [Layered.compLTS] builds structural congruence into the semantics as
   silent steps: [do_comm] and [do_assocl]/[do_assocr] reorder and rebracket
   parallel components, and [do_par_end] collapses two finished ones. Every
   arrangement of the same components is then a state of its own, all
   silently interconvertible: four components in 81 local configurations
   make 81 x 120 = 9720 states ([Bisimilarity/Proc/Test4]), far too many to
   prove anything about.

   Here the congruence is a separate relation, [congr], and a second
   semantics, [nLTS], takes one canonical representative per class ([norm]:
   the unfinished components, sorted, rebuilt right-nested) as every step's
   target. From the same four components it has 82 states. [link] proves the
   two semantics weakly bisimilar on congruent terms, once and for all terms,
   so a [weak_sim] or [weak_bisimilar] proved by the plugin over [nLTS]
   transfers to [compLTS] ([wsim_transfer], [wbis_transfer]).

   Note what makes the difference: a canonical representative, not merely
   explicit congruence rules. Used as a premise ([p == p' -> p' -a-> q' ->
   q' == q -> p -a-> q]), the rules would let the plugin enumerate every
   congruent target, and the 9720 states would come back. *)

From Stdlib Require Import List Permutation Bool Relations.Relation_Operators.
Import ListNotations.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Layered.

Module Normalised.

  (****************************************************************************)
  (** Structural congruence, explicitly *)

  Inductive congr : comp -> comp -> Prop :=
  | cg_refl : forall c, congr c c
  | cg_sym : forall c d, congr c d -> congr d c
  | cg_trans : forall c d e, congr c d -> congr d e -> congr c e
  | cg_comm : forall l r, congr (cpar l r) (cpar r l)
  | cg_assoc : forall x y z, congr (cpar x (cpar y z)) (cpar (cpar x y) z)
  | cg_unit : forall x, congr (cpar (cprc tend) x) x
  | cg_parl : forall l l' r, congr l l' -> congr (cpar l r) (cpar l' r)
  | cg_parr : forall l r r', congr r r' -> congr (cpar l r) (cpar l r').

  (****************************************************************************)
  (** Canonical representatives *)

  Fixpoint flat (c : comp) : list term :=
    match c with cprc t => [t] | cpar l r => flat l ++ flat r end.

  Definition nonend (t : term) : bool := match t with tend => false | _ => true end.

  (* the components still to run: what a state is, up to congruence *)
  Definition comps (c : comp) : list term := filter nonend (flat c).

  (* a total order on terms, by a code; only used to sort *)
  Definition lcode (l : label) : nat := match l with A => 0 | B => 1 | C => 2 end.
  Definition acode (a : action) : nat :=
    match a with send l => lcode l | recv l => 3 + lcode l end.
  Fixpoint code (t : term) : list nat :=
    match t with
    | trec => [0] | tend => [1]
    | tfix t => 2 :: code t ++ [9]
    | tact a t => 3 :: acode a :: code t ++ [9]
    | tseq t s => 4 :: code t ++ 8 :: code s ++ [9]
    end.
  Fixpoint lex (a b : list nat) : bool :=
    match a, b with
    | [], _ => true
    | _ :: _, [] => false
    | x :: a', y :: b' =>
        if Nat.ltb x y then true else if Nat.ltb y x then false else lex a' b'
    end.
  Fixpoint insert (t : term) (l : list term) : list term :=
    match l with
    | [] => [t]
    | u :: l' => if lex (code t) (code u) then t :: l else u :: insert t l'
    end.
  Fixpoint sort (l : list term) : list term :=
    match l with [] => [] | t :: l' => insert t (sort l') end.

  Fixpoint build (l : list term) : comp :=
    match l with
    | [] => cprc tend
    | [t] => cprc t
    | t :: l' => cpar (cprc t) (build l')
    end.

  Definition norm (c : comp) : comp := build (sort (comps c)).

  (****************************************************************************)
  (** The normalised semantics *)

  (* [compLTS] without the congruence rules ([do_comm], [do_assoc*],
     [do_par_end]) *)
  Inductive core : comp -> option label -> comp -> Prop :=
  | n_t : forall t t' a, termLTS t a t' -> core (cprc t) a (cprc t')
  | n_parl : forall l l' r a, core l a l' -> core (cpar l r) a (cpar l' r)
  | n_parr : forall l r r' a, core r a r' -> core (cpar l r) a (cpar l r').

  (* ... with every target normalised *)
  Inductive nLTS : comp -> option label -> comp -> Prop :=
  | n_step : forall p a q, core p a q -> nLTS p a (norm q).

  (****************************************************************************)
  (** Lemmas *)

  Lemma insert_perm : forall t l, Permutation (insert t l) (t :: l).
  Proof.
    intros t l; induction l as [|u l IH]; simpl; [reflexivity|].
    destruct (lex (code t) (code u)); [reflexivity|].
    eapply perm_trans; [apply perm_skip, IH | apply perm_swap].
  Qed.

  Lemma sort_perm : forall l, Permutation (sort l) l.
  Proof.
    induction l as [|t l IH]; simpl; [reflexivity|].
    eapply perm_trans; [apply insert_perm | apply perm_skip, IH].
  Qed.

  Lemma comps_build : forall l, Forall (fun t => nonend t = true) l ->
    comps (build l) = l.
  Proof.
    induction l as [|t l IH]; intros F; [reflexivity|].
    inversion F as [|? ? Nt Fl]; subst.
    destruct l as [|u l].
    - unfold comps; simpl; rewrite Nt; reflexivity.
    - change (comps (cpar (cprc t) (build (u :: l))) = t :: u :: l).
      specialize (IH Fl); unfold comps in *; simpl; rewrite Nt.
      f_equal; exact IH.
  Qed.

  Lemma comps_nonend : forall c, Forall (fun t => nonend t = true) (comps c).
  Proof.
    intros c; apply Forall_forall; intros t I.
    apply filter_In in I as [_ N]; exact N.
  Qed.

  Lemma comps_norm : forall c, Permutation (comps (norm c)) (comps c).
  Proof.
    intros c; unfold norm.
    rewrite comps_build; [apply sort_perm|].
    apply (Permutation_Forall (Permutation_sym (sort_perm _))), comps_nonend.
  Qed.

  Lemma nonend_step : forall t a t', termLTS t a t' -> nonend t = true.
  Proof. intros t a t' T; destruct t; [reflexivity | inversion T | ..]; reflexivity. Qed.

  Lemma core_comp : forall c a c', core c a c' -> compLTS c a c'.
  Proof. induction 1; constructor; assumption. Qed.

  (* A [core] step moves exactly one component. *)
  Lemma core_split : forall c a c', core c a c' ->
    exists l1 l2 t t', flat c = l1 ++ t :: l2 /\ flat c' = l1 ++ t' :: l2
                       /\ termLTS t a t'.
  Proof.
    induction 1 as [t t' a T | l l' r a _ IH | l r r' a _ IH].
    - exists [], [], t, t'; auto.
    - destruct IH as [l1 [l2 [t [t' [E [E' T]]]]]].
      exists l1, (l2 ++ flat r), t, t'; simpl; rewrite E, E'.
      repeat rewrite <- app_assoc; auto.
    - destruct IH as [l1 [l2 [t [t' [E [E' T]]]]]].
      exists (flat l ++ l1), l2, t, t'; simpl; rewrite E, E'.
      repeat rewrite <- app_assoc; auto.
  Qed.

  (* ... and any component can be moved, wherever it sits. *)
  Lemma core_at : forall d l1 t l2 t' a, flat d = l1 ++ t :: l2 ->
    termLTS t a t' -> exists d', core d a d' /\ flat d' = l1 ++ t' :: l2.
  Proof.
    induction d as [u | l IHl r IHr]; intros l1 t l2 t' a E T.
    - simpl in E. destruct l1 as [|x l1].
      + inversion E; subst. exists (cprc t'); split; [constructor; exact T | reflexivity].
      + inversion E as [[Hx Hnil]]. destruct l1; discriminate.
    - simpl in E. apply app_eq_app in E as [m [[El Er] | [El Er]]].
      + (* [t] in [r], or the head of [r] *)
        destruct m as [|x m].
        * rewrite app_nil_r in El; subst.
          destruct (IHr [] t l2 t' a (eq_sym Er) T) as [r' [C F]].
          exists (cpar l r'); split; [constructor; exact C|].
          simpl; rewrite F; reflexivity.
        * inversion Er as [[Hx Hl2]]; subst.
          destruct (IHl l1 x m t' a El T) as [l' [C F]].
          exists (cpar l' r); split; [constructor; exact C|].
          simpl; rewrite F, <- app_assoc; reflexivity.
      + (* [t] in [r] *)
        destruct (IHr m t l2 t' a Er T) as [r' [C F]].
        exists (cpar l r'); split; [constructor; exact C|].
        simpl; rewrite F, El, <- app_assoc; reflexivity.
  Qed.

  (* A [compLTS] step moves one component, or is a congruence step: silent,
     and leaving the components as they were, up to order. *)
  Lemma comp_cases : forall c a c', compLTS c a c' ->
    (exists l1 l2 t t', flat c = l1 ++ t :: l2 /\ flat c' = l1 ++ t' :: l2
                        /\ termLTS t a t')
    \/ (a = None /\ Permutation (comps c) (comps c')).
  Proof.
    induction 1 as [t t' a T | l l' r a _ IH | l r r' a _ IH | | l r | x y z | x y z].
    - left; exists [], [], t, t'; auto.
    - destruct IH as [[l1 [l2 [t [t' [E [E' T]]]]]] | [Ha P]].
      + left; exists l1, (l2 ++ flat r), t, t'; simpl; rewrite E, E'.
        repeat rewrite <- app_assoc; auto.
      + right; split; [exact Ha|]. unfold comps in *; simpl.
        repeat rewrite filter_app; apply Permutation_app_tail, P.
    - destruct IH as [[l1 [l2 [t [t' [E [E' T]]]]]] | [Ha P]].
      + left; exists (flat l ++ l1), l2, t, t'; simpl; rewrite E, E'.
        repeat rewrite <- app_assoc; auto.
      + right; split; [exact Ha|]. unfold comps in *; simpl.
        repeat rewrite filter_app; apply Permutation_app_head, P.
    - right; split; [reflexivity|]. reflexivity.
    - right; split; [reflexivity|]. unfold comps; simpl.
      repeat rewrite filter_app; apply Permutation_app_comm.
    - right; split; [reflexivity|]. unfold comps; simpl.
      rewrite app_assoc; reflexivity.
    - right; split; [reflexivity|]. unfold comps; simpl.
      rewrite app_assoc; reflexivity.
  Qed.

  (* Replacing one unfinished component on both sides keeps them permutations
     of each other. *)
  Lemma perm_replace : forall l1 l2 m1 m2 t t', nonend t = true ->
    Permutation (filter nonend (l1 ++ t :: l2)) (filter nonend (m1 ++ t :: m2)) ->
    Permutation (filter nonend (l1 ++ t' :: l2)) (filter nonend (m1 ++ t' :: m2)).
  Proof.
    intros l1 l2 m1 m2 t t' N P.
    repeat rewrite filter_app in *; simpl in *; rewrite N in P.
    apply Permutation_app_inv in P.
    destruct (nonend t'); [apply Permutation_elt|]; exact P.
  Qed.

  (* a moving component of [c] is a component of [d] *)
  Lemma find_in : forall c d l1 l2 t a t', flat c = l1 ++ t :: l2 ->
    termLTS t a t' -> Permutation (comps c) (comps d) ->
    exists m1 m2, flat d = m1 ++ t :: m2.
  Proof.
    intros c d l1 l2 t a t' E T P.
    assert (I : In t (comps c)).
    { unfold comps; apply filter_In; split; [rewrite E; apply in_elt | eapply nonend_step; eauto]. }
    apply (Permutation_in _ P), filter_In in I as [I _].
    apply in_split in I as [m1 [m2 F]]; exists m1, m2; exact F.
  Qed.

  (****************************************************************************)
  (** The two semantics agree, up to congruence *)

  Lemma link : forall c d, Permutation (comps c) (comps d) ->
    weak_bisimilar compLTS nLTS c d.
  Proof.
    cofix CH; intros c d P; constructor; constructor.
    - intros c' a T.
      destruct (comp_cases _ _ _ T) as [[l1 [l2 [t [t' [E [E' Tt]]]]]] | [Ha P']].
      + destruct (find_in c d l1 l2 t a t' E Tt P) as [m1 [m2 F]].
        destruct (core_at d m1 t m2 t' a F Tt) as [d'' [C F'']].
        exists (norm d''); split.
        * apply inject_weak; constructor; exact C.
        * apply CH.
          eapply Permutation_trans; [| apply Permutation_sym, comps_norm].
          unfold comps in *; rewrite E', F''; rewrite E, F in P.
          eapply perm_replace; [eapply nonend_step; eauto | exact P].
      + subst a; exists d; split.
        * apply wk_none; apply rt1n_refl.
        * apply CH; eapply Permutation_trans; [apply Permutation_sym, P' | exact P].
    - intros e a T; inversion T as [d0 a0 d'' C]; subst.
      destruct (core_split _ _ _ C) as [m1 [m2 [t [t' [F [F'' Tt]]]]]].
      destruct (find_in d c m1 m2 t a t' F Tt (Permutation_sym P)) as [l1 [l2 E]].
      destruct (core_at c l1 t l2 t' a E Tt) as [c'' [Cc E'']].
      exists c''; split.
      + apply inject_weak, core_comp, Cc.
      + apply CH.
        eapply Permutation_trans; [| apply Permutation_sym, comps_norm].
        unfold comps in *; rewrite E'', F''; rewrite E, F in P.
        eapply perm_replace; [eapply nonend_step; eauto | exact P].
  Qed.

  Lemma congr_comps : forall c d, congr c d -> Permutation (comps c) (comps d).
  Proof.
    induction 1; unfold comps in *; simpl;
      repeat rewrite filter_app in *; simpl in *.
    - reflexivity.
    - apply Permutation_sym; assumption.
    - eapply Permutation_trans; eassumption.
    - apply Permutation_app_comm.
    - rewrite app_assoc; reflexivity.
    - reflexivity.
    - apply Permutation_app_tail; assumption.
    - apply Permutation_app_head; assumption.
  Qed.

  Theorem congr_link : forall c d, congr c d -> weak_bisimilar compLTS nLTS c d.
  Proof. intros c d H; apply link, congr_comps, H. Qed.

  (* What the plugin proves over [nLTS] holds over [compLTS]. *)
  Theorem wsim_transfer : forall p q,
    weak_sim nLTS nLTS p q -> weak_sim compLTS compLTS p q.
  Proof.
    intros p q H.
    apply (weak_sim_trans compLTS nLTS compLTS p p q).
    - apply weak_bisimilar_sim, link; reflexivity.
    - apply (weak_sim_trans nLTS nLTS compLTS p q q H).
      apply weak_bisimilar_sim, weak_bisimilar_sym, link; reflexivity.
  Qed.

  Theorem wbis_transfer : forall p q,
    weak_bisimilar nLTS nLTS p q -> weak_bisimilar compLTS compLTS p q.
  Proof.
    intros p q H.
    apply (weak_bisimilar_trans compLTS nLTS compLTS p p q).
    - apply link; reflexivity.
    - apply (weak_bisimilar_trans nLTS nLTS compLTS p q q H).
      apply weak_bisimilar_sym, link; reflexivity.
  Qed.

End Normalised.
