(* https://rocq-prover.org/doc/v8.9/stdlib/Coq.Relations.Relation_Operators.html *)
From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.


Set Primitive Projections.

Section Definitions.
  Context (M : Type) (A : Type).
  Definition LTS : Type := M -> option A -> M -> Prop.
  (* tau-labelled transition *)
  Definition tau (R : LTS) : relation M := fun x y => R x None y.
End Definitions.
Arguments tau {M A} R.

Hint Constructors clos_refl_trans_1n clos_trans_1n : rel_db.
Lemma clos_t_clos_rt {A : Type} (R : relation A) :
      forall x y, clos_trans_1n A R x y -> clos_refl_trans_1n A R x y.
Proof. intros x y H; induction H; eauto with rel_db. Qed.

Lemma clos_rt_clos_t {A : Type} {R : relation A} :
      forall {x y z}, R x y -> clos_refl_trans_1n A R y z ->
                    clos_trans_1n A R x z.
Proof. intros x y z Rxy H. revert x Rxy. induction H; eauto with rel_db. Qed.

Lemma clos_rt_trans {A : Type} {R : relation A} : forall {x y z},
    clos_refl_trans_1n A R x y -> clos_refl_trans_1n A R y z ->
    clos_refl_trans_1n A R x z.
Proof.
  intros. revert z H0. induction H; eauto.
  intros; eapply rt1n_trans; eauto.
Qed.

Hint Resolve clos_t_clos_rt clos_rt_clos_t clos_rt_trans : rel_db.

Section WeakTrans.
  Context {M : Type} {A : Type} (lts : LTS M A).

  (* trace of tau-labelled transitions *)
  Definition silent : relation M := clos_refl_trans_1n M (tau lts).
  Definition silent1 : relation M := clos_trans_1n M (tau lts).

  (*  x ==> pre_str ->^a post_str ==> y *)
  Inductive weak (x : M) (y : M) : option A -> Prop :=
  | wk_some : forall a z t, silent x z -> lts z (Some a) t -> silent t y -> 
                            weak x y (Some a)
  | wk_none : silent x y -> weak x y None.
End WeakTrans.
Hint Constructors weak : rel_db.
Hint Unfold silent silent1 : rel_db.

Lemma inject_weak : forall {M : Type} {A : Type} {lts : LTS M A} m a n,
    lts m a n -> weak lts m n a.
Proof. destruct a; eauto with rel_db. Qed.
Hint Resolve inject_weak : rel_db.

Section WeakSim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Record simF G m1 n1 :=
    Pack_sim
      { sim_weak : forall {m2 a},
          ltsM m1 a m2 -> exists n2, weak ltsN n1 n2 a /\ G m2 n2
      }.

  CoInductive weak_sim (s : M) (t : N) : Prop
    := In_sim { out_sim :  simF weak_sim s t }.
End WeakSim.
Arguments Pack_sim {M N A}%_type_scope & {ltsM ltsN G}%_function_scope
  {m1 n1} (sim_weak)%_function_scope.
Arguments sim_weak {M N A}%_type_scope & {ltsM ltsN G%_function_scope m1 n1} s
  {m2 a} _.
Arguments out_sim {M N A}%_type_scope & {ltsM ltsN s t} w.
Hint Constructors weak_sim simF : rel_db.
Hint Resolve sim_weak : rel_db.

Lemma weak_sim_refl {M A} (lts : LTS M A) : forall x, weak_sim lts lts x x.
Proof. cofix CH; repeat constructor; eauto with rel_db. Qed.
Hint Resolve weak_sim_refl : rel_db.

Lemma weak_sim_silent_clos : forall {M N A ltsM ltsN m1 n1},
    @weak_sim M N A ltsM ltsN m1 n1 ->
    forall {m2}, silent ltsM m1 m2 ->
                 exists n2, silent ltsN n1 n2 /\ weak_sim ltsM ltsN m2 n2.
Proof.
  intros; revert n1 H. induction H0 as [|????? Ih]; eauto with rel_db.
  intros; destruct (sim_weak (out_sim H1) H) as [?[W Ws]].
  apply Ih in Ws as [?[??]]; inversion W; eauto with rel_db.
Qed.
(* Hint Resolve weak_sim_silent_clos : rel_db. *)

Lemma weak_sim_act_clos : forall {M N A ltsM ltsN m1 n1},
    @weak_sim M N A ltsM ltsN m1 n1 ->
    forall {m2 a}, weak ltsM m1 m2 a ->
                 exists n2, weak ltsN n1 n2 a /\ weak_sim ltsM ltsN m2 n2.
Proof.
  intros. destruct H0 as [??? PRE ACT POST|TAUs].
  - destruct (weak_sim_silent_clos H PRE) as [?[? W1]].
    destruct (sim_weak (out_sim W1) ACT) as [?[Wk W2]].
    destruct (weak_sim_silent_clos W2 POST) as [?[]].
    inversion Wk; eauto 10 with rel_db.
  - destruct (weak_sim_silent_clos H TAUs) as [?[]].
    eauto with rel_db.
Qed.
(* Hint Resolve weak_sim_act_clos : rel_db. *)

Lemma weak_sim_trans {M N R A}
  (ltsM : LTS M A) (ltsN : LTS N A) (ltsR : LTS R A)
  : forall x y r, weak_sim ltsM ltsN x y -> weak_sim ltsN ltsR y r ->
                  weak_sim ltsM ltsR x r.
Proof.
  cofix CH; repeat constructor; intros.
  destruct (sim_weak (out_sim H) H1) as [?[??]].
  destruct (weak_sim_act_clos H0 H2) as [?[??]]; eauto.
Qed.
Hint Resolve weak_sim_trans : rel_db.

(* Proofs up to silent steps (notes/13, option 4; added 2026-10-03, nothing
   above changed). A state silently reachable from [r] is simulated by
   whatever simulates [r]: each of its moves is a weak move of [r]. And a
   state that silently reaches [n'] simulates whatever [n'] simulates: it
   answers as [n'] does, after the silent steps. So a proof need only treat
   one state per silent strongly connected component, and transfer. *)

(* A silent prefix extends a weak move. *)
Lemma weak_silent_prefix {M A} {lts : LTS M A} : forall x y z a,
    silent lts x y -> weak lts y z a -> weak lts x z a.
Proof.
  intros x y z a S W; destruct W as [b u t PRE ACT POST | TAUs].
  - exact (wk_some lts x z b u t (clos_rt_trans S PRE) ACT POST).
  - exact (wk_none lts x z (clos_rt_trans S TAUs)).
Qed.

(* A strong move after silent steps is a weak move. *)
Lemma weak_after_silent {M A} {lts : LTS M A} : forall r m m2 a,
    silent lts r m -> lts m a m2 -> weak lts r m2 a.
Proof.
  intros r m m2 a S T; exact (weak_silent_prefix r m m2 a S (inject_weak _ _ _ T)).
Qed.

Lemma weak_sim_silent_l {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall r m n, silent ltsM r m -> weak_sim ltsM ltsN r n ->
                weak_sim ltsM ltsN m n.
Proof.
  intros r m n S H; constructor; constructor; intros m2 a T.
  exact (weak_sim_act_clos H (weak_after_silent r m m2 a S T)).
Qed.

Lemma weak_sim_silent_r {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n n', silent ltsN n n' -> weak_sim ltsM ltsN m n' ->
                 weak_sim ltsM ltsN m n.
Proof.
  intros m n n' S H; constructor; constructor; intros m2 a T.
  destruct (sim_weak (out_sim H) T) as [n2 [W B]].
  exists n2; split; [exact (weak_silent_prefix n n' n2 a S W) | exact B].
Qed.

Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.
Hint Unfold weak_bisim : rel_db.

Lemma wk_bisim_refl {M A} (lts : LTS M A) : forall x, weak_bisim lts lts x x.
Proof. eauto with rel_db. Qed.

Lemma wk_bisim_trans {M A} (lts : LTS M A) : forall x y z,
    weak_bisim lts lts x y -> weak_bisim lts lts y z -> weak_bisim lts lts x z.
Proof. intros ??? [] []; eauto with rel_db. Qed.

Lemma wk_bisim_sym {M A} (lts : LTS M A) : forall x y,
    weak_bisim lts lts x y -> weak_bisim lts lts y x.
Proof. intros ?? []; eauto with rel_db. Qed.

(* Mutual similarity, under its honest name: a weak simulation each way,
   two possibly different relations. It is exactly [weak_bisim] above, whose
   name suggests bisimilarity but which is strictly coarser (see
   [weak_bisimilar] below, and [Test.v]'s [WeakBisimilarVsMutualSim]).
   [weak_bisim] is kept, unchanged, for existing proofs; new statements
   should say [mutual_sim] or [weak_bisimilar], whichever they mean. Added
   2026-10-02, alongside the existing definitions. *)
Section MutualSim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition mutual_sim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End MutualSim.
Hint Unfold mutual_sim : rel_db.

Lemma mutual_sim_weak_bisim {M N A} (ltsM : LTS M A) (ltsN : LTS N A) :
  forall s t, mutual_sim ltsM ltsN s t <-> weak_bisim ltsM ltsN s t.
Proof. split; intros H; exact H. Qed.

Lemma mutual_sim_refl {M A} (lts : LTS M A) : forall x, mutual_sim lts lts x x.
Proof. eauto with rel_db. Qed.

Lemma mutual_sim_sym {M N A} (ltsM : LTS M A) (ltsN : LTS N A) :
  forall s t, mutual_sim ltsM ltsN s t -> mutual_sim ltsN ltsM t s.
Proof. intros ?? []; split; assumption. Qed.

Lemma mutual_sim_trans {M N R A}
  (ltsM : LTS M A) (ltsN : LTS N A) (ltsR : LTS R A) :
  forall x y z, mutual_sim ltsM ltsN x y -> mutual_sim ltsN ltsR y z ->
                mutual_sim ltsM ltsR x z.
Proof. intros ??? [] []; split; eauto with rel_db. Qed.

(* Weak bisimilarity proper: ONE relation that is a weak simulation in both
   directions at once (Milner's observation equivalence; Sangiorgi,
   "Introduction to Bisimulation and Coinduction", ch. 4).

   [weak_bisim] above is two separate [weak_sim]s, i.e. mutual similarity,
   which is strictly coarser: each direction may pick a different relation.
   [a.b + a] and [a.b] are mutually similar but not bisimilar (after [a], the
   left can be stuck while the right can always still do [b]); so are
   [tau.a + b] and [a + b]. [weak_bisimilar] implies [weak_bisim]
   ([weak_bisimilar_weak_bisim]), not conversely.

   Added 2026-10-02, alongside the existing definitions rather than in place
   of them: nothing above is changed, and the plugin does not yet refer to
   anything below. *)
Section WeakBisimilar.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Record bisimF G m1 n1 :=
    Pack_bisim
      { bisim_l : forall {m2 a},
          ltsM m1 a m2 -> exists n2, weak ltsN n1 n2 a /\ G m2 n2
      ; bisim_r : forall {n2 a},
          ltsN n1 a n2 -> exists m2, weak ltsM m1 m2 a /\ G m2 n2
      }.

  CoInductive weak_bisimilar (s : M) (t : N) : Prop
    := In_bisim { out_bisim : bisimF weak_bisimilar s t }.
End WeakBisimilar.
Arguments Pack_bisim {M N A ltsM ltsN G m1 n1} _ _.
Arguments bisim_l {M N A ltsM ltsN G m1 n1} _ {m2 a} _.
Arguments bisim_r {M N A ltsM ltsN G m1 n1} _ {n2 a} _.
Arguments out_bisim {M N A ltsM ltsN s t} _.
Hint Constructors weak_bisimilar bisimF : rel_db.

Lemma weak_bisimilar_sim {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall s t, weak_bisimilar ltsM ltsN s t -> weak_sim ltsM ltsN s t.
Proof.
  cofix CH; intros s t H; constructor; constructor; intros m2 a T.
  destruct (bisim_l (out_bisim H) T) as [n2 [W B]].
  exists n2; split; [exact W | exact (CH _ _ B)].
Qed.

Lemma weak_bisimilar_sym {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall s t, weak_bisimilar ltsM ltsN s t -> weak_bisimilar ltsN ltsM t s.
Proof.
  cofix CH; intros s t H; constructor; constructor; intros x2 a T.
  - destruct (bisim_r (out_bisim H) T) as [m2 [W B]].
    exists m2; split; [exact W | exact (CH _ _ B)].
  - destruct (bisim_l (out_bisim H) T) as [n2 [W B]].
    exists n2; split; [exact W | exact (CH _ _ B)].
Qed.

Lemma weak_bisimilar_mutual_sim {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall s t, weak_bisimilar ltsM ltsN s t -> mutual_sim ltsM ltsN s t.
Proof.
  intros s t H; split.
  - exact (weak_bisimilar_sim _ _ H).
  - exact (weak_bisimilar_sim _ _ (weak_bisimilar_sym _ _ H)).
Qed.

Lemma weak_bisimilar_weak_bisim {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall s t, weak_bisimilar ltsM ltsN s t -> weak_bisim ltsM ltsN s t.
Proof.
  intros s t H; split.
  - exact (weak_bisimilar_sim _ _ H).
  - exact (weak_bisimilar_sim _ _ (weak_bisimilar_sym _ _ H)).
Qed.

Lemma weak_bisimilar_refl {M A} (lts : LTS M A) :
  forall x, weak_bisimilar lts lts x x.
Proof.
  cofix CH; intros x; constructor; constructor; intros y a T; exists y;
    (split; [exact (inject_weak _ _ _ T) | exact (CH y)]).
Qed.

Lemma weak_bisimilar_silent_clos : forall {M N A ltsM ltsN m1 n1},
    @weak_bisimilar M N A ltsM ltsN m1 n1 ->
    forall {m2}, silent ltsM m1 m2 ->
                 exists n2, silent ltsN n1 n2 /\ weak_bisimilar ltsM ltsN m2 n2.
Proof.
  intros; revert n1 H. induction H0 as [|????? Ih]; eauto with rel_db.
  intros; destruct (bisim_l (out_bisim H1) H) as [?[W Ws]].
  apply Ih in Ws as [?[??]]; inversion W; eauto with rel_db.
Qed.

Lemma weak_bisimilar_act_clos : forall {M N A ltsM ltsN m1 n1},
    @weak_bisimilar M N A ltsM ltsN m1 n1 ->
    forall {m2 a}, weak ltsM m1 m2 a ->
                 exists n2, weak ltsN n1 n2 a /\ weak_bisimilar ltsM ltsN m2 n2.
Proof.
  intros. destruct H0 as [??? PRE ACT POST|TAUs].
  - destruct (weak_bisimilar_silent_clos H PRE) as [?[? W1]].
    destruct (bisim_l (out_bisim W1) ACT) as [?[Wk W2]].
    destruct (weak_bisimilar_silent_clos W2 POST) as [?[]].
    inversion Wk; eauto 10 with rel_db.
  - destruct (weak_bisimilar_silent_clos H TAUs) as [?[]].
    eauto with rel_db.
Qed.

Lemma weak_bisimilar_trans {M N R A}
  (ltsM : LTS M A) (ltsN : LTS N A) (ltsR : LTS R A)
  : forall x y r, weak_bisimilar ltsM ltsN x y -> weak_bisimilar ltsN ltsR y r ->
                  weak_bisimilar ltsM ltsR x r.
Proof.
  cofix CH; intros x y r Hxy Hyr; constructor; constructor; intros.
  - destruct (bisim_l (out_bisim Hxy) H) as [y' [Wy Bxy]].
    destruct (weak_bisimilar_act_clos Hyr Wy) as [r' [Wr Byr]].
    exists r'; split; [exact Wr | exact (CH _ _ _ Bxy Byr)].
  - destruct (bisim_r (out_bisim Hyr) H) as [y' [Wy Byr]].
    destruct (weak_bisimilar_act_clos (weak_bisimilar_sym _ _ Hxy) Wy)
      as [x' [Wx Byx]].
    exists x'; split;
      [exact Wx | exact (CH _ _ _ (weak_bisimilar_sym _ _ Byx) Byr)].
Qed.

(* The same for [weak_bisimilar], within a silent SCC: [m] and [r] must
   reach each other silently. One way is not enough: [m] must also answer
   the other side's moves as [r] did, which it can only do by first reaching
   [r] ([tau.a + b] reaches [a], but [a] cannot answer a [b]). Added
   2026-10-03 (notes/13). *)
Lemma weak_bisimilar_silent_l {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall r m n, silent ltsM r m -> silent ltsM m r ->
                weak_bisimilar ltsM ltsN r n -> weak_bisimilar ltsM ltsN m n.
Proof.
  intros r m n Srm Smr H; constructor; constructor.
  - intros m2 a T.
    exact (weak_bisimilar_act_clos H (weak_after_silent r m m2 a Srm T)).
  - intros n2 a T.
    destruct (bisim_r (out_bisim H) T) as [m2 [W B]].
    exists m2; split; [exact (weak_silent_prefix m r m2 a Smr W) | exact B].
Qed.

Lemma weak_bisimilar_silent_r {M N A} {ltsM : LTS M A} {ltsN : LTS N A} :
  forall m n n', silent ltsN n n' -> silent ltsN n' n ->
                 weak_bisimilar ltsM ltsN m n' -> weak_bisimilar ltsM ltsN m n.
Proof.
  intros m n n' Snn' Sn'n H.
  exact (weak_bisimilar_sym _ _
           (weak_bisimilar_silent_l n' n m Sn'n Snn' (weak_bisimilar_sym _ _ H))).
Qed.
