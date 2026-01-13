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
