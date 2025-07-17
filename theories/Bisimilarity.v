Require Import MEBI.Examples.CADP.

(* https://rocq-prover.org/doc/v8.9/stdlib/Coq.Relations.Relation_Operators.html *)
Require Import Coq.Relations.Relation_Definitions.
Require Import Coq.Relations.Relation_Operators.
Require Coq.Relations.Operators_Properties.
Require Import Coq.Classes.RelationClasses.

Set Primitive Projections.

(**************************************)
Section Definitions.
  Context (M : Type) (A : Type).
  (* option A = None -> silent (tau-labelled) transition *)
  Definition LTS : Type := M -> option A -> M -> Prop.
  (* tau-labelled transition *)
  Definition tau (R : LTS) : relation M := fun (x y : M) => R x None y.
End Definitions.
Arguments tau {M A} R.

(**************************************)
Section WeakTrans.
  Context {M : Type} {A : Type} (lts : LTS M A).

  (* trace of tau-labelled transitions *)
  Definition silent  : relation M := clos_refl_trans_1n M (tau lts).
  Definition silent1 : relation M := clos_trans_1n M (tau lts).

  (* x ==> pre_str ->^a post_str ==> y *)
  Record weak_tr (x z : M) a (t y : M) : Prop :=
    Pack_weak { pre : silent x z; str : lts z a t; post : silent t y }.
  Definition weak (x : M) a (y : M) : Prop := exists z t, weak_tr x z a t y.
End WeakTrans.

(**************************************)
Section WeakSim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Record simF G m1 n1 :=
    Pack_sim
      { sim_weak : forall m2 a,
          weak ltsM m1 (Some a) m2 ->
          exists n2, weak ltsN n1 (Some a) n2 /\ G m2 n2;
        sim_tau : forall m2,
          (* weak ltsM m1 None m2 -> *)
          silent1 ltsM m1 m2 ->
          exists n2, silent ltsN n1 n2 /\ G m2 n2;
      }.

  CoInductive weak_sim (s : M) (t : N) : Prop := 
    In_sim { out_sim : simF weak_sim s t }.
End WeakSim.

(** (unused) **************************)
Lemma _weak_sim_silent_refl (X A : Type) (lts : LTS X A)
: forall x, weak_sim lts lts x x -> silent lts x x.
Proof. intros x Hxx. constructor. Qed.

(** (unused) **************************)
Lemma _weak_sim_silent_geq_1 (X A : Type) (lts : LTS X A)
: forall x, weak_sim lts lts x x -> silent lts x x ->
  exists y, silent1 lts x y -> silent lts x y /\ weak_sim lts lts x y.
Proof. 
  intros x Hxx_sim Hxx. eexists x. intros _. split.
  - apply Hxx.
  - apply Hxx_sim. 
Qed.

(** (unused) **************************)
Lemma _silent_geq_1 (X A : Type) (lts : LTS X A)
: forall x, silent lts x x ->
  exists y, silent1 lts x y -> silent lts x y.
Proof. intros x Hxx. eexists x. intros _. apply Hxx. Qed.

(** (unused) **************************)
Lemma _tau_silent1 (X A: Type) (lts : LTS X A)
: forall x y, tau lts x y -> silent1 lts x y.
Proof. intros x y Hxy. constructor. apply Hxy. Qed.

(** (1 admitted case) *****************)
Lemma silent1_step (X A: Type) (lts : LTS X A)
: forall x y, silent1 lts x y -> silent lts x y.
Proof.
  intros x y Hxy. induction Hxy.
  - eapply rt1n_trans. apply H. apply rt1n_refl.
  - apply (rt1n_trans _ _ _ _ _ H IHHxy).
Qed.

(** (depends on Lemma [silent1_step]) *)
Lemma weak_sim_refl (X A : Type) (lts : LTS X A)
: forall x, weak_sim lts lts x x.
Proof.
  cofix CH; intros x. 
  (* cofix Hxx_sim. *)
  apply In_sim. apply Pack_sim.
  { intros y a Hxy; eexists y; split.
    - apply Hxy.
    - apply CH. Guarded. }
  { intros y Hxy.
    eexists y. split; [| apply CH]. Guarded.
    (*****************************)
    (* silent  ltsX x y       -> *)
    (* silent1 ltsX x y          *)
    (*****************************)
    apply silent1_step. apply Hxy. }
Qed.

(**************************************)
(* Lemma silent_trans_l (X A : Type) (lts : LTS X A)
: forall x y, silent1 lts x y -> 
  exists z, silent lts x z -> silent1 lts z y.
Proof.
  intros x y Hxy. apply clos_t1n_trans in Hxy.
  inversion  Hxy as [|z]; subst.
  - eexists x. intros Hxx. apply clos_trans_t1n. apply Hxy.
  - eexists z. intros Hxz. apply clos_trans_t1n. apply H0.
Qed.

(**************************************)
Lemma silent_trans_r (X A : Type) (lts : LTS X A)
: forall x y, silent1 lts x y -> 
  exists z, silent1 lts x z -> silent lts z y.
Proof. intros x y Hxy. eexists y. intros _. constructor. Qed.

(** (unused, 1 admitted case) *********)
Lemma _silent_trans (X A : Type) (lts : LTS X A)
: forall x, weak_sim lts lts x x -> 
  forall y, silent1 lts x y -> 
  exists z1 z2, silent lts x z1 -> silent1 lts z1 z2 -> silent lts z2 y.
Proof.
  intros x Hxx_sim y Hxy.

  assert (Hxy_z := Hxy); apply silent_trans_l in Hxy_z.
  destruct Hxy_z as [z1 Hxy_z]; eexists z1.
  apply silent_trans_r in Hxy_z.
  { destruct Hxy_z as [z2 Hxy_z]; eexists z2.
    intros Hxz Hzz. apply Hxy_z. apply Hzz. }
  { (********************************)
    (* Hxy : silent1 lts x y        *)
    (* apply silent_trans_l in Hxy. *)
    About silent_trans_l.
    (* Hxy : silent lts x z1 ->     *)
    (*       silent lts z1 y        *)
    (* apply silent_trans_r in Hxy. *)
    About silent_trans_r.
    (* Hxy : silent lts x z1   ->   *)
    (*       silent1 lts z1 z2 ->   *)
    (*       silent lts z2 y        *)
    (********************************)
    admit. }
Admitted. *)

Lemma rt_t {X A : Type} {ltsX : LTS X A}
: forall {x1 x2 x3},
  tau ltsX x1 x2 ->
  clos_refl_trans_1n X (tau ltsX) x2 x3 ->
  clos_trans_1n X (tau ltsX) x1 x3.
Proof.
  intros x y z Hxy Hyz.
  revert x Hxy.
  induction Hyz.
  - intros x0 Hxy. eapply t1n_step, Hxy.
  - intros x0 Hxy.  
    eapply t1n_trans.
    + apply Hxy.
    + apply IHHyz. apply H.   
Qed.

(** (1 admitted case) *****************)
Lemma weak_sim_trans (X Y Z A : Type) 
(ltsX : LTS X A) (ltsY : LTS Y A) (ltsZ : LTS Z A) 
: forall x y z,
  weak_sim ltsX ltsY x y -> 
  weak_sim ltsY ltsZ y z -> 
  weak_sim ltsX ltsZ x z.
Proof.
  cofix CH; intros x1 y1 z1.
  intros Hxy. intros Hyz.

  inversion Hxy as [Hxy_inv]; subst; destruct Hxy_inv as [Hxy_weak Hxy_tau].
  inversion Hyz as [Hyz_inv]; subst; destruct Hyz_inv as [Hyz_weak Hyz_tau].

  apply In_sim. apply Pack_sim.
  { intros x2 ax Hx. 
  
    apply Hxy_weak in Hx; inversion_clear Hx as [y2 Hy_xy]; subst.
    destruct Hy_xy as [Hy Hxy2].

    apply Hyz_weak in Hy; inversion_clear Hy as [z2 Hz_yz]; subst.
    destruct Hz_yz as [Hz Hyz2].

    eexists z2. split.
    - apply Hz.
    - About CH. apply (@CH x2 y2 z2 Hxy2 Hyz2). Guarded. }
  { intros x2 Hx.
  
    apply Hxy_tau in Hx. destruct Hx as [y2 [[|y2a y2b Hy12a] H0]].
    - eexists z1. split.
      + apply rt1n_refl.
      + eapply CH. apply H0. apply Hyz. Guarded.
    - (* turn into lemma *)

      About rt_t.
      apply (rt_t Hy12a) in H.
      apply Hyz_tau in H. destruct H as [z2 [Hzz Hyz2]].
      eexists z2. split.
      apply Hzz. eapply CH. apply H0. apply Hyz2. Guarded.
  } 
Qed.

(**************************************)
Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.

(**************************************)
Lemma weak_bisim_refl (M A : Type) (ltsM : LTS M A)
: forall m, weak_bisim ltsM ltsM m m.
Proof. constructor 1; try apply weak_sim_refl. Qed.

(**************************************)
Lemma weak_bisim_sym (X Y A : Type) (ltsX : LTS X A) (ltsY : LTS Y A) 
: forall x y, weak_bisim ltsX ltsY x y -> weak_bisim ltsY ltsX y x.
Proof. 
  intros x y Hxy. split.
  - destruct Hxy as [_ Hyx]. apply Hyx.
  - destruct Hxy as [Hxy _]. apply Hxy.
Qed.

(**************************************)
Lemma weak_bisim_trans (X Y Z A : Type) 
(ltsX : LTS X A) (ltsY : LTS Y A) (ltsZ : LTS Z A) 
: forall x y z,
  weak_bisim ltsX ltsY x y ->
  weak_bisim ltsY ltsZ y z ->
  weak_bisim ltsX ltsZ x z.
Proof.
  intros x y z Hbxy Hbyz.
  destruct Hbxy as [Hsxy Hsyx]. destruct Hbyz as [Hsyz Hszy].
  split. 
  - apply (@weak_sim_trans X Y Z A ltsX ltsY ltsZ x y z).
    + apply Hsxy.
    + apply Hsyz.
  - apply (@weak_sim_trans Z Y X A ltsZ ltsY ltsX z y x).
    + apply Hszy.
    + apply Hsyx.
Qed.

(**************************************)
Module BisimTest1.
  Inductive action : Type := | TheAction1 | TheAction2.

  Inductive term : Type :=
    | trec : term
    | tend : term
    | tfix : term -> term
    | tact : option action -> term -> term
    | tpar : option action -> option action -> term -> term
    .

  Fixpoint subst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (subst t1 t)
    | tpar a b t => tpar a b (subst t1 t)
    end.

  Inductive termLTS : term -> option action -> term -> Prop :=
    | do_act : forall a t, 
        (* out_sim (tact a t) = consF a t -> *)
        termLTS (tact a t) a t

    | do_parl : forall a b t, 
        (* out_sim (tpar a b t) = consF a (tact b t) -> *)
        termLTS (tpar a b t) a (tact b t)

    | do_parr : forall a b t, 
        (* out_sim (tpar a b t) = consF a (tact a t) -> *)
        termLTS (tpar a b t) b (tact a t)

    | do_fix : forall a t t',
        (* out_sim (tfix t) = consF a t' -> *)
        termLTS (subst (tfix t) t) a t' ->
        termLTS (tfix t) a t'
    .

  Inductive termLTS_tc : term -> Prop :=
    | do_step : forall t a t', termLTS t a t' -> termLTS_tc t' -> termLTS_tc t
    | do_none : forall t, termLTS_tc t
    .

  Lemma sim_tend : weak_sim termLTS termLTS (tend) (tend).
  Proof. apply weak_sim_refl. Qed.
End BisimTest1.

(**************************************)
Module BisimExa1. Import BisimTest1.

  Example m1 := (tact None (tact (Some TheAction1) (tact None tend))).
  Example n1 := (tact (Some TheAction1) tend).

  Goal weak_sim termLTS termLTS m1 n1.
  Proof. 
    apply In_sim. apply Pack_sim.
    { intros m2 a Hw. destruct Hw. destruct H. destruct H.
      destruct pre0.
      inversion str0.
      inversion H; subst. 
      destruct pre0; [| inversion H0]. (* clear H. *)
      inversion str0; subst. (* clear str0. *)
      inversion post0; subst.
      - eexists tend. split.
        + do 2 eexists. do 2 constructor. 
        + admit.
      - eexists tend. split.
        + do 2 eexists. do 2 constructor.
        + admit.
    }
    { intros m2 Hw. eexists n1. split.
      - constructor.
      - admit. 
    }
  Admitted. 
End BisimExa1.

(**************************************)
Module BisimExa2. Import BisimTest1.
  Example m1 := (tfix (tact (Some TheAction1) trec)).
  Example n1 := (tact (Some TheAction1) (tfix (tact (Some TheAction1) trec))).
  
  Goal weak_sim termLTS termLTS m1 n1.
  Proof.
    apply In_sim; apply Pack_sim.
  
  Admitted.

End BisimExa2.


