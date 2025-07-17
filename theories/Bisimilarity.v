Require Import MEBI.Examples.CADP.

(* https://rocq-prover.org/doc/v8.9/stdlib/Coq.Relations.Relation_Operators.html *)
Require Import Coq.Relations.Relation_Definitions.
Require Import Coq.Relations.Relation_Operators.
Require Import Coq.Relations.Operators_Properties.
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

(** (1 admitted case) *****************)
Lemma silent1_step (M A: Type) (lts : LTS M A)
: forall x y, silent1 lts x y -> silent lts x y.
Proof.
  intros x y Hxy.
  inversion Hxy; subst.
  - apply clos_rt1n_step. apply H.
  - apply clos_rt1n_step. admit.
Admitted.

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
Lemma silent_trans_l (X A : Type) (lts : LTS X A)
: forall x y, silent1 lts x y -> 
  exists z, silent lts x z -> silent1 lts z y.
Proof.
  intros x y Hxy. apply clos_t1n_trans in Hxy.
  inversion Hxy as [|z]; subst.
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
Admitted.

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
  { intros x2 Hx. eexists ?[z2]. split.
    { constructor. }
    { apply (@CH x2 y1 z1); [| apply Hyz]. Guarded.
    
      apply Hxy_tau in Hx; inversion_clear Hx as [y2 Hy]; subst.
      destruct Hy as [Hy Hxy2]. inversion Hy; subst.
      { apply Hxy2. }
      { (*******************************)
        (* weak_sim ltsX ltsY x1 y1 /\ *)
        (* silent1 ltsX x1 x2       /\ *)
        (* silent  ltsY y1 y2       /\ *)
        (* weak_sim ltsX ltsY x2 y2 -> *)

        (* weak_sim ltsX ltsY x2 y1    *)
        (*******************************)
        admit. } } }
Admitted.


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
