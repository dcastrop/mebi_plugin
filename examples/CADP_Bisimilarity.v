Require Import MEBI.Examples.CADP.

(* https://rocq-prover.org/doc/v8.9/stdlib/Coq.Relations.Relation_Operators.html *)
Require Import Coq.Relations.Relation_Definitions.
Require Import Coq.Relations.Relation_Operators.
Require Import Coq.Relations.Operators_Properties.
Require Import Coq.Classes.RelationClasses.
(* Require Import FunInd. *)

Set Primitive Projections.

Section Definitions.
  Context (M : Type) (A : Type).
  Definition LTS : Type := M -> option A -> M -> Prop.
  (* tau-labelled transition *)
  Definition tau (R : LTS) : relation M := fun (x y : M) => R x None y.
End Definitions.
Arguments tau {M A} R.

Section WeakTrans.
  Context {M : Type} {A : Type} (lts : LTS M A).

  (* trace of tau-labelled transitions *)
  Definition silent : relation M := clos_refl_trans_1n M (tau lts).
  Definition silent1 : relation M := clos_trans_1n M (tau lts).

  (*  x ==> pre_str ->^a post_str ==> y *)
  Record weak_tr (x z : M) a (t y : M) : Prop :=
    Pack_weak { pre : silent x z; str : lts z a t; post : silent t y }.
  Definition weak (x : M) a (y : M) : Prop :=
    exists z t, weak_tr x z a t y.
End WeakTrans.


(************* admitted lemmas ****************)
(** R.e.: silent1 lts m m' -> silent lts m m' *)
(**       since if we know one-or-more have occurred *)
Lemma clos_silent (M A: Type) (lts : LTS M A) : forall m1 m2, 
  clos_trans_1n M (tau lts) m1 m2 -> 
  clos_refl_trans_1n M (tau lts) m1 m2.
Proof.
  intros x y Hxy. 
  (* destruct Hxy as [ y Hxyt | y z Hyzt Hyztr ]. *)
  induction Hxy as [ x y Hxyt | x y z Hxytr Hyzt Hyztr ].
  - apply clos_rt1n_step; apply Hxyt.
  - apply clos_rt1n_step in Hxytr; apply clos_rt1n_rt in Hxytr.
    apply clos_t1n_trans in Hyzt. apply clos_rt1n_rt in Hyztr. 

    (* apply (@clos_rt_t M (tau lts) x y z Hxytr Hyzt). *)

    admit.
Admitted. 

Lemma silent1_step (M A: Type) (lts : LTS M A): forall m m', 
  silent1 lts m m' -> silent lts m m'.
Proof. intros m1 m2. apply clos_silent. Qed.

Lemma _clos_silent (X A : Type) (lts : LTS X A)
: forall x y,
  clos_trans_1n X (tau lts) x y ->
  exists z,
  ( clos_refl_trans_1n X (tau lts) x z ->
    clos_trans_1n X (tau lts) z y ).
Proof.
  intros x y Hxy.

  inversion Hxy as [| z _y Hxz Hzy]; subst.
  - eexists x. intros _Hxx. apply Hxy.
  - eexists z. intros _Hxz. apply Hzy.
Qed.  

Lemma silent_geq_1 (X A : Type) (lts : LTS X A)
: forall x y,
  silent1 lts x y ->
  exists z,
  tau lts x z -> 
  silent lts z y ->
  silent lts x y.
Proof.
  intros x y.
  unfold silent. unfold silent1.
  intros Hxy1.

  inversion Hxy1 as [ _y Hxy_t | z _y Hxz_t Hzy ]; subst.
  - eexists x; intros Hxx_t Hxy. 
    apply clos_rt1n_step. apply Hxy_t.
  - eexists y. intros Hxy_t Hyy. 
    apply clos_rt1n_step. apply Hxy_t.
Qed.
  
  (* eexists z; intros _.

    apply clos_rt1n_step in Hxz.
    apply _clos_silent.

    apply clos_t1n_trans in Hxz.
    apply clos_trans_1n in Hxz.
    + constructor. 

    apply _clos_silent.

    apply clos_rt1n_step.

    unfold silent.

    inversion Hzy; subst.
    inversion Hzy; subst.
    + admit.
    +  

    apply Hxz.
    intros Hxy.  

  destruct Hxy1.
  - eexists x. intros Hxx.
    apply clos_rt1n_step. apply H.
  - eexists y. intros Hxy. 
    unfold silent. apply Hxy1. 

  inversion H1xy; subst.
  - eexists y. 
    intros Hxy.
    apply H.  *)



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

Lemma weak_sim_silent_geq_1 (X A : Type) (lts : LTS X A)
: forall x y,
  weak_sim lts lts x y ->
  silent1 lts x y ->
  exists z,
  tau lts x z -> 
  weak_sim lts lts x z.
Proof.
  intros x y Hxy.
  unfold silent. unfold silent1.
  intros Hxy1.

  inversion Hxy1 as [ _y Hxz | z _y Hxz Hzy ]; subst;
  eexists y; intros _; apply Hxy.
Qed.
  
Lemma _weak_sim_step_none (M N A : Type) (ltsM : LTS M A) (ltsN : LTS N A) 
: forall x1 y1, 
  weak_sim ltsM ltsN x1 y1 ->
  forall x2, silent1 ltsM x1 x2 ->
  exists y2, silent ltsN y1 y2 /\ weak_sim ltsM ltsN x2 y2.
Proof.
  intros x1 y1 Hw1.
  inversion_clear Hw1; subst. inversion out_sim0; subst. apply sim_tau0.
Qed.

Lemma _weak_sim_step_some (M N A : Type) (ltsM : LTS M A) (ltsN : LTS N A) 
: forall x1 y1, 
  weak_sim ltsM ltsN x1 y1 ->
  forall x2 a, weak ltsM x1 (Some a) x2 ->
  exists y2, weak ltsN y1 (Some a) y2 /\ weak_sim ltsM ltsN x2 y2.
Proof.
  intros x1 y1 Hw1.
  inversion_clear Hw1; subst. inversion out_sim0; subst. apply sim_weak0.
Qed.

Lemma _weak_sim_step (M N A : Type) (ltsM : LTS M A) (ltsN : LTS N A)
: forall m1 n1,
  weak_sim ltsM ltsN m1 n1 ->
  ( forall m2 a, weak ltsM m1 (Some a) m2 ->
    exists n2, weak ltsN n1 (Some a) n2 /\ weak_sim ltsM ltsN m2 n2)
  \/
  ( forall m2, silent1 ltsM m1 m2 ->
    exists n2, silent ltsN n1 n2 /\ weak_sim ltsM ltsN m2 n2 ).
Proof. 
  intros m1 n1 Hw1. left. 
  inversion_clear Hw1; subst. inversion out_sim0; subst. apply sim_weak0.
Qed.


(* Lemma _w (X A : Type) (lts : LTS X A)
: forall x, 
  weak_sim lts lts x x -> 
  forall y, 
  silent1 lts x y -> 
  silent lts x y ->
  exists z,
  silent1 lts x z -> 
  silent lts z y.
Proof.
  intros x Hxx y Hxy1 Hxy. *)


(* Lemma _w (X A : Type) (lts : LTS X A)
: forall x, 
  weak_sim lts lts x x -> 
  forall y, 
  silent1 lts x y -> 
  exists z,
  silent lts x z ->
  silent1 lts z y.
Proof.
  intros x Hxx_sim y Hxy.

  (* apply clos_t1n_trans in Hxy.
  inversion Hxy as [|z]; subst.
  - eexists x. intros Hxx. apply clos_trans_t1n. apply Hxy.
  - eexists z. intros Hxz. apply clos_trans_t1n. apply H0.
Qed. *)


  apply clos_t1n_trans in Hxy.
  inversion Hxy as [|_z z]; subst.
  - apply clos_rt1n_step. apply H.
  - apply clos_rt_rt1n.


    (* apply (@clos_rt_t X (tau lts) x y z _ _ ). *)
    (* apply (@clos_rt_t X (tau lts) x y z) in H. *)


    apply t_step in Hxy.

    About rt_trans.
    apply (@rt_trans X (tau lts) x z y).
    + apply (@rt_trans X (tau lts) x y z).


    About clos_rt_t.
    (* apply (@clos_rt_t X (tau lts) x y z (clos_refl_trans X (tau lts) x y) H0    ) in H. *)
    apply (@clos_rt_t X (tau lts) x z y _ H0) in Hxy.




  destruct Hxy.
  - apply clos_rt1n_step. apply H.
  - 
    apply clos_rt_rt1n.
    apply clos_t1n_trans in Hxy.
    apply (@clos_rt_t X (tau lts) x y z _ Hxy   ) in Hxy.

    
    apply (@rt_trans) in H.


  apply clos_t1n_trans in Hxy.
  inversion Hxy; subst.
  - apply clos_rt1n_step. apply H.
  - apply clos_rt_rt1n.
    (* About clos_rt_t.  *)
    (* apply (@clos_rt_t X (tau lts) x ) in H. *)
    (* apply (@clos_rt_t X (tau lts) x ) in H0. *)
    apply (@clos_rt_t X (tau lts) x ).
    +  *)

  
Lemma silent_trans_l (X A : Type) (lts : LTS X A)
: forall x, 
  weak_sim lts lts x x -> 
  forall y, 
  silent1 lts x y -> 
  exists z,
  silent lts x z ->
  silent1 lts z y.
Proof.
  intros x Hxx_sim y Hxy.

  apply clos_t1n_trans in Hxy.
  inversion Hxy as [|z]; subst.
  - eexists x. intros Hxx. apply clos_trans_t1n. apply Hxy.
  - eexists z. intros Hxz. apply clos_trans_t1n. apply H0.
Qed.

Lemma silent_trans_r (X A : Type) (lts : LTS X A)
: forall x, 
  weak_sim lts lts x x -> 
  forall y, 
  silent1 lts x y -> 
  exists z,
  silent1 lts x z ->
  silent lts z y.
Proof.
  intros x Hxx_sim y Hxy.
  eexists y. intros _Hxy.
  constructor.
Qed.

(* Lemma weak_sim_refl_weak (X A : Type) (lts : LTS X A)
: forall x, 
  weak_sim lts lts x x ->
  forall y a,
  weak lts x (Some a) y ->
  exists z,
  weak lts x (Some a) z /\
  weak_sim lts lts y z.
Proof. intros x Hxx_sim. apply Hxx_sim. Qed.  *)

Lemma silent_trans (X A : Type) (lts : LTS X A)
: forall x, 
  weak_sim lts lts x x -> 
  forall y, 
  silent1 lts x y -> 
  exists z1 z2,
  silent lts x z1 ->
  silent1 lts z1 z2 ->
  silent lts z2 y.
Proof.
  intros x Hxx_sim y Hxy. 
  assert (Hxy_l := Hxy); apply silent_trans_l in Hxy_l; [| apply Hxx_sim].
  destruct Hxy_l as [z1 Hxy_l]; eexists z1.
  
  assert (Hxy_r := Hxy_l); apply silent_trans_r in Hxy_r.
  { destruct Hxy_r as [z2 Hxy_r]; eexists z2.
    intros Hxz Hzz. apply Hxy_r. apply Hzz. 
  }
  { admit. }
  { admit. }
Admitted.



  (* assert (Hxy_r := Hxy_l). apply silent_trans_r in Hxy_r.
  { destruct Hxy_r as [z2 Hxy_r]. eexists z2. 
    intros Hxz Hzz. apply Hxy_r. apply Hzz. }
  { admit. }
  {
    destruct Hxy_l; subst.
  }


  {
    cofix CHz1. 
    apply In_sim. apply Pack_sim.
    - intros z1b a Hzz1. eexists z1b. split.
      + apply Hzz1.
      + apply CHz1. 
    
    apply weak_sim_refl_weak. apply CHz1. Guarded.  

  }
  - apply Hzz.
  - admit.
  - destruct Hxy_l.
  
  apply Hxy_l.  
  eexists z2.












  intros x Hxx_sim y Hxy. 
  assert (Hxy_r := Hxy); apply silent_trans_r in Hxy_r; [| apply Hxx_sim].
  destruct Hxy_r as [z1 Hxy_r]; eexists z1.
  
  assert (Hxy_l := Hxy_r).
  apply silent_trans_l in Hxy.


  assert (Hxy_l' := Hxy_r); destruct Hxy_l'; subst.
  - apply Hxy_r. 
  apply silent_trans_l in Hxy_l.
   [| apply Hxx_sim].
  destruct Hxy_l as [z2 Hxy_l]; eexists z2.





  (** prove that z1 z1 is weak_sim *)
  assert (weak_sim lts lts z1 z1).
  {

  }












  assert (Hxy_r := Hxy); apply silent_trans_r in Hxy_r; [| apply Hxx_sim].
  destruct Hxy_r as [z2 Hxy_r]; eexists z2.

  intros Hxz1. apply Hxy_l in Hxz1.
  (* intros Hzz. *)
  About silent_trans_r.
  apply (@silent_trans_r X A lts x Hxx_sim y ).
  
  
  (* eexists ?[z2]. *)
  intros Hxz1. apply Hxy_l in Hxz1.








  assert (Hxy_r := Hxz1); apply silent_trans_r in Hxy_r.
  destruct Hxy_r as [z2 Hxy_r]. 
  intros Hzz. apply Hxy_r in Hzz.
  eexists z2.



  assert (Hxy_r := Hxy); apply silent_trans_r in Hxy_r; [| apply Hxx_sim].
  destruct Hxy_r as [z2 Hxy_r]; eexists z2.

  intros Hxz1. apply Hxy_l in Hxz1.
  
  intros Hzz.

  inversion Hxy_l; subst.
   *)

(* Lemma silent_trans_refl (X A : Type) (lts : LTS X A)
: forall x, 
  weak_sim lts lts x x -> 
  silent lts x x -> 
  forall y, 
  weak_sim lts lts y y ->
  silent lts x y -> 
  weak_sim lts lts x y.
Proof.
  (* cofix CH. *)
  intros x Hxx_sim Hxx y Hyy_sim Hxy.

  inversion Hxx_sim as [Hxx_simF]; subst;
  inversion_clear Hxx_simF as [Hxx_weak Hxx_tau]; subst.

  inversion Hyy_sim as [Hyy_simF]; subst;
  inversion_clear Hyy_simF as [Hyy_weak Hyy_tau]; subst.

  apply In_sim; apply Pack_sim.
  - intros z a Hxz_weak. eexists z. split. (* [| apply CH]. Guarded. *)
    + apply Hxx_weak in Hxz_weak.
      inversion Hxz_weak as [z']; subst.
    
    apply Hyy_weak.
      constructor. apply Hx.
    + apply CH. Guarded.

  apply clos_t1n_trans in Hxy.
  inversion Hxy as [|z]; subst.
  - eexists x. intros Hxx. apply clos_trans_t1n. apply Hxy.
  - eexists z. intros Hxz. apply clos_trans_t1n. apply H0.
Qed. *)


Lemma weak_sim_refl (X A : Type) (lts : LTS X A)
: forall x, weak_sim lts lts x x.
Proof.
  cofix CH; intros x. cofix Hxx_sim.
  apply In_sim. apply Pack_sim.
  { intros y a Hxy; eexists y; split.
    + apply Hxy.
    + apply CH. Guarded.
  }
  { intros y Hxy.
    eexists y.
    split.
    - 
      About silent_trans.
      apply (@silent_trans_l X A lts x Hxx_sim y) in Hxy.
      destruct Hxy as [z Hxzzy].
      apply silent_trans_r in Hxzzy.


      About silent_trans2.
      apply (@silent_trans2 X A lts x Hxx_sim y ) in Hxy.


    - constructor.
    - destruct Hxy.

    (* apply (@silent_trans X A lts x Hxx_sim y) in Hxy. *)
    apply (@silent_trans X A lts x Hxx_sim y) in Hxy.
    destruct Hxy as [z].
    - eexists z. split.
      +
      
        destruct H.
        * admit. 
        * admit. 
        * admit. 
      split.
      * apply H.  

    eexists ?[z].
    
  }

    eexists y. split; [| apply CH]. Guarded.

    About silent_trans.
    apply (@silent_trans X A lts x Hxx_sim y y).

    apply clos_t1n_trans in Hxy.


    inversion Hxy; subst.
    { apply clos_rt1n_step. apply H. }
    { apply clos_t1n_trans in H0.
      apply clos_rt_rt1n.
      
      apply (@rt_trans X (tau lts) x y0 y).
      - admit.
      - About clos_rt_t. apply clos_rt_t.

    }
      apply clos_rt_rt1n.
      apply (@rt_trans X (tau lts) x y0 y).
      * apply clos_rt_rt1n in H.   


      apply clos_rt1n_step.
      
      assert ((tau lts x y0 -> clos_trans X (tau lts) y0 y) -> tau lts x y).
      { 
        intros Ha. destruct Ha.
        - apply H.
        - apply clos_t1n_trans in Hxy.  
          destruct H0.
          + assert ((tau lts x y0 /\ tau lts y0 y) -> tau lts x y) as Hb.
            {
              intros Hb'.
              destruct Hb'.
              About clos_refl_trans_1n.
              About rt_trans.
              About rt1n_trans.
              apply (@rt_trans X (tau lts) x y0 y).  
            }

      }
      { About H1. apply H1; intros _H. apply H0. }
    
    About silent_geq_1.
    (* inversion Hxy as [_y Hxy_t| z _y Hxz_t Hzy]; subst. *)
    (* apply silent_geq_1 in Hxy. *)

    apply weak_sim_silent_geq_1.

    apply silent_geq_1 in Hxy.
    destruct Hxy as [z H].
    destruct H as [| Hxz_t H].
    + constructor.
     destruct H as [Hzy Hxy].


    eexists y.
    split.
    + inversion Hxy as [_y Hxy_t| z _y Hxz_t Hzy]; subst.
      * apply clos_rt1n_step. apply Hxy_t.
      *
    
    apply silent_geq_1 in Hxy.
    
    (* unfold silent. *)
    
    (* apply clos_t1n_trans. *)
    (* apply clos_trans_1n. *)

    About weak_sim_silent_geq_1.
    apply (@weak_sim_silent_geq_1 X A lts x y) in Hxy.


    inversion Hxy; subst.
    About silent_geq_1.
    apply silent_geq_1 in Hxy.
    apply (@silent_geq_1 X A lts x y).

    eexists ?[z]. split.
    + 
      About silent1_step.

      About silent_geq_1.
      apply silent_geq_1 in Hxy.



    About silent_geq_1.
    apply silent_geq_1 in Hxy.
    induction Hxy as [z Hxz_zy].

    inversion Hxy; subst.
    apply clos_rt1n_step in H.



    inversion Hx; subst.





  - intros x2 Hx; eexists x2.
    split.
    + apply silent1_step. apply Hx.
    + apply CH. Guarded.
Qed.

Lemma _weak_sim_trans_some (X Y Z A : Type) 
(ltsX : LTS X A) (ltsY : LTS Y A) (ltsZ : LTS Z A) 
: forall x y z,
  weak_sim ltsX ltsY x y -> 
  weak_sim ltsY ltsZ y z -> 
  forall x2 a,
  weak ltsX x (Some a) x2 ->
  exists y2, weak ltsY y (Some a) y2 -> weak_sim ltsX ltsY x2 y2 /\
  exists z2, weak ltsZ z (Some a) z2 -> weak_sim ltsY ltsZ y2 z2 
  .
Proof.
  intros x1 y1 z1.
  intros Hxy Hyz.

  inversion Hxy as [Hxy_simF]; subst. 
  inversion Hxy_simF as [Hxy_weak Hxy_tau]; subst.
  
  intros x2 a.

  intros Hx.
  Search weak.
  apply Hxy_weak in Hx.

  inversion Hx as [y2 Hy_xy].
  eexists y2.
  intros Hy.
  split.
  - apply Hy_xy.
  - 
    inversion Hyz as [Hyz_simF]; subst. 
    inversion Hyz_simF as [Hyz_weak Hyz_tau]; subst.
    apply Hyz_weak in Hy.
    inversion Hy as [z2 Hz_yz].
    eexists z2.
    intros Hz.
    apply Hz_yz.
Qed.

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
  {
    intros x2 ax Hx.

    apply Hxy_weak in Hx.
    inversion_clear Hx as [y2 Hy_xy]; subst.
    induction Hy_xy as [Hy Hxy2].

    apply Hyz_weak in Hy.
    inversion_clear Hy as [z2 Hz_yz]; subst.
    induction Hz_yz as [Hz Hyz2].

    eexists z2.
    split.
    - apply Hz.
    - About CH. apply (@CH x2 y2 z2 Hxy2 Hyz2). Guarded.
  }
  {
    intros x2 Hx. eexists ?[z2]. split.
    { constructor. }
    { apply (@CH x2 y1 z1); [| apply Hyz]. Guarded.

      apply Hxy_tau in Hx;
      inversion_clear Hx as [y2 Hy]; subst;
      destruct Hy as [Hy Hxy2].
      
      inversion Hy; subst.
      { apply Hxy2. }
      { 
        (*****************************)
        (* weak_sim ltsX ltsY x1 y1 *)
        (* silent1 ltsX x1 x2 *)
        (* silent  ltsY y1 y2 *)
        (* weak_sim ltsX ltsY x2 y2 *)

        (* weak_sim ltsX ltsY x2 y1 *)
        (*****************************)
        admit.

      } 
    }

  }
Admitted.


Lemma _asym1 (X Y A : Type) (ltsX : LTS X A) (ltsY : LTS Y A) 
: forall x1 x2 y1,
  weak_sim ltsX ltsY x1 y1  /\ 
  silent1 ltsX x1 x2        ->
  exists y2,
  weak_sim ltsX ltsY x2 y2  /\
  silent  ltsY y1 y2 ->

  weak_sim ltsX ltsY x2 y1.
Proof.
  intros x1 x2 y1. 
  intros H; destruct H as [Hxy1 Hx].

  inversion Hxy1 as [Hxy1_simF]; subst;
  inversion_clear Hxy1_simF as [_ Hxy1_tau]; subst.
  apply Hxy1_tau in Hx.
  inversion Hx as [y2]; subst.
  eexists y2.
Admitted.
  

Lemma _asym2 (X Y A : Type) (ltsX : LTS X A) (ltsY : LTS Y A) 
: forall x1 x2 y1 y2,
  weak_sim ltsX ltsY x1 y1  /\ 
  weak_sim ltsX ltsY x2 y2  /\ 
  silent1 ltsX x1 x2        /\ 
  silent  ltsY y1 y2 ->

  weak_sim ltsX ltsY x2 y1.
Proof.
  intros x1 x2 y1 y2.

  intros H; destruct H as [Hxy1 H]; 
  destruct H as [Hxy2 H]; 
  destruct H as [Hx Hy].

  inversion Hxy1 as [Hxy1_simF]; subst;
  inversion_clear Hxy1_simF as [_ Hxy1_tau]; subst.
  
  apply Hxy1_tau in Hx.
  (* inversion Hx; subst. *)
  (* destruct Hx as []. *)
  (* -   *)
Admitted.


    
(*****************************)

    apply Hxy_tau in Hx.
    inversion_clear Hx as [y2 Hy]; subst.
    destruct Hy as [Hy Hxy2].
    eexists ?[z2]. split. constructor.
    apply (@CH x2 y1 z1); [| apply Hyz]. Guarded.

    (*****************************)
    (* weak_sim ltsX ltsY x1 y1 *)
    (* weak_sim ltsX ltsY x2 y2 *)
    (* silent1 ltsX x1 x2 *)
    (* weak_sim ltsX ltsY x2 y1 *)
    (*****************************)

    inversion Hy as [| y1b _y2 Hy1b Hyb2]; subst.
    { apply Hxy2. }
    { (* apply clos_rt_t in Hyb2. *)



    }


    { admit.
      
    }
    { apply Hyz. }


    destruct Hy as [| y1b y2 Hyb H2b ]; subst.
    { 
      eexists ?[z2]. split. constructor.
      About CH. apply (@CH x2 y1 z1 Hxy2 Hyz). Guarded.
    }
    {
      (* eexists ?[z2]. split; [constructor |]. *)
      (* About CH. apply (@CH x2 y2 z1 Hxy2). Guarded. *)


      assert (silent1 ltsY y1 y2) as Ha.
      { (* tau ltsY y1 y1b /\ clos_refl_trans_1n Y (tau ltsY) y1b y2 *)
        admit.
      }
      {
        apply Hyz_tau in Ha.
        inversion Ha as [z2 [Hz Hyz2]]; subst.
        eexists z2. 
        split.
        - apply Hz.
        - About CH. apply (@CH x2 y2 z2 Hxy2 Hyz2). Guarded.
      }
    }
  }
Admitted.

  
Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.

Lemma weak_bisim_refl (M A : Type) (ltsM : LTS M A)
: forall m, weak_bisim ltsM ltsM m m.
Proof. constructor 1; try apply weak_sim_refl. Qed.

Lemma weak_bisim_sym (M N A : Type) (ltsM : LTS M A) (ltsN : LTS N A) 
: forall m n,
  weak_bisim ltsM ltsN m n ->
  (weak_sim ltsM ltsN m n -> weak_sim ltsN ltsM n m) /\
  (weak_sim ltsN ltsM n m -> weak_sim ltsM ltsN m n).
Proof.
  intros m n Hb. split.
  - destruct Hb as [_ Hnm]. intros _. apply Hnm.
  - destruct Hb as [Hmn _]. intros _. apply Hmn.
Qed.

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

  Example m1 := (tact None (tact (Some TheAction1) (tact None tend))).
  Example n1 := (tact (Some TheAction1) tend).

  Goal weak_sim termLTS termLTS m1 n1.
  Proof. 
    (* unfold m1; unfold n1.  *)
    (* cofix CH.  *)
    apply In_sim.
    apply Pack_sim.
    { intros m2 a Hw. destruct Hw. destruct H. destruct H.
      destruct pre0.
      inversion str0.
      inversion H; subst. destruct pre0; [| inversion H0].
      clear H. inversion str0. subst. clear str0. inversion post0. subst.
      exists tend. split.
      - do 2 eexists. do 2 constructor. 
      - 
      
      
      }
    Print Pack_sim.
    constructor. split. 

    (**** issue with picking constructor from Pack_sim ******)

    (** below asserts the kind of action, but both constructors are shown **)

    { intros m2 a1.
      replace a1 with TheAction1.
      { intros H1; eexists.
        split. 
        Print weak.
        - unfold weak. eexists; eexists. constructor.
          + constructor.
          + apply do_act.
          + constructor.
        - constructor. Print simF.
          (* how to pick the constructor? surely only one is needed *)
          apply sim_tau. 
          constructor.
          (* *)
          
          { intros m3 a2.
            replace 

          }
      

      }

    }

    (** below shows for induction on action, which includes impossible cases **)

    (* { intros m2 a1 H1.
      inversion_clear H1; subst.
      inversion H; subst.
      eexists.

      induction a1.
      { split.
        - unfold weak. eexists; eexists.
          constructor.
            + constructor.
            + apply do_act.
            + constructor.
        - (* m2 must be tend or have silent transitions until tend *)
        
        constructor; constructor.
          + intros m3 a2 h2. induction a2.
            {
              eexists. constructor.
              - unfold weak; eexists; eexists; constructor.
                + constructor.
                + apply do_act.
                + constructor.
            }  
      }
    }

      revert Hw1. 
      (* unfold m1 in Hw1. *)
      induction a1.
      + eexists. split.
        *  
       *)
      


  Example m1 := (tfix (tact (Some TheAction1) trec)).
  Example n1 := (tact (Some TheAction1) (tfix (tact (Some TheAction1) trec))).

  Compute (weak_sim termLTS termLTS m1 n1).
  Compute (In_sim termLTS termLTS m1 n1).
  Compute (out_sim termLTS termLTS m1 n1).

  Goal weak_sim termLTS termLTS m1 n1.
  Proof.
    cofix CH.
    constructor.
    split.
    { intros m2 a1 Hw1.

      (* inversion_clear Hw1; subst. *)
      (* inversion H; subst. *)

      induction a1.
      {
        inversion_clear Hw1; subst.
        inversion H; subst.
        compute in H0.

        eexists.
        split.
        - unfold weak; eexists; eexists.
          constructor.
          + constructor.
          + apply do_act.
          + constructor.
        - constructor. constructor.
          + intros m3 a2 Hw2.
             
        eapply do_act.  

      }
      constructor.
      eexists. 
      unfold weak.

    }
    About simF.
    unfold simF.



    constructor.
    {
      intros m2 a1 Hw1.
      eexists.
      induction a1; split.
      { unfold weak; eexists; eexists. constructor.
        - constructor.
        - apply do_act.
        - constructor. }
      { constructor; constructor.
        - intros m3 a2 Hw2; eexists.
          induction a2; split.
          + { unfold weak; eexists; eexists. constructor.
              - constructor.
              - apply do_fix. constructor.
              - constructor. }
          + { constructor; constructor.
        - intros m4 a3 Hw3; eexists.
          induction a3; split.
          + { unfold weak; eexists; eexists. constructor.
              - constructor.
              - apply do_fix. constructor.
              - constructor. }
          + { constructor; constructor. }  }  }


      split.
      -
      
        + 
          unfold weak. 
          eexists; eexists.
          constructor.
          * constructor.
          * apply do_act1.
          * constructor.
        +
          unfold weak.
          eexists; eexists.
          constructor.
          *
            constructor.
          *
            apply act. 

            discriminate. 
            apply do_act2.
          * constructor. 
        + apply do_act2.

        unfold weak.
        eexists; eexists.
        Print weak_tr.
        constructor.
        + constructor.
        +
          

    }



    (* unfold test1_m. *)
    (* unfold test1_n. *)
    (* cofix CH. *)
    constructor.
    split.
    (* About weak_sim. *)
    (* About simF. *)
    - intros m' a Hm.
      eexists.
      constructor.
      + unfold weak.
        eexists.
        eexists.
        constructor 1.
        * constructor 1.
        * 
          eapply do_act1. 
          constructor 1. 
      
      
      (* inversion_clear Hm; subst. *)
      (* inversion H; subst. *)
      exists n'. 
      split.
      + unfold weak.
        About weak_tr.
        eexists.
        eexists.
        constructor.
        * constructor.
        * 
          eapply do_act1. 
          constructor. 

        (* intros post_m pre_m'. *)
        (* exists test1_m. *)
        (* exists m'. *)
        constructor.
        * unfold silent. 


        (* eexists. *)
        intros z t.
        apply do_act1. 
        unfold test1_n.
      


    (* split.
      + unfold weak. unfold weak in tr. constructor. *)
  Qed.
  
  Goal weak_sim termLTS termLTS
    (tfix (tpar TheAction1 TheAction2 trec))
    (tact TheAction1 (tfix (tact TheAction1 trec))).
  Proof.
    
  Qed.

End BisimTest1.



Lemma glued_sim : weak_sim (lts g1 action) bigstep. 
  
Lemma glued_bisim (g1 g2 : composition) : 





Section Definitions.
  Context
    {M : Type}        (* term of lts 1 *)
    {N : Type}        (* term of lts 2 *)
    {A : Type}        (* actions of lts *)
    (TAU : A -> Prop) (* determine if [A] is silent -- for weak *)
    (LTS1 : M -> A -> M -> Prop)
    (LTS2 : N -> A -> N -> Prop).

  Definition simF (Sim : M -> N -> Prop) (s : M) (t : N) : Prop :=
    forall s' a,
      LTS1 s a s' ->
      exists t',
        LTS2 t a t' /\ Sim s' t'.

  CoInductive sim (s : M) (t : N) : Prop :=
    In_sim { out_sim :  simF sim s t }.


  (* weak transitions -- refl & trans closure of tau transitions *)
  (* prop holds *)
  Inductive weak_refltr1 (lhs : Prop) (s : M) : Prop :=
  | ltau1 : forall a s',
      (TAU a -> True) /\ LTS1 s a s' /\ (lhs -> True) ->
      weak_refltr1 lhs s' -> weak_refltr1 lhs s
  | rtau1 : forall a s',
      (TAU a -> True) /\ LTS1 s a s' /\ (lhs -> False) ->
      weak_refltr1 lhs s' -> weak_refltr1 lhs s
  | act1 : forall a s',
      (TAU a -> False) /\ LTS1 s a s' /\ (lhs -> True) ->
      weak_refltr1 False s' -> weak_refltr1 lhs s
  | none1 : forall a s', LTS1 s a s'
  .

  (* Inductive weak_comptr1 (s : M) (a : A) : Prop :=
  | comp : forall s',
      weak_refltr1 True  *)


  (* same as above, but for lts2 *)
  Inductive weak_refltr2 (lhs:Prop) (s : N) : Prop :=
  | ltau2 : forall a s',
      (TAU a -> True) /\ LTS2 s a s' /\ (lhs -> True) ->
      weak_refltr2 lhs s' -> weak_refltr2 lhs s
  | rtau2 : forall a s',
      (TAU a -> True) /\ LTS2 s a s' /\ (lhs -> False) ->
      weak_refltr2 lhs s' -> weak_refltr2 lhs s
  | act2 : forall a s',
      (TAU a -> False) /\ LTS2 s a s' /\ (lhs -> True) ->
      weak_refltr2 False s' -> weak_refltr2 lhs s
  | none2 : forall a s', LTS2 s a s'
  .

  (* Definition weak_tr () *)


  (* | rtau1 : forall s a s',
      TAU a /\ LTS1 s a s' -> *)



  (* weak sim *)
  (* Definition wsimF (Sim : M -> N -> Prop) (s : M) (t : N) : Prop :=
    forall s' a,
    LTS1 s a s' ->
      () *)


  (* CoInductive wsim (s : M) (t : N) : Prop :=
    In_wsim { out_wsim :  wsimF wsim s t }. *)


End Definitions.

Lemma sim_refl (M A : Type) (LTS : M -> A -> M -> Prop) (m : M)
  : sim LTS LTS m m.
Proof.
  revert m.
  cofix CH.
  intros m.
  constructor.
  intros m' a tr.
  exists m'.
  split.
  - exact tr.
  - apply CH. Guarded.
Qed.


(* weak transition *)
Definition tau_tr (a:CADP.action) : Prop := 
  match a with 
  | SILENT => True
  | _ => False 
  end.

Lemma weak_bisim 
  (* changed from [Set] to [Type] due to error. *) 
  (* -- uncertain of impact, but recall D.CP. preferring [Set] *)
  (M N A : Type) 
  (LTS1 : M -> A -> M -> Prop)
  (LTS2 : N -> A -> N -> Prop)
  (m : M) (n : N) : weak_sim lts bigstep tau_tr g1 g1.
Proof.
  (* copied from [sim_refl] *)
  revert m n.
  cofix CH.
  intros m n.
  constructor. 
  unfold simF. (* this was omitted in [sim_refl] *) 
  intros m' a tr. 
  exists m'. (* *)
  split.
  (* - exact tr. *)
  (* - apply CH. Guarded. *)



(* strong bisim, based upon [sim_refl] *)
(* ! aborted since we are showing weak bisim for the existing example. *)
Lemma bisim 
  (* changed from [Set] to [Type] due to error. *) 
  (* -- uncertain of impact, but recall D.CP. preferring [Set] *)
  (M N A : Type) 
  (LTS1 : M -> A -> M -> Prop)
  (LTS2 : N -> A -> N -> Prop)
  (m : M) (n : N) : sim lts bigstep g1 g1.
Proof.
  (* copied from [sim_refl] *)
  revert m n.
  cofix CH.
  intros m n.
  constructor. 
  unfold simF. (* this was omitted in [sim_refl] *) 
  intros m' a tr. 
  exists m'. (* *)
  split.
  (* - exact tr. *)
  (* - apply CH. Guarded. *)
Abort.






(* example from online *)












CoInductive stream :=
  | cons : nat -> stream -> stream.

Definition sunf s :=
  match s with cons n s' => cons n s' end.

Lemma sunf_eq : forall s, s = sunf s.
Proof.
  destruct s; auto.
Qed.


CoFixpoint enumerate n : stream :=
  cons n (enumerate (S n)).

CoFixpoint map f s : stream :=
  match s with cons n s' => cons (f n) (map f s') end.

Inductive seq_gen seq : stream -> stream -> Prop :=
  | _seq_gen : forall n s1 s2 (R : seq s1 s2 : Prop), seq_gen seq (cons n s1) (cons n s2).
Hint Constructors seq_gen.

CoInductive seq : stream -> stream -> Prop :=
  | seq_fold : forall s1 s2, seq_gen seq s1 s2 -> seq s1 s2.

Theorem example : forall n, seq (enumerate n) (cons n (map S (enumerate n))).
Proof.
  cofix CIH.
  intros. apply seq_fold.
  (* pattern (enumerate n) at 1; rewrite sunf_eq; simpl. *)
  rewrite sunf_eq at 1; simpl.
  constructor.
  rewrite (sunf_eq (enumerate n)). simpl.
  rewrite (sunf_eq (map _ _)). simpl.
  apply CIH.
Qed.








