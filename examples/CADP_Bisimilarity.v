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

(* Lemma tau_trans (M A: Type) (lts : LTS M A): forall x y z, 
  tau lts x y -> tau lts y z -> tau lts x z.
Proof.
  intros x y z Hxy Hyz.
  apply clos_rt1n_step in Hxy; apply clos_rt1n_rt in Hxy.
  apply clos_rt1n_step in Hyz; apply clos_rt1n_rt in Hyz.
  apply (@clos_rt_t M (tau lts) x y z Hxy Hyz). 
  *)
  

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

(* Lemma w (M A : Type) (ltsM : LTS M A) (m : M)
: weak_sim ltsM ltsM m m ->
  forall y z,
  tau ltsM m y -> clos_trans_1n M (tau ltsM) y z -> tau ltsM m z.
Proof.
  revert m; intros x Hxx y z Hxy Hyz.
  destruct Hyz as [y Hyz | ]. 
  - apply clos_rt1n_step. apply H.  *)

(* Lemma w (M A : Type) (ltsM : LTS M A) :

forall x y z,
clos_refl_trans_1n M (tau ltsM) x y -> clos_refl_trans_1n M (silent ltsM) y z -> clos_refl_trans_1n M (silent ltsM) x z
.
Proof.
  intros x y z Hxy Hyz.
  Print clos_refl_trans_1n.

  (* apply (@rt1n_trans M (tau ltsM) x y) in Hxy. *)
  apply rt1n_trans in Hxy. *)


Lemma weak_sim_step_none (M A : Type) (ltsM : LTS M A) 
: forall x1 y1, 
  weak_sim ltsM ltsM x1 y1 ->
  forall x2, silent1 ltsM x1 x2 ->
  exists y2, silent ltsM y1 y2 /\ weak_sim ltsM ltsM x2 y2.
Proof.
  intros x1 y1 Hw1.
  inversion_clear Hw1; subst. inversion out_sim0; subst. apply sim_tau0.
Qed.

Lemma weak_sim_step_some (M A : Type) (ltsM : LTS M A) 
: forall x1 y1, 
  weak_sim ltsM ltsM x1 y1 ->
  forall x2 a, weak ltsM x1 (Some a) x2 ->
  exists y2, weak ltsM y1 (Some a) y2 /\ weak_sim ltsM ltsM x2 y2.
Proof.
  intros x1 y1 Hw1.
  inversion_clear Hw1; subst. inversion out_sim0; subst. apply sim_weak0.
Qed.

Lemma weak_sim_step (M N A : Type) (ltsM : LTS M A) 
                                   (ltsN : LTS N A) (m1 : M) (n1 : N)
: weak_sim ltsM ltsN m1 n1 ->
  ( forall m2 a, weak ltsM m1 (Some a) m2 ->
    exists n2, weak ltsN n1 (Some a) n2 /\ weak_sim ltsM ltsN m2 n2)
  \/
  ( forall m2, silent1 ltsM m1 m2 ->
    exists n2, silent ltsN n1 n2 /\ weak_sim ltsM ltsN m2 n2 ).
Proof. 
  intros Hw1. left. 
  inversion_clear Hw1; subst. inversion out_sim0; subst. apply sim_weak0.
Qed.

Lemma weak_sim_refl (M A : Type) (ltsM : LTS M A) (m : M)
: weak_sim ltsM ltsM m m.
Proof.
  revert m; cofix CH; intros m1.
  apply In_sim. apply Pack_sim.
  - intros m2 a Hm; eexists m2; split.
    + apply Hm.
    + apply CH. Guarded.
  - intros m2 Hm; eexists m2; split. 
    + apply silent1_step. apply Hm.
    + apply CH. Guarded.
Qed.

Lemma weak_sim_trans (X Y Z A : Type) 
(ltsX : LTS X A) (ltsY : LTS Y A) (ltsZ : LTS Z A) 
: forall x y z,
  weak_sim ltsX ltsY x y -> 
  weak_sim ltsY ltsZ y z -> 
  weak_sim ltsX ltsZ x z.
Proof.
Admitted.

(***********************************************)


Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.

Lemma weak_bisim_refl (M A : Type) (ltsM : LTS M A) (m : M)
  : weak_bisim ltsM ltsM m m.
Proof. constructor 1; try apply weak_sim_refl. Qed.


Lemma weak_bisim_sym (M N A : Type) (ltsM : LTS M A) 
                                    (ltsN : LTS N A) (m : M) (n : N)
: weak_bisim ltsM ltsN m n ->
  (weak_sim ltsM ltsN m n -> weak_sim ltsN ltsM n m) /\
  (weak_sim ltsN ltsM n m -> weak_sim ltsM ltsN m n).
Proof.
  intros Hb. split.
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




Lemma weak_sim_trans (M A : Type) (ltsM : LTS M A) (x y z : M)
: weak_sim ltsM ltsM x y -> 
  weak_sim ltsM ltsM y z -> 
  weak_sim ltsM ltsM x z.
Proof.
  revert x y z; cofix CH; intros x1 y1 z1.
  intros Hxy Hyz.
  apply In_sim. apply Pack_sim. 
  { intros x2 a Hx.

    destruct Hyz as [Hyz]; destruct Hyz as [Hyz_weak Hyz_tau].
    (* apply Hyz_weak. *)


    (* inversion_clear Hx; subst; inversion_clear H; subst; inversion H0; subst.  *)
    (* inversion pre0; subst; inversion post0; subst. *)


    About sim_weak.
    apply (@sim_weak M M A ltsM ltsM _ x1 _).
    {
      admit.
    }
    - apply Hx.
  }
  { intros x2 Hx. 
    (* apply silent1_step in Hx. *)
    }
    apply Hyz_weak.


(* Lemma w (M A : Type) (ltsM : LTS M A) (x y : M)
: weak_sim ltsM ltsM x y ->
  weak *)


    (* apply weak_sim_refl in Hxy.  *)
    (* apply weak_sim_refl in Hx.  *)

    apply Hxy.
    destruct Hxy as [Hxy]; destruct Hxy as [Hxy_weak Hxy_tau].


    apply Hxy_weak.

    (* destruct Hx; destruct H; destruct H.  *)
    inversion_clear Hx; subst; inversion_clear H; subst; inversion H0; subst. 
    (* inversion pre0; subst. inversion post0; subst. *)


    apply Hx.

    apply Hxy_weak.
    
    inversion Hxy; subst.

  }




  destruct Hxy as [out_Hxy].
  destruct out_Hxy as [Hxy_weak Hxy_tau].



  (* apply In_sim in out_Hxy.  *)


  (* apply Pack_sim in out_Hxy.  *)















  (* revert x y z; intros x1 y1 z1; cofix CH. *)
  intros Hxy Hyz.
  (* inversion Hxy; subst; inversion Hyz; subst. *)
  (* inversion out_sim0; subst. *)

  (* destruct Hxy as [Hxy]; destruct Hxy as [Hxy_weak Hxy_tau]. *)
  (* destruct Hyz as [Hyz]; destruct Hyz as [Hyz_weak Hyz_tau]. *)
  (* compute in Hxy_weak. *)

  apply In_sim. apply Pack_sim. 
  
  - intros x2 ax Hwx. 
    About sim_weak.
    (* eexists ?[z2].  *)
    apply (@sim_weak M M A ltsM ltsM (weak_sim ltsM ltsM) z1 _).
    (* apply (@sim_weak M M A ltsM ltsM (weak_sim ltsM ltsM) x1 z1). *)
    (* apply (@sim_weak M M A ltsM ltsM (weak_sim ltsM ltsM) x2 z1). *)
    + apply Pack_sim; apply weak_sim_refl.
    +
      inversion_clear Hwx; subst.
      inversion_clear H; subst.
      unfold weak.
      apply H0. 
      unfold weak_tr. 
      apply Pack_weak.
      
      Print weak.
      unfold weak. 
      apply Pack_weak.

      * intros z2 az Hwz. 
    
    
    eexists ?[z2]. 
    split.
    + apply sim_weak. 
  
    destruct Hxy as [Hxy]; destruct Hxy as [Hxy_weak Hxy_tau].
    destruct Hyz as [Hyz]; destruct Hyz as [Hyz_weak Hyz_tau].

    

    inversion_clear Hxy; subst. inversion_clear out_sim0; subst.
    apply sim_weak0 in out_sim0.

    apply Pack_weak in .

    (* apply sim_weak in Hyz. *)
    (* apply sim_weak in Hyz. *)
  
    apply out_sim.
    apply sim_weak.
    intros x2 a Hwx.

    inversion_clear Hxy; subst. inversion out_sim0; subst.
    
    
    apply sim_weak0 in out_sim0.
    revert x2 a Hwx. apply sim_weak0.




    apply weak_sim_step_some.


    apply clos_rt1n_step in Hxy.



    



    apply weak_sim_step_some.

    admit.

  - intros x2.
    apply weak_sim_step_none.


    (* revert Hxy Hyz. *)
    apply CH. apply Hxy. apply Hyz. Guarded.
    (* apply CH. Guarded. *)

    inversion_clear Hx; subst; inversion_clear H; subst.
     
    destruct Hx as [x1b Hx]; destruct Hx as [x2a Hx]. inversion Hx; subst.
    destruct pre0 as [| x1a x1b FFF].
    + 
    (* eexists ?[z2]. split.
      * admit.
      *  *)
      admit.
    +  
        (* apply weak_sim_step. *)
  
    apply weak_sim_step_some. 
    apply CH. Guarded.



  { 
    
    apply weak_sim_step_some.
     
    (* revert Hxy Hyz.  *)
    apply CH. Guarded.
  }
  { apply weak_sim_step_none. 
    revert Hxy Hyz. 
    apply CH. Guarded.
  }



  transitivity.

  


  inversion out_sim0; subst. destruct sim_weak0.
  destruct H.
  apply In_sim in H.
  Print weak_sim.
  
  apply Pack_weak in H.


  compute.
  unfold silent1.
  
  
  intros Hw1 xH yH.


Lemma weak_sim_step_none (M A : Type) (ltsM : LTS M A) 
: forall x1 y1, 
  weak_sim ltsM ltsM x1 y1 ->
  (* weak ltsM x1 None x2 ->  *)
  forall x2, silent1 ltsM x1 x2 ->
  exists y2, silent ltsM y1 y2 /\ weak_sim ltsM ltsM x2 y2.
Proof.
  intros x1 y1 Hw x2 xHw.
  eexists ?[y2]. split. 
  - apply silent1_step. 

    inversion Hw.



  - revert Hw. cofix CH.  



  inversion xHw as [x1b]; inversion_clear H as [x2a]; revert H0; intros xHw'.
  Print weak_tr.
  
  
  destruct xHw as [x1b xHw]. destruct xHw as [x2a xHw]. destruct xHw.
  eexists ?[y2]. 

  Print Pack_sim.

  split.
  - admit.
  - inversion Hw1; subst. destruct out_sim0.
  
  
  split.
  { admit.

  }
  { apply In_sim. apply Pack_sim.
    { intros x3 act2 Hx2. eexists ?[y3]. split.
      -  

    }
  }
  
  { intros x

  }

  destruct Hx as [x1b Hx]; destruct Hx as [x2a Hx]. 
  destruct Hx as [xPre xStr xPost].

  intros Hw. destruct Hw as [Hwsim].
  destruct Hwsim as [xHweak xHtau]. 


Print silent.
Print silent1.
Print weak_sim.
Print Pack_sim.
Lemma weak_sim_trans (M A : Type) (ltsM : LTS M A) (x y z : M)
  : weak_sim ltsM ltsM x y -> 
    weak_sim ltsM ltsM y z -> 
    weak_sim ltsM ltsM x z.
Proof.
  revert x y z; cofix CH; intros x1 y1 z1.
  intros Hxy Hyz.
  (* apply In_sim in Hxy.  *)
  (* apply Pack_sim in Hxy. *)
  destruct Hxy.
  destruct Hyz.


  
  apply In_sim; apply Pack_sim.
  { intros x2 axy Hx. eexists ?[z2]. 
    destruct Hx as [x1b Hxt]. destruct Hxt as [x2a Hx]. destruct Hx. 
    destruct pre0; destruct post0; split.
    - unfold weak; eexists; eexists. apply Pack_weak.
      + constructor.
      + compute.
    
    intros z1b z2a.
  }



(* Lemma weak_sim_trans (M N A : Type) (ltsM : LTS M A) 
                                    (ltsN : LTS N A)
  : forall m1 n1, weak_sim ltsM ltsN m1 n1 -> 
    forall m2 act, weak ltsM m1 act m2 ->
    exists n2, weak ltsN n1 act n2 /\ weak_sim ltsM ltsN m2 n2.
Proof.
  intros m1 n1 Hw.
  inversion Hw.
  intros m2 act Hwm.
  induction act.
  { destruct Hwm. destruct H. destruct H. destruct pre0.
    - eexists. split.
      + unfold weak. eexists; eexists. constructor.
        * constructor.
        * 
  }
  (* Print weak.
  eexists. split.
  -   *)
  
  destruct Hwm. destruct H. destruct H.
   (* destruct pre0. *)
  induction act.
  -
    destruct pre0.
   
    (* apply In_sim. *)
    (* apply Pack_sim. *)
    apply sim_weak.
   
  inversion Hwm. inversion H. inversion H0.
Admitted. *)




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








