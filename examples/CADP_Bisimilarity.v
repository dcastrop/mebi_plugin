Require Import MEBI.Examples.CADP.

(* https://rocq-prover.org/doc/v8.9/stdlib/Coq.Relations.Relation_Operators.html *)
Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Import Operators_Properties.
(* Require Import FunInd. *)

Set Primitive Projections.

Section Definitions.
  Context (M : Type) (A : Type).
  Definition LTS : Type := M -> option A -> M -> Prop.
  (* tau-labelled transition *)
  Definition tau (R : LTS) : relation M := fun (x y : M) => R x None y.
  About tau.
  Check tau.
  Compute tau.
  (* Functional Scheme tau_ind := Induction for tau Sort Type. *)
End Definitions.
Arguments tau {M A} R.

Section WeakTrans.
  Context {M : Type} {A : Type} (lts : LTS M A).

  (* trace of tau-labelled transitions *)
  Definition silent : relation M := clos_refl_trans_1n M (tau lts).
  Definition silent1 : relation M := clos_trans_1n M (tau lts).

  Lemma silent1_step : forall m m', 
    silent1 m m' -> silent m m'.
  Proof.
    intros m1 m2 H1.
    induction H1 as [ m1 m2 H1 | ].
  - Admitted.


  (* Lemma silent_refl : forall m1 m2,
    (* silent m1 m2 /\ silent1 m1 m2 -> *)
    silent m1 m2 -> silent1 m1 m2 ->
    (* silent1 m1 m2 -> silent m1 m2 -> *)
      exists m3, 
      silent m1 m3 /\ silent m3 m2.
  Proof.
    (* intros m1 m2 [H H1]. *)
    (* intros m1 m2 H1 H. *)
    intros m1 m2 H H1.
    eexists.
    split.
    - constructor.
    - apply H.
  Qed.
     *)



  Print clos_refl_trans_1n.

  (* Lemma silent_pred : forall m1 m2,
    silent m1 m2 -> m1 = m2 \/ silent1 m1 m2.
  Proof.
    intros m1 m2 H.
    constructor. *)
    

  (* Lemma silent_tau : forall m m', silent m m' -> tau lts m m'.
  Proof.
    intros m m'.
    compute.
    destruct H as [ | H1 ].
    - assumption. *)
    
  (* Lemma silent_tau : forall m m', tau lts m m' -> silent1 m m' -> silent m m'.
  Proof.
    intros m m' Ht H1.
    (* compute in Ht. *)
    (* revert Ht. *)
    (* revert H1. *)
    (* compute. *)
    edestruct H1 as [ m' Ht' | m' H1' ].
    - intros H'. *)

  Check tau.
  About tau.
  (* Lemma silent1_step : forall m m', 
    silent1 m m' ->
    exists m'',
    silent m m''.
  Proof.
    intros m m' H1.
    eexists.
    constructor.
  Qed. *)

  (* Lemma silent1_step : forall m m', 
    silent1 m m' -> silent m m'.
  Proof.
    intros m m'.
    compute.
    intros H.
    transitivity H.
    compute.
    


    destruct H1 as [ m' H1 | m' H2 ].
    - 

    (* eexists. *)
    constructor.
  Qed. *)
  
    (* compute. *)
    (* Print clos_trans_1n. *)
    (* intros H1.
    induction H1.
    eexists m. (* this doesnt always make sense? *)
    constructor. *)
  (* Admitted. *)
  (* Qed. *)

  (* Lemma silent_step : forall m m', 
    silent m m' ->
    silent m' m \/ 
    silent1 m m'.
  Proof.
    intros m m' H1.
    constructor.
  Qed. *)

  Lemma silent_step : forall m, exists m',
    silent m m' /\ (silent1 m m' -> 
                    exists m'', 
                      (* ((silent1 m m'' -> silent m'' m') \/ 
                       (silent m m'' -> silent1 m'' m')) /\ *)
                      silent m m'' -> silent m'' m').
  Proof.
    intros m.
    eexists m. (* this doesnt always make sense? *)
    split.
    - constructor.
    - intros H1. 
      eexists m. (* this doesnt always make sense? *)
      constructor.
  Qed.

  (* Lemma rt_from_t : 
    forall R m m',
    inclusion (clos_refl_trans_1n R m m') (clos_trans_1n R m m'). *)
  
  Lemma clos_rt_from_t : forall x z,
    clos_trans_1n M (tau lts) x z ->
    exists y, 
      clos_refl_trans_1n M (tau lts) x y /\ clos_trans_1n M (tau lts) y z.
  Proof.
    intros x z Hxz.
    eexists.
    split.
    - constructor.
    - apply Hxz.
  Qed.

  Lemma expand_silent1 : forall m m', 
    silent1 m m' ->
    exists m'', 
      silent m m'' /\ silent1 m'' m'.
  Proof.
    intros m m' H1.
    eexists.
    split.
    - constructor.
    - apply H1.
  Qed.

  (* Lemma expand_silent1_to_silent : forall m m', 
    silent1 m m' ->
    exists m'', 
      silent m m'' /\ silent m'' m'.
  Proof.
    intros m m' H1.
    eexists.
    split.
    - constructor.
    - 
      apply H1.
  Qed. *)

  (* Lemma silent_from_silent1 : forall m m', silent1 m m' -> silent m m'.
  Proof.
    intros m m' H1.
    eapply expand_silent1 in H1.
    (* revert H1. *)
    eapply silent_step with (m:=m) in H1.
    



    Check silent1_step.
    eapply silent1_step in H1.
    (* specialize H1 with (m'':=m'). *)
    (* eapply silent1_step with (m':=m') in H1. *)
    (* revert H1. *)
    destruct H1.
    revert H.
    
    compute.
    (* eexists m' in H1. *)
    split.
    eexists m'.

    eapply expand_silent1.

    unfold silent1. unfold silent.
    constructor.
  Qed. *)
    

  (*  x ==> pre_str ->^a post_str ==> y *)
  Record weak_tr (x z : M) a (t y : M) : Prop :=
    Pack_weak { pre : silent x z; str : lts z a t; post : silent t y }.
  Definition weak (x : M) a (y : M) : Prop :=
    exists z t, weak_tr x z a t y.
End WeakTrans.

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

  CoInductive weak_sim (s : M) (t : N) : Prop
    := In_sim { out_sim : simF weak_sim s t }.
End WeakSim.

Lemma weak_sim_refl (M A : Type) (ltsM : LTS M A) (m : M)
  : weak_sim ltsM ltsM m m.
Proof.
  revert m.
  cofix CH.
  intros m1.
  constructor 1.
  split.
  - intros m2 a Hm.
    eexists m2.
    split.
    + apply Hm.
    + apply CH. Guarded.
  - intros m2 Hm.
    eexists m2.
    split. 
    + apply silent1_step. apply Hm.
    + apply CH.
Qed.  

Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.

Lemma weak_bisim_refl (M A : Type) (ltsM : LTS M A) (m : M)
  : weak_bisim ltsM ltsM m m.
Proof.
  constructor 1; try apply weak_sim_refl.
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
    | do_act : forall a t, termLTS (tact a t) 
                                  a 
                                  t

    | do_parl : forall a b t, termLTS (tpar a b t) 
                                     a 
                                     (tact b t)

    | do_parr : forall a b t, termLTS (tpar a b t) 
                                     b 
                                     (tact a t)


    | do_fix : forall a t t',
        termLTS (subst (tfix t) t) a t' ->
        termLTS (tfix t) a t'
    .

  Example m1 := (tfix (tact (Some TheAction1) trec)).
  Example n1 := (tact (Some TheAction1) (tfix (tact (Some TheAction1) trec))).

  Goal weak_sim termLTS termLTS m1 n1.
  Proof.
    cofix CH.
    constructor.
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


