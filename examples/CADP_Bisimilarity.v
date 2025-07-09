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
  Print tau.
  Compute tau.
  Compute (relation M).
  (* Functional Scheme tau_ind := Induction for tau Sort Type. *)
End Definitions.
Arguments tau {M A} R.

Section WeakTrans.
  Context {M : Type} {A : Type} (lts : LTS M A).

  (* trace of tau-labelled transitions *)
  Definition silent : relation M := clos_refl_trans_1n M (tau lts).
  Definition silent1 : relation M := clos_trans_1n M (tau lts).

  Compute (tau lts).

  Lemma silent1_step : forall m m', 
    silent1 m m' -> silent m m'.
  Proof.
    intros m1 m2 H1.
    (* revert H1; compute; intros H1. *)
    induction H1 as [ m1 m2 H1 | ].
    - (* issue: Unable to find an instance for the variable y. *)
      (* get stuck -- can't apply H1 *)
      unfold silent. compute.
      (* while H1 can resolve as expected *)
      compute in H1.
  Admitted.

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

Check weak_sim.
Check In_sim.
Check out_sim.
Check simF.
Check sim_weak.
Check sim_tau.
(* Infinite case *)
(* Inductive StreamLTS 
{M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A)
: weak_sim ltsM ltsN M N -> option A -> weak_sim ltsM ltsN -> Prop :=
  | stream_step : 
      (* forall m1 m2 n1 n2 w1 x w2,  *)
      forall x m' n' w1 w2, 
      out_sim m n = 
        sim_weak 
        x m' n'
        (* consF x s' -> StreamLTS A s x s' *)
        (* sim_weak x m2 n2  *)
        (* sim_tau weak_sim m1 n1 x m2 n2  *)
        
        -> StreamLTS m n w1 x w2 
  . *)


  (* Inductive sim_step : 
  Prop :=
    | step_sim_weak :  *)

  (* Check (sim_weak weak_sim _ _). *)

  (* Inductive simLTS (s : M) (t : N) : 
  weak_sim s t -> option A -> weak_sim s t -> Prop :=
    | sim_step: forall a s' t', 
        out_sim s t = 
          sim_weak weak_sim s t -> 
          (* simF sim_weak  ->  *)
        (* out_sim s t = y ->  *)
        (* sim_weak weak_sim s t  s' a t' ->  *)
        simLTS s t a s' t'
    (* | tau_step: forall s s', out_sim s = sim_tau  *)
    . *)
(* End WeakSim. *)

Lemma weak_sim_refl (M A : Type) (ltsM : LTS M A) (m : M)
  : weak_sim ltsM ltsM m m.
Proof.
  revert m; cofix CH; intros m1.
  constructor.
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
    + apply CH. Guarded.
Qed.  

Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.

Lemma weak_bisim_refl (M A : Type) (ltsM : LTS M A) (m : M)
  : weak_bisim ltsM ltsM m m.
Proof. constructor 1; try apply weak_sim_refl. Qed.

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

  Example m1 := (tact None (tact (Some TheAction1) (tact None tend))).
  Example n1 := (tact (Some TheAction1) tend).

  Goal weak_sim termLTS termLTS m1 n1.
  Proof. 
    (* unfold m1; unfold n1.  *)
    cofix CH. 
    constructor. split. 

    (**** issue with picking constructor from Pack_sim ******)

    (** below asserts the kind of action, but both constructors are shown **)

    (* { intros m2 a1.
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

    } *)

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


