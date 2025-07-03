Require Import MEBI.Examples.CADP.

(* https://rocq-prover.org/doc/v8.9/stdlib/Coq.Relations.Relation_Operators.html *)
Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Import Operators_Properties.
(* Variable R1 : relation M. *)

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

  Print clos_refl_trans_1n.

  (* Lemma rt_from_t : 
    forall R m m',
    inclusion (clos_refl_trans_1n R m m') (clos_trans_1n R m m'). *)
  
  Lemma clos_rt_from_t : forall x y z,
    clos_trans_1n M (tau lts) x y ->
    clos_trans_1n M (tau lts) x z ->

  Print clos_rt_t.
  About clos_rt_t.
  Check clos_rt_t.
  (* Lemma clos_rt_t_1n : forall x y z, 
    clos_refl_trans_1n M (tau lts) x y -> 
    clos_trans_1n M (tau lts) y z -> 
    clos_trans_1n M (tau lts) x z.
  Proof.
    intros x y z Hxy Hyz.
    constructor.
    match z with
    | y => reflexivity
    | _ => 
    end. *)
  
  (* Lemma rt_from_t : 
    forall m m',
    clos_trans_1n M (tau lts) m m' -> 
    clos_refl_trans_1n M (tau lts) m m'.
  Proof.
    intros m m' H.
    Print clos_rt_t.
    About clos_rt_t.
    Check clos_rt_t.
    apply clos_rt_t.
    apply (@clos_rt_t M (tau lts) m m' _).
    Print clos_trans_1n.
    Print clos_refl_trans_1n.
    Print clos_rt_t. *)
    (* constructor. *)


    (* intros m m'. *)
    (* destruct clos_trans_1n. *)
    (* destruct clos_refl_trans_1n. *)
    (* constructor. *)



  (* Lemma silent_from_silent1 : forall m m', silent1 m m' -> silent m m'.
  Proof.
    intros m m' H. *)

    
    

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
    := In_sim { out_sim :  simF weak_sim s t }.
End WeakSim.

Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.


Check tau.
About tau.
(* Lemma silent_refl (M : Type) : forall m m', 
  clos_refl_trans_1n M (fun x y => R x None y) m m' -> *)


Lemma weak_sim_refl (M A : Type) (ltsM : LTS M A) (m : M)
  : weak_sim ltsM ltsM m m.
Proof.
  revert m.
  cofix CH.
  intros m.
  constructor 1.
  split.
  - intros m' a Hm.
    exists m'.
    split.
    + apply Hm.
    + apply CH. Guarded.
  - intros m' Hm. 
    exists m'.
    (* unfold silent1 in Hm. *)
    split. 
    + 
      Print clos_rt_t.
      Check clos_rt_t.
      About clos_rt_t.
      (* apply (@clos_rt_t M (silent ltsM) m m'). *)
      
      
      Check silent1.
      Check silent.
      About silent1.
      About silent.
      (* exact (tau ltsM m m'). *)
      (* constructor. *)
      unfold silent.
      Print clos_refl_trans_1n.

      (* unfold silent1 in Hm. *)
      (* apply clos_rt_t in Hm. *)
      (* induction . *)

      (* apply tau with (y:=m). *)
      (* unfold tau. *)
      constructor.

      (* Show Proof. *)
      (* match goal with 
      | [ x ] => constructor
      end. *)

      Check tau.
      (* replace y with m'. *)
      (* exists y. *)
      constructor.
      Check clos_refl_trans_1n.
      About clos_refl_trans_1n.
      constructor.
    apply Hm.   

  
  (* eapply simF. *)
  simpl.
  About simF.
  unfold simF.
  intros m' a tr.
  exists m'.
  split.
  - exact tr.
  - apply CH. Guarded.
Qed.

Module BisimTest1.
  Inductive action : Type := | TheAction1 | TheAction2.

  Inductive term : Type :=
    | trec : term
    | tend : term
    | tfix : term -> term
    | tact : action -> term -> term
    | tpar : action -> action -> term -> term
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
    | do_act1 : forall t, termLTS (tact TheAction1 t) 
                                  (Some TheAction1) 
                                  t
    
    | do_act2 : forall t, termLTS (tact TheAction2 t) 
                                  None 
                                  t

    | do_par1l : forall b t, termLTS (tpar TheAction1 b t) 
                                     (Some TheAction1) 
                                     (tact b t)

    | do_par1r : forall a t, termLTS (tpar a TheAction1 t) 
                                     (Some TheAction1) 
                                     (tact a t)

    | do_par2l : forall b t, termLTS (tpar TheAction2 b t) 
                                     None 
                                     (tact b t)

    | do_par2r : forall a t, termLTS (tpar a TheAction2 t) 
                                     None 
                                     (tact a t)

    | do_fix : forall a t t',
        termLTS (subst (tfix t) t) a t' ->
        termLTS (tfix t) a t'
    .


  Ltac example_tactic CH :=
   let r := fresh "r" in
   let a := fresh "a" in
   let tr := fresh "tr" in
   constructor; intros r a tr;

   (*  *)
   repeat
     match goal with
     | [ H : termLTS ?l ?a ?r |- _ ] =>
         inversion_clear H; simpl in *
     end;
   (*  *)

   eexists;
   split; [ repeat constructor | idtac ];
   try apply sim_refl; try apply CH.

  Ltac example_bisim :=
    let CH := fresh "CH" in
    cofix CH; repeat (example_tactic CH).

  About weak_tr.
  About Pack_weak.
  About weak.

  About simF.
  About Pack_sim.
  About weak_sim.

  Example test1_m := (tfix (tact TheAction1 trec)).
  Example test1_n := (tact TheAction1 (tfix (tact TheAction1 trec))).

  Goal weak_sim termLTS termLTS test1_m test1_n.    
  Proof. example_bisim. Qed.

  
  Goal weak_sim termLTS termLTS test1_m test1_n.    
  Proof. 
    unfold test1_m.
    unfold test1_n.
    cofix CH.
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
          (* eapply do_act1.  *)
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


