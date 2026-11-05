(* Evaluation series: nesting depth. The calculus of [Test.v]'s
   [Collapsing] (backlog B1), with labels as [option action] so proofs can
   be stated, and [Collapse] as the silent [None]. [do_fix] wraps its
   target in [tfix] again, so each pass through [trec] adds a level;
   [Guarded] allows it only while the body is shallower than [K], so
   [tfix X] has [2K + 1] states and its derivations get deeper with [K].
   The proof is [weak_bisimilar] between [tfix X] and [tfix (tfix X)],
   which differ by one silent collapse. Bounds as in [Width.v]. *)
Require Import MEBI.loader.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Result" False.
Require Import MEBI.Bisimilarity.

MeBi Divider "Examples.Evaluation.Depth".

Inductive action : Set := | TheAction1 | TheAction2.
Inductive term : Set :=
| trec : term
| tend : term
| tfix : term -> term
| tact : action -> term -> term
| tpar : action -> action -> term -> term.

Fixpoint subst (t1 : term) (t2 : term) :=
  match t2 with
  | trec => t1
  | tend => tend
  | tfix t => tfix t
  | tact a t => tact a (subst t1 t)
  | tpar a b t => tpar a b (subst t1 t)
  end.

Fixpoint fix_depth (t : term) : nat :=
  match t with tfix u => S (fix_depth u) | _ => 0 end.

Definition X := tact TheAction1 (tact TheAction2 trec).

Module Type BOUND. Parameter K : nat. End BOUND.

Module Guarded (B : BOUND).
  Inductive termLTS : term -> option action -> term -> Prop :=
  | do_act : forall a t, termLTS (tact a t) (Some a) t
  | do_par1 : forall a b t, termLTS (tpar a b t) (Some a) (tact b t)
  | do_par2 : forall a b t, termLTS (tpar a b t) (Some b) (tact a t)
  | do_fix : forall a t t',
      fix_depth t < B.K ->
      termLTS (subst (tfix t) t) a t' ->
      termLTS (tfix t) a (tfix t')
  | do_collapse : forall t, termLTS (tfix (tfix t)) None (tfix t).
End Guarded.

MeBi Config Weak As Option action.

Module K1 <: BOUND. Definition K := 1. End K1.
Module G1 := Guarded K1.
Set MeBi MaxStates 3.
MeBi Run LTS (tfix X) Using G1.termLTS.
Set MeBi MaxStates 2.
Fail MeBi Run LTS (tfix X) Using G1.termLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 20.
Example wbis_k1 : weak_bisimilar G1.termLTS G1.termLTS (tfix X) (tfix (tfix X)).
Proof. bisim. Qed.

Module K2 <: BOUND. Definition K := 2. End K2.
Module G2 := Guarded K2.
Set MeBi MaxStates 5.
MeBi Run LTS (tfix X) Using G2.termLTS.
Set MeBi MaxStates 4.
Fail MeBi Run LTS (tfix X) Using G2.termLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 92.
Example wbis_k2 : weak_bisimilar G2.termLTS G2.termLTS (tfix X) (tfix (tfix X)).
Proof. bisim. Qed.

Module K4 <: BOUND. Definition K := 4. End K4.
Module G4 := Guarded K4.
Set MeBi MaxStates 9.
MeBi Run LTS (tfix X) Using G4.termLTS.
Set MeBi MaxStates 8.
Fail MeBi Run LTS (tfix X) Using G4.termLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 406.
Example wbis_k4 : weak_bisimilar G4.termLTS G4.termLTS (tfix X) (tfix (tfix X)).
Proof. bisim. Qed.

Module K8 <: BOUND. Definition K := 8. End K8.
Module G8 := Guarded K8.
Set MeBi MaxStates 17.
MeBi Run LTS (tfix X) Using G8.termLTS.
Set MeBi MaxStates 16.
Fail MeBi Run LTS (tfix X) Using G8.termLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 2170.
Example wbis_k8 : weak_bisimilar G8.termLTS G8.termLTS (tfix X) (tfix (tfix X)).
Proof. bisim. Qed.

MeBi Config Reset Weak.
