Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Simple.

Require Import MEBI.Examples.bisimilarity.Proc.Test1.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.Bisim".
(* MeBi Set ShowAny True.  *)
MeBi Set ShowAny False.
(* MeBi Set ShowNotices  True. *)
(* MeBi Set ShowDebug    True. *)
(* MeBi Set ShowDetails  True. *)
MeBi Set ShowDetails  False.
(* MeBi Set ShowResults  True. *)
(* MeBi Set ShowWarnings True. *)
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
MeBi Bisim p With termLTS And q With termLTS Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs".
MeBi Reset All.
MeBi Set ShowAny      True.
MeBi Set ShowNotices  True.
MeBi Set ShowDebug    True.
MeBi Set ShowDetails  True.
MeBi Set ShowDetails  False.
MeBi Set ShowResults  True.
MeBi Set ShowWarnings True.
(* MeBi Set Bound 100. *)
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
MeBi Set FailIfIncomplete True.
MeBi Set FailIfNotBisim True.
(* MeBi See All.  *)
(* MeBi Set ShowAny False. *)

Require Import Logic.


MeBi Divider "Testing ctor trees".

MeBi FSM p Using termLTS. 

(* Example e1 : term := (tseq (tseq (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend) tend) tend). *)
Example e1 : term := (tseq (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend) tend).
(* Example e1 : term := (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend). *)

(* MeBi FSM e1 Using termLTS.  *)
(* MeBi Saturate e1 Using termLTS. *)


(* MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof.
  MeBiSim Begin termLTS p And termLTS q Using termLTS. (* unfold p, q. *)

  MeBiSim Step. (* cofix Cofix0; apply In_sim, Pack_sim; intros. *) 
  MeBiSim Step. (* inversion H; simpl in *. *)
  MeBiSim Step. (* exists n2 *)
  MeBiSim Step. (* apply wk_none. apply rt1n_refl. *)
  MeBiSim Step. (* clear H; cofix Cofix0; apply In_sim, Pack_sim; intros. *) 
  MeBiSim Step. (* inversion H; simpl in *. *)
  MeBiSim Step. (* inversion H0; simpl in *. *)
  MeBiSim Step. (* exists n2 *)
  MeBiSim Step. (* eapply wk_some; unfold silent. *)
  MeBiSim Step. (* eapply rt1n_trans. *)
  MeBiSim Step. (* constructor 4. simpl in *. *)
  MeBiSim Step. (* eapply rt1n_trans. *)
  MeBiSim Step. (* constructor 2. *)
  MeBiSim Step. (* constructor 5. *)
  MeBiSim Step. (* apply rt1n_refl. *)
  MeBiSim Step. (* constructor 2. *)
  MeBiSim Step. (* constructor 1. *)
  MeBiSim Step. (* apply rt1n_refl. *)
  MeBiSim Step. (* constructor 2. *)
  MeBiSim Step. (* constructor 5. *)
  MeBiSim Step. (* apply rt1n_refl. *)
  MeBiSim Step. (* clear H; cofix Cofix0; apply In_sim, Pack_sim; intros. *) 
  MeBiSim Step. (* inversion H; simpl in *. *)
  MeBiSim Step. (* inversion H4; simpl in *. *)
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 
  MeBiSim Step. 

  (* (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) (tfix (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) trec))) *)
  (* (tseq (tpar tend tend) (tfix (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) trec))) *)
  (* (tseq (tpar (tact (recv A) tend) (tact (send A) tend)) (tfix (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) trec))) *)
  (* MeBiSim Step.  *)
  (* TODO: investigate sandboxed_unify *)



Admitted. *)