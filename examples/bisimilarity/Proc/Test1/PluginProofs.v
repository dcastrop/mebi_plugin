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
MeBi Set ShowAny True. 
(* MeBi Set ShowAny False. *)
(* MeBi Set ShowNotices  True. *)
(* MeBi Set ShowDebug    True. *)
(* MeBi Set ShowDetails  True. *)
MeBi Set ShowDetails  False.
MeBi Set ShowResults  True.
(* MeBi Set ShowWarnings True. *)
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
(* MeBi LTS p Using termLTS. *)
(* MeBi LTS q Using termLTS. *)
(* MeBi Bisim p With termLTS And q With termLTS Using termLTS. *)

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs".
MeBi Reset All.
MeBi Set ShowAny      True.
MeBi Set ShowNotices  True.
(* MeBi Set ShowDebug    True. *)
(* MeBi Set ShowDetails  True. *)
MeBi Set ShowDetails  True.
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

MeBi Set ShowAny True.
MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof.
  MeBiSim Begin termLTS p And termLTS q Using termLTS. 
  MeBi Set ShowAny False.
  MeBiSim Solve 900.
Qed.
  (* 
  MeBiSim Step.
  *)
(* Admitted. *)

MeBi Set ShowAny True.


(* MeBi Set ShowDebug True. MeBi Set ShowDetails True. *)

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof.
  MeBiSim Begin termLTS q And termLTS p Using termLTS. 
  (* MeBi Set ShowAny False. *)
  (* MeBiSim Solve 900. *)
(* Qed. *)
Admitted.
(* TODO: looks like we try to apply [wk_none] for [Some action] *)
  (* MeBiSim Solve 30. *)
  (* MeBiSim Step.
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
  (* 
  MeBiSim Step.
  *)
Admitted. *)
