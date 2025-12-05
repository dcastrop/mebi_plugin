Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Complex.

Require Import MEBI.Examples.bisimilarity.Proc.Test2.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs".
MeBi Reset All.
MeBi Set ShowAny      True.
MeBi Set ShowNotices  True.
MeBi Set ShowDebug    True.
MeBi Set ShowDetails  True.
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

(* MeBi Set ShowDebug False.  *)
(* MeBi Set ShowDetails False. *)


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. MeBiSim Begin termLTS p And termLTS q Using termLTS. 
  (* MeBiSim Solve 2000.
Qed. *)
  (* MeBiSim Solve 10. *)
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.

  (* MeBiSim Step. *)
  (*
  MeBiSim Step.
  MeBiSim Step.
  *)
Admitted.

(*

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. MeBiSim Begin termLTS q And termLTS p Using termLTS. 
  MeBiSim Solve 2000.
Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. MeBiSim Begin termLTS q And termLTS r Using termLTS. 
  MeBiSim Solve 2000.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. MeBiSim Begin termLTS r And termLTS q Using termLTS. 
  MeBiSim Solve 2000.
Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. MeBiSim Begin termLTS p And termLTS r Using termLTS. 
  MeBiSim Solve 2000.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. MeBiSim Begin termLTS r And termLTS p Using termLTS. 
  MeBiSim Solve 2000.
Qed.

*)