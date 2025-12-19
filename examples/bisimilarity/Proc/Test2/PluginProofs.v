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
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
(* MeBi Config Output Debug Enable. *)
(* MeBi Config Output Info Enable. *)
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
(* MeBi Config Bound 100. *)
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)
MeBi Config Output Disable.

Require Import Logic.



MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. MeBiSim Begin termLTS p And termLTS q Using termLTS. 
  MeBiSim Solve 470.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. MeBiSim Begin termLTS q And termLTS p Using termLTS. 
  MeBiSim Solve 300.
Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. MeBiSim Begin termLTS q And termLTS r Using termLTS. 
  MeBiSim Solve 330.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. MeBiSim Begin termLTS r And termLTS q Using termLTS. 
  MeBiSim Solve 200.
Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. MeBiSim Begin termLTS p And termLTS r Using termLTS. 
  MeBiSim Solve 500.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. MeBiSim Begin termLTS r And termLTS p Using termLTS. 
  MeBiSim Solve 200.
Qed.
