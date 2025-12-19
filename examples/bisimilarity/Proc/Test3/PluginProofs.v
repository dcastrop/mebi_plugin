Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Import Layered.

Require Import MEBI.Examples.bisimilarity.Proc.Test3.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs".
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Debug Enable.
MeBi Config Output Notice Enable.
MeBi Config Output Info Enable.
MeBi Config Output Warning Enable.
MeBi Config Output Trace Enable.
MeBi Config Output Results Enable.
(* MeBi Config Bound 100. *)
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)
(* MeBi Config Output Disable. *)

Require Import Logic.



MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim compLTS compLTS p q. 
Proof. MeBiSim Begin compLTS p And compLTS q Using compLTS termLTS. 
  (* MeBiSim Solve 470. *)
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  (* unfold s1 in *. *)
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim compLTS compLTS q p. 
Proof. MeBiSim Begin compLTS q And compLTS p Using compLTS termLTS. 
  MeBiSim Solve 300.
Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim compLTS compLTS q r. 
Proof. MeBiSim Begin compLTS q And compLTS r Using compLTS termLTS. 
  MeBiSim Solve 330.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim compLTS compLTS r q. 
Proof. MeBiSim Begin compLTS r And compLTS q Using compLTS termLTS. 
  MeBiSim Solve 200.
Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim compLTS compLTS p r. 
Proof. MeBiSim Begin compLTS p And compLTS r Using compLTS termLTS. 
  MeBiSim Solve 500.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim compLTS compLTS r p. 
Proof. MeBiSim Begin compLTS r And compLTS p Using compLTS termLTS. 
  MeBiSim Solve 200.
Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rs".
Example wsim_rp : weak_sim compLTS compLTS r s. 
Proof. MeBiSim Begin compLTS r And compLTS s Using compLTS termLTS. 
  MeBiSim Solve 200.
Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.sr".
Example wsim_pr : weak_sim compLTS compLTS s r. 
Proof. MeBiSim Begin compLTS s And compLTS r Using compLTS termLTS. 
  MeBiSim Solve 500.
Qed.
