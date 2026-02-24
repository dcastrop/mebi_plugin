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

MeBi Config Weak As Option label.

Require Import Logic.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. MeBiSim Begin termLTS p And termLTS q Using termLTS. 
  (* Iteration History: 446 <- 465 *) 
  MeBiSim Solve 446. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. MeBiSim Begin termLTS q And termLTS p Using termLTS. 
  (* Iteration History: 278 <- 297 *) 
  MeBiSim Solve 278. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. MeBiSim Begin termLTS q And termLTS r Using termLTS. 
  (* Iteration History: 299 <- 322 *) 
  MeBiSim Solve 299. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. MeBiSim Begin termLTS r And termLTS q Using termLTS. 
  (* Iteration History: 194 <- 195 *) 
  MeBiSim Solve 194. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. MeBiSim Begin termLTS p And termLTS r Using termLTS. 
  (* Iteration History: 446 <- 497 *) 
  MeBiSim Solve 446. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. MeBiSim Begin termLTS r And termLTS p Using termLTS. 
  (* Iteration History: 182 <- 191 *) 
  MeBiSim Solve 182. Qed.
