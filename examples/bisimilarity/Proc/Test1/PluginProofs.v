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

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs".
  
MeBi Config Weak As Option label.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. MeBiSim Begin termLTS p And termLTS q Using termLTS. 
  (* Iteration History: 114 <- 165 <- 116 *) 
  MeBiSim Solve 114. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. MeBiSim Begin termLTS q And termLTS p Using termLTS. 
  (* Iteration History: 105 <- 157 <- 108 *)
  MeBiSim Solve 105. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. MeBiSim Begin termLTS q And termLTS r Using termLTS. 
  (* Iteration History: 106 <- 188 <- 109 *) 
  MeBiSim Solve 106. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. MeBiSim Begin termLTS r And termLTS q Using termLTS. 
  (* Iteration History: 109 <- 161 <- 112 *)
  MeBiSim Solve 109. Qed. 


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. MeBiSim Begin termLTS p And termLTS r Using termLTS. 
  (* Iteration History: 69 <- 196 <- 113 *)
  MeBiSim Solve 69. Qed. 

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. MeBiSim Begin termLTS r And termLTS p Using termLTS. 
  (* Iteration History: 63 <- 165 <- 108 *) 
  MeBiSim Solve 63. Qed. 
