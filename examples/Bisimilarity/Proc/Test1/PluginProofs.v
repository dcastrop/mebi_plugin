Require Import MEBI.loader.

MeBi Config Output "Debug" False.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" False.
MeBi Config Output "Result" False.
MeBi Config Output "Show" False.
MeBi Config Output "DecodeResults" False.
MeBi Config Output "DumpResults" False.

Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Simple.

Require Import MEBI.Examples.Bisimilarity.Proc.Test1.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs".
  
MeBi Config Weak As Option label.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. 
  (* Iteration History: 114 <- 114 <- 165 <- 116 *) 
  MeBi Sim Solve 114. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. MeBi Sim Begin termLTS q And termLTS p Using termLTS. 
  (* Iteration History: 105 <- 105 <- 157 <- 108 *)
  MeBi Sim Solve 105. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. MeBi Sim Begin termLTS q And termLTS r Using termLTS. 
  (* Iteration History: 106 <- 106 <- 188 <- 109 *) 
  MeBi Sim Solve 106. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. MeBi Sim Begin termLTS r And termLTS q Using termLTS. 
  (* Iteration History: 109 <- 109 <- 161 <- 112 *)
  MeBi Sim Solve 109. Qed. 


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. MeBi Sim Begin termLTS p And termLTS r Using termLTS. 
  (* Iteration History: 22 <- 69 <- 196 <- 113 *)
  MeBi Sim Solve 22. Qed. 

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. MeBi Sim Begin termLTS r And termLTS p Using termLTS. 
  (* Iteration History: 21 <- 63 <- 165 <- 108 *) 
  MeBi Sim Solve 21. Qed. 
