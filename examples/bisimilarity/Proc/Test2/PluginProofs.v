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
Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. 
  (* Iteration History: 446 <- 446 <- 465 <- _ *) 
  MeBi Sim Solve 446. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. MeBi Sim Begin termLTS q And termLTS p Using termLTS. 
  (* Iteration History: 278 <- 278 <- 297 <- _ *) 
  MeBi Sim Solve 278. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. MeBi Sim Begin termLTS q And termLTS r Using termLTS. 
  (* Iteration History: 299 <- 299 <- 322 <- _ *) 
  MeBi Sim Solve 299. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. MeBi Sim Begin termLTS r And termLTS q Using termLTS. 
  (* Iteration History: 194 <- 194 <- 195 <- _ *) 
  MeBi Sim Solve 194. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. MeBi Sim Begin termLTS p And termLTS r Using termLTS. 
  (* Iteration History: 446 <- 446 <- 497 <- _ *) 
  MeBi Sim Solve 446. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. MeBi Sim Begin termLTS r And termLTS p Using termLTS. 
  (* Iteration History: 182 <- 182 <- 191 <- _ *) 
  MeBi Sim Solve 182. Qed.
