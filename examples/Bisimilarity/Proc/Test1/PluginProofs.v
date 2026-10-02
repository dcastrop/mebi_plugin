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

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pq.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_pq : mutual_sim termLTS termLTS p q.
Proof. split; [exact wsim_pq | exact wsim_qp]. Qed.


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

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qr.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_qr : mutual_sim termLTS termLTS q r.
Proof. split; [exact wsim_qr | exact wsim_rq]. Qed.


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

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pr.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_pr : mutual_sim termLTS termLTS p r.
Proof. split; [exact wsim_pr | exact wsim_rp]. Qed.

(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]), one per pair -- stronger than the [mutual_sim]s above.
   These need the mutual cofix, which [Auto] chooses for every one of them;
   forced to the nested strategy, all but [Glued/MutualExclusion]'s are
   unfinished after 20000 steps, which is why they come last: a run with the
   strategy forced still reaches every [weak_sim] above. Bounds are the
   measured count minus one ([Solve N] permits N + 1 steps). *)

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pq.bisimilar".
Example wbis_pq : weak_bisimilar termLTS termLTS p q.
Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS.
  MeBi Sim Solve 708. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qr.bisimilar".
Example wbis_qr : weak_bisimilar termLTS termLTS q r.
Proof. MeBi Sim Begin termLTS q And termLTS r Using termLTS.
  MeBi Sim Solve 708. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pr.bisimilar".
Example wbis_pr : weak_bisimilar termLTS termLTS p r.
Proof. MeBi Sim Begin termLTS p And termLTS r Using termLTS.
  MeBi Sim Solve 50. Qed.
