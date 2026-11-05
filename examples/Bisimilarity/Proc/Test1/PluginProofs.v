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
Set MeBi MaxSteps 87.
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. sim.
  (* Iteration History: 114 <- 114 <- 165 <- 116 *)
   Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qp".
Set MeBi MaxSteps 81.
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. sim.
  (* Iteration History: 105 <- 105 <- 157 <- 108 *)
   Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pq.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_pq : mutual_sim termLTS termLTS p q.
Proof. split; [exact wsim_pq | exact wsim_qp]. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qr".
Set MeBi MaxSteps 87.
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. sim.
  (* Iteration History: 106 <- 106 <- 188 <- 109 *)
   Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.rq".
Set MeBi MaxSteps 87.
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. sim.
  (* Iteration History: 109 <- 109 <- 161 <- 112 *)
   Qed. 

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qr.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_qr : mutual_sim termLTS termLTS q r.
Proof. split; [exact wsim_qr | exact wsim_rq]. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pr".
Set MeBi MaxSteps 22.
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. sim.
  (* Iteration History: 22 <- 69 <- 196 <- 113 *)
   Qed. 

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.rp".
Set MeBi MaxSteps 9.
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. sim.
  (* Iteration History: 21 <- 63 <- 165 <- 108 *)
   Qed. 

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
Set MeBi MaxSteps 304.
Example wbis_pq : weak_bisimilar termLTS termLTS p q.
Proof. bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.qr.bisimilar".
Set MeBi MaxSteps 317.
Example wbis_qr : weak_bisimilar termLTS termLTS q r.
Proof. bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.PluginProofs.ProofTest.pr.bisimilar".
Set MeBi MaxSteps 50.
Example wbis_pr : weak_bisimilar termLTS termLTS p r.
Proof. bisim. Qed.
