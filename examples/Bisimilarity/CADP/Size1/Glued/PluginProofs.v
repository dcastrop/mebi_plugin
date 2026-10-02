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
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.Bisimilarity.CADP.Size1.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 100.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.PluginProofs.bigstep".
Example wsim_bigstep : weak_sim bigstep lts c1 c1. 
Proof. MeBi Sim Begin bigstep c1 And lts c1 Using step.
  (* Iteration History: 267 <- 300 <- _ <- _ *) 
  MeBi Sim Solve 267. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.PluginProofs.lts".
Example wsim_lts : weak_sim lts bigstep c1 c1. 
Proof. MeBi Sim Begin lts c1 And bigstep c1 Using step. 
  (* Iteration History: 395 <- _ <- _ <- _  *) 
  MeBi Sim Solve 395. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.PluginProofs.ProofTest.bigstep.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_bigstep : mutual_sim bigstep lts c1 c1.
Proof. split; [exact wsim_bigstep | exact wsim_lts]. Qed.

(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]), one per pair -- stronger than the [mutual_sim]s above.
   These need the mutual cofix, which [Auto] chooses for every one of them;
   forced to the nested strategy, all but [Glued/MutualExclusion]'s are
   unfinished after 20000 steps, which is why they come last: a run with the
   strategy forced still reaches every [weak_sim] above. Bounds are the
   measured count minus one ([Solve N] permits N + 1 steps). *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.PluginProofs.ProofTest.bigstep.bisimilar".
Example wbis_bigstep : weak_bisimilar bigstep lts c1 c1.
Proof. MeBi Sim Begin bigstep c1 And lts c1 Using step.
  MeBi Sim Solve 2874. Qed.
