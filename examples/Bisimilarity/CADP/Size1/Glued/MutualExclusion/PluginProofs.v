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
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 2000.

Require Import Logic.

(* MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion". *)
(* MeBi Run FSM (make_spec 1) Using spec_lts. *)
(* MeBi Run FSM (compose (create 1 Protocol.P)) Using lts step. *)


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.bigstep".
Example wsim_bigstep : weak_sim bigstep spec_lts (compose (create 1 Protocol.P)) (make_spec 1). 
Proof. MeBi Sim Begin bigstep (compose (create 1 Protocol.P)) And spec_lts (make_spec 1) Using lts step.
  (* Iteration History: 82 <- _ <- _ <- _ *) 
  MeBi Sim Solve 82. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.spec_lts".
Example wsim_spec_lts : weak_sim spec_lts bigstep (make_spec 1) (compose (create 1 Protocol.P)). 
Proof. MeBi Sim Begin spec_lts (make_spec 1) And bigstep (compose (create 1 Protocol.P)) Using lts step.
  (* Iteration History: 64 <- _ <- _ <- _ *) 
  MeBi Sim Solve 64. Qed.
