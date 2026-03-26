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


(* MeBi Config Output "Debug" True. *)
MeBi Config Output "Info" True.
MeBi Config Output "DecodeResults" True.
MeBi Config Output "DumpResults" True.

Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 2000.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.MutualExclusion.bigstep".
Example wsim_bigstep_spec_lts : weak_sim bigstep spec_lts c2 (make_spec 1). 
Proof. MeBi Sim Begin bigstep c2 And spec_lts (make_spec 1) Using lts step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.MutualExclusion.spec_lts".
Example wsim_spec_lts_bigstep : weak_sim spec_lts bigstep (make_spec 1) c2. 
Proof. MeBi Sim Begin spec_lts (make_spec 1) And bigstep c2 Using lts step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed.
