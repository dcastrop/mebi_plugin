Require Import MEBI.loader.

MeBi Config Output "Debug" False.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" False.
MeBi Config Output "Result" True.
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

Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.PluginProofs".

MeBi Config Weak As Option label.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.PluginProofs.bigstep".
Example wsim_bigstep_lts : weak_sim bigstep lts c2 c2. 
Proof. MeBi Sim Begin bigstep c2 And lts c2 Using step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.PluginProofs.lts".
Example wsim_lts_bigstep : weak_sim lts bigstep c2 c2. 
Proof. MeBi Sim Begin lts c2 And bigstep c2 Using step. 
  (* Iteration History: _ <- _ <- _ <- _  *) 
  MeBi Sim Solve 1000. Qed.
