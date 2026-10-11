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
Require Import MEBI.Examples.CADP_Norm.

Require Import MEBI.Examples.Bisimilarity.CADP.Size3.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.

MeBi Divider "Examples.Bisimilarity.CADP.Size3.Norm.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 1000.

Require Import Logic.

(* Three processes against [spec_lts], over [bigstep_norm] (644 states;
   [bigstep] has 128044), carried back to [bigstep].

   Only spec <= system is proved here. Mutual exclusion proper,
   [weak_sim bigstep_norm spec_lts c3 (make_spec 2)], was OOM-killed on
   2026-10-11: at 8GB after ~11 min under Auto (mutual cofix over 644
   pairs, 1932 moves), and at 9GB after ~13 min with [Solver Answers
   Minimal]. Nor is it decided: [Run Bisim] ([Norm/TermTests.v]) finds the
   two not bisimilar, as at [Size2], which says nothing about this one
   simulation, and no MeBi command decides a single simulation. It holds
   for one and two processes.

   Measured 2026-10-11 (Auto, mutual cofix): [wsim_norm_r] 326 steps.
   The bound is the measured count minus one. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size3.Norm.MutualExclusion.spec_lts".
Example wsim_norm_r : weak_sim spec_lts bigstep_norm (make_spec 2) c3.
Proof. MeBi Sim Begin spec_lts (make_spec 2) And bigstep_norm c3 Using bigstep.
  MeBi Sim Solve 325. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size3.Norm.MutualExclusion.transfer".
Example wsim_spec_lts : weak_sim spec_lts bigstep (make_spec 2) c3.
Proof.
  apply wsim_transfer_r; [apply composition_create_wf | exact wsim_norm_r].
Qed.
