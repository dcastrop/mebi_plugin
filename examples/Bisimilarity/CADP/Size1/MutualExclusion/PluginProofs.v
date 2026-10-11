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

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 100.

Require Import Logic.

(* Mutual exclusion for the layered semantics ([lts] over [step]), against
   the lock server [spec_lts], for one process. [Glued/MutualExclusion]
   proves the same for the glued semantics. With one process the two are
   weakly bisimilar; with two they are only mutually similar (see
   [Size2/Glued/MutualExclusion] and [examples/README.md]).

   This file held a copy of [Glued/PluginProofs.v] (glued against layered)
   from 2026-03-16 to 2026-10-11; these are the goals its name and its
   original version ([3cc6f10]) stated. Bounds are the measured count minus
   one ([Solve N] permits N + 1 steps). *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.lts".
Example wsim_lts : weak_sim lts spec_lts c1 (make_spec 0).
Proof. MeBi Sim Begin lts c1 And spec_lts (make_spec 0) Using step.
  MeBi Sim Solve 258. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.spec_lts".
Example wsim_spec_lts : weak_sim spec_lts lts (make_spec 0) c1.
Proof. MeBi Sim Begin spec_lts (make_spec 0) And lts c1 Using step.
  MeBi Sim Solve 211. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.PluginProofs.ProofTest.lts.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_lts : mutual_sim lts spec_lts c1 (make_spec 0).
Proof. split; [exact wsim_lts | exact wsim_spec_lts]. Qed.

(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]) -- stronger than the [mutual_sim] above. It needs the
   mutual cofix, which [Auto] chooses for it; forced to the nested strategy
   it is unfinished after 50000 steps, which is why it comes last. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.PluginProofs.ProofTest.lts.bisimilar".
Example wbis_lts : weak_bisimilar lts spec_lts c1 (make_spec 0).
Proof. MeBi Sim Begin lts c1 And spec_lts (make_spec 0) Using step.
  MeBi Sim Solve 1441. Qed.
