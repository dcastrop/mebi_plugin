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

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.BisimProofs".

MeBi Config Weak As Option label.

(* One [weak_bisimilar] proof per pair, where [PluginProofs.v] proves two
   separate [weak_sim]s (mutual similarity, a weaker statement). These need
   the mutual cofix, which [Auto] chooses for every one of them: forced to
   the nested strategy, all but [Glued/MutualExclusion] are unfinished after
   20000 steps. Bounds are the measured count minus one ([Solve N] permits
   N + 1 steps). See ASSISTED-CHANGES.md, 2026-10-02, option A part 2. *)
MeBi Config Bounds As Num States 100.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.BisimProofs.ProofTest.bigstep".
Example wbis_bigstep : weak_bisimilar bigstep lts c1 c1.
Proof. MeBi Sim Begin bigstep c1 And lts c1 Using step.
  MeBi Sim Solve 2874. Qed.


