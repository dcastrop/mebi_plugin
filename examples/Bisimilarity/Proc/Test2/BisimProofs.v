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
Import Flat.Complex.

Require Import MEBI.Examples.Bisimilarity.Proc.Test2.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.BisimProofs".

MeBi Config Weak As Option label.

(* One [weak_bisimilar] proof per pair, where [PluginProofs.v] proves two
   separate [weak_sim]s (mutual similarity, a weaker statement). These need
   the mutual cofix, which [Auto] chooses for every one of them: forced to
   the nested strategy, all but [Glued/MutualExclusion] are unfinished after
   20000 steps. Bounds are the measured count minus one ([Solve N] permits
   N + 1 steps). See ASSISTED-CHANGES.md, 2026-10-02, option A part 2. *)

Require Import Logic.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.BisimProofs.ProofTest.pq".
Example wbis_pq : weak_bisimilar termLTS termLTS p q.
Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS.
  MeBi Sim Solve 701. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.BisimProofs.ProofTest.qr".
Example wbis_qr : weak_bisimilar termLTS termLTS q r.
Proof. MeBi Sim Begin termLTS q And termLTS r Using termLTS.
  MeBi Sim Solve 851. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.BisimProofs.ProofTest.pr".
Example wbis_pr : weak_bisimilar termLTS termLTS p r.
Proof. MeBi Sim Begin termLTS p And termLTS r Using termLTS.
  MeBi Sim Solve 851. Qed.

