(* Three processes, mutual exclusion proper: [weak_sim bigstep_norm spec_lts
   c3 (make_spec 2)], system <= spec, over the normalised semantics (644
   states). PROVED on a machine with 31GB, not on one with 14GB: this is
   the proof that ran out of memory there.

   Measured 2026-10-11 on a 14GB machine, before [Answers Auto] was the
   default: OOM-killed at 8GB after ~11 min (Auto, mutual cofix over 644
   pairs, 1932 moves), and at 9GB after ~13 min with [Solver Answers
   Minimal].

   Measured 2026-10-15 on a 31GB machine (24GiB cap): [Solve] 20,432
   steps, 1,205s, then [Qed] 157s; 22.8 min, 13.4GiB peak. The same under
   [Answers Default] (20,432 steps, 23.7 min, 13.4GiB): the policy makes no
   difference here, the machine does. See
   doc/measurements/cadp-large.md.

   One proof, so that it is its own process: run it under a memory cap, as
   [bench/proofs.sh -s cadp-huge -f ThreeProcessesMutex.v -M <cap>]. The
   bound is large on purpose: it finishes or runs out of memory. See
   [MutualExclusionLarge.v] for what is proved at three processes, and
   [doc/future-work.md], "Re-running the large CADP proofs". *)

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

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Glued.
Require Import MEBI.Examples.CADP.Norm.
Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecMutex.

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 1000.

MeBi Divider "Examples.CADP.ThreeProcessesMutex".
Example wsim_norm_3 : weak_sim bigstep_norm spec_lts c3 (make_spec 2).
Proof. MeBi Sim Begin bigstep_norm c3 And spec_lts (make_spec 2) Using bigstep.
  MeBi Sim Solve 1000000. Qed.
