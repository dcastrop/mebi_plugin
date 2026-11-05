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
Require Import MEBI.Examples.CCS.

(* [weak_bisimilar abp spec]: the Alternating Bit Protocol is weakly
   bisimilar to a one-place buffer (Milner 1989). See [ABPProofs.v] for why
   this is a file of its own and how to run it (about 3.8 minutes, peak
   about 4.2GB; under a memory cap).

   Until 2026-10-02 this did not terminate: the solver inverted, layer by
   layer through [var k] / [def k], transitions that do not exist (61% of
   all its inversions). It now refutes such a step outright (note 11,
   option D'; ASSISTED-CHANGES.md, 2026-10-02, third session).

   The bound is the least that closes the proof: [Set MeBi MaxSteps N] permits
   N + 1 steps, and the proof reports 9914. *)

MeBi Divider "Examples.Bisimilarity.CCS.ABPBisimProofs".
MeBi Config Weak As Option act.
Set MeBi MaxStates 1000.

Set MeBi MaxSteps 9913.
Example wbis_abp_spec : weak_bisimilar step step abp (var 20).
Proof. bisim. Qed.

MeBi Config Reset Bounds.
MeBi Config Reset Weak.
