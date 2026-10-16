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

(* The Alternating Bit Protocol is weakly bisimilar to a one-place buffer
   (Milner 1989). [PluginProofs.v] proves [spec <= abp]; this file the other
   direction, [ABPBisimProofs.v] [weak_bisimilar]. Kept apart because they
   are slow: about 3.7 minutes each, and each peaks at about 4.2GB, so they
   are not built by default (see [_CoqProject]) and are one per file: both
   in one process exceed a 6GB cap, as the first proof term stays in memory.
   Run under a memory cap
   (systemd-run --user --scope -p MemoryMax=6G -p MemorySwapMax=0 ...).

   Until 2026-10-02 this did not terminate: the solver inverted, layer by
   layer through [var k] / [def k], transitions that do not exist (61% of
   all its inversions). It now refutes such a step outright (note 11,
   option D'; ASSISTED-CHANGES.md, 2026-10-02, third session).

   The bound is the least that closes the proof: [MeBi Sim Solve N] permits
   N + 1 steps, and the proof reports 6494. *)

MeBi Divider "Examples.Bisimilarity.CCS.ABPProofs".
MeBi Config Weak As Option act.
MeBi Config Bounds As Num States 1000.

Example wsim_abp_spec : weak_sim step step abp (var 20).
Proof. MeBi Sim Begin step abp And step (var 20) Using step. MeBi Sim Solve 6493. Qed.

MeBi Config Reset Bounds.
MeBi Config Reset Weak.
