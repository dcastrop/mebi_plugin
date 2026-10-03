(* Test4's weak bisimilarity, proved under its own semantics [compLTS]:
   the plugin proves [weak_bisimilar] over the normalised semantics [nLTS],
   and [ProcCongruence.Normalised.wbis_transfer] carries it to [compLTS].
   Its own file, as heavy proofs add up in one process (see NormProofs.v).

   With the default answers the proof would visit 6592 pairs (every state
   is bisimilar to every other, so answers scatter over nearly all 82 x 82
   pairs) and 52,088 moves. [Answers Minimal] plans within a minimal
   relation instead: 245 pairs, 1912 moves. Its planning took 19.5 minutes
   until 2026-10-03 ([Product.Policy.minimal_relation]), now 5.6s.

   Not built by default: ~4 minutes and ~2.8GB peak; run under a memory cap
   (systemd-run --user --scope -p MemoryMax=6G -p MemorySwapMax=0 ...). The
   bound is the least that closes the proof: [MeBi Sim Solve N] permits
   N + 1 steps, and it reports 61161 (the plan predicted ~32k). *)

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
Require Import MEBI.Examples.Proc.
Import Layered.
Require Import MEBI.Examples.ProcCongruence.
Import Normalised.
Require Import MEBI.Examples.Bisimilarity.Proc.Test4.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.NormBisimProofs".
MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 200.
MeBi Config Solver Answers Minimal.

Example wbis_pq_norm : weak_bisimilar nLTS nLTS p q.
Proof. MeBi Sim Begin nLTS p And nLTS q Using nLTS core termLTS. MeBi Sim Solve 61160. Qed.

(* Test4's bisimilarity, in the semantics it was written in *)
Example wbis_pq : weak_bisimilar compLTS compLTS p q.
Proof. exact (wbis_transfer p q wbis_pq_norm). Qed.

MeBi Config Solver Answers Default.
MeBi Config Reset Bounds.
MeBi Config Reset Weak.
