(* Test4 proved, under its own semantics [compLTS]: the plugin proves
   [weak_sim] over the normalised semantics [nLTS] (82 states), and
   [ProcCongruence.Normalised.wsim_transfer] -- one hand proof, for all
   terms -- carries it to [compLTS], whose 9720-state LTS no proof could
   cover. Not built by default: ~7 minutes and ~3.8GB peak, so run it under
   a memory cap (systemd-run --user --scope -p MemoryMax=6G
   -p MemorySwapMax=0 ...). The bound is the least that closes the proof:
   [MeBi Sim Solve N] permits N + 1 steps, and it reports 48821.

   [weak_bisimilar nLTS nLTS p q] is not attempted: Auto predicts 6592 pairs
   and 52,088 moves, and it did not finish within 25 minutes. *)

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

MeBi Divider "Examples.Bisimilarity.Proc.Test4.NormProofs".
MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 200.

Example wsim_pq_norm : weak_sim nLTS nLTS p q.
Proof. MeBi Sim Begin nLTS p And nLTS q Using nLTS core termLTS. MeBi Sim Solve 48820. Qed.

(* Test4's simulation, in the semantics it was written in *)
Example wsim_pq : weak_sim compLTS compLTS p q.
Proof. exact (wsim_transfer p q wsim_pq_norm). Qed.

MeBi Config Reset Bounds.
MeBi Config Reset Weak.
