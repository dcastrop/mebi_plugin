(* Three processes, FIFO order: [weak_bisimilar bigstep_obs_norm spec_fifo
   c3 (make_fifo 2)], over the normalised semantics with joining the queue
   visible (644 states). PROVED on a machine with 31GB, not on one with
   14GB: this is the proof that ran out of memory there. One and two
   processes are proved in [Fifo.v] (119 and 2228 steps); three is decided
   ([Explore.v], ~4s). This file proves it over the normalised semantics;
   it does not apply [wbis_transfer_obs] to carry it to [bigstep_obs], as
   [Fifo.v] does for one and two.

   Measured 2026-10-11 on a 14GB machine, before [Answers Auto] was the
   default: OOM-killed at 9GB after ~15 min.

   Measured 2026-10-15 on a 31GB machine (24GiB cap): [Solve] 45,773
   steps, 1,906s, then [Qed] 346s; 37.6 min, 17.8GiB peak. The same under
   [Answers Default] (45,773 steps, 36.3 min, 17.9GiB): the policy makes no
   difference here, the machine does. See
   doc/measurements/cadp-large.md.

   One proof, so that it is its own process: run it under a memory cap, as
   [bench/proofs.sh -s cadp-huge -f ThreeProcessesFifo.v -M <cap>]. The
   bound is large on purpose: it finishes or runs out of memory. See
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
Require Import MEBI.Examples.CADP.Obs.
Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecFifo.

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 1000.

MeBi Divider "Examples.CADP.ThreeProcessesFifo".
Example wbis_norm_3 : weak_bisimilar bigstep_obs_norm spec_fifo c3 (make_fifo 2).
Proof. MeBi Sim Begin bigstep_obs_norm c3 And spec_fifo (make_fifo 2) Using bigstep_obs.
  MeBi Sim Solve 1000000. Qed.
