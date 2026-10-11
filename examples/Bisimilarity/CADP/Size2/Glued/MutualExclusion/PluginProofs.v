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

Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 2000.

Require Import Logic.

(* Two processes ([c2] is [composition_create 1]: pids 1 and 0) against the
   lock server [spec_lts] for the same pids. [bigstep c2] has 772 states.

   The two are mutually similar but NOT weakly bisimilar ([MeBi Run Bisim c2
   With bigstep And (make_spec 1) With spec_lts] fails with
   [Not_Bisimilar]). The MCS lock fixes the order of entry silently: once
   pid 1 has swapped itself into the lock's tail behind pid 0, only
   [ENTER 1] can follow pid 0's [LEAVE 0]. [spec_lts] has no such state:
   when it is [Free], either pid may enter next. So the system has states
   [spec_lts] cannot match, and bisimilarity fails.

   Each simulation still holds:
   - [wsim_bigstep] (system <= spec) is mutual exclusion proper: every
     [ENTER]/[LEAVE] trace of the system is allowed by the lock server;
   - [wsim_spec_lts] (spec <= system) holds because the simulating side
     picks its own silent steps: the system can always answer along a path
     where the other process has not yet queued.
   With one process ([Size1]) there is no order to fix, and the two are
   bisimilar. Stating no starvation needs a spec that sees the queue (see
   [examples/README.md]).

   Measured 2026-10-11 (Auto): [wsim_bigstep] 11128 steps, ~9 min solving
   + ~47s Qed, ~4.4GB peak, so run it under a memory cap;
   [wsim_spec_lts] 405 steps, ~4s. Bounds are the measured count minus one
   ([Solve N] permits N + 1 steps). [Auto] takes the mutual cofix for both
   (forced [MutualCofix True] gives the same counts). Forced [False]
   (nested), [wsim_spec_lts] takes 1141 steps, and [wsim_bigstep] was
   OOM-killed at 6GB after ~13 min, so this file does not compile with the
   nested strategy forced. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.MutualExclusion.bigstep".
Example wsim_bigstep : weak_sim bigstep spec_lts c2 (make_spec 1).
Proof. MeBi Sim Begin bigstep c2 And spec_lts (make_spec 1) Using lts step.
  MeBi Sim Solve 11127. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.MutualExclusion.spec_lts".
Example wsim_spec_lts : weak_sim spec_lts bigstep (make_spec 1) c2.
Proof. MeBi Sim Begin spec_lts (make_spec 1) And bigstep c2 Using lts step.
  MeBi Sim Solve 404. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.MutualExclusion.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_bigstep : mutual_sim bigstep spec_lts c2 (make_spec 1).
Proof. split; [exact wsim_bigstep | exact wsim_spec_lts]. Qed.
