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
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Glued.
Require Import MEBI.Examples.CADP.Norm.

Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecMutex.

(* [MutualExclusion.v] for two and three processes, glued only. Kept apart
   because these proofs are large: the direct two-process proof takes
   ~10 min and ~4.4GB, and under forced [MutualCofix False] the normalised
   one was OOM-killed at 6GB. Run it under a memory cap.

   With two or more processes the system and [spec_lts] are mutually
   similar but NOT weakly bisimilar ([MeBi Run Bisim c2 With bigstep And
   (make_spec 1) With spec_lts] fails with [Not_Bisimilar]). The MCS lock
   fixes the order of entry silently: once pid 1 has swapped itself into
   the lock's tail behind pid 0, only [ENTER 1] can follow pid 0's
   [LEAVE 0]. [spec_lts] has no such state: when it is [Free], either pid
   may enter next. So the system has states [spec_lts] cannot match, and
   bisimilarity fails.

   Each simulation still holds:
   - system <= spec is mutual exclusion proper: every [ENTER]/[LEAVE] trace
     of the system is allowed by the lock server;
   - spec <= system holds because the simulating side picks its own silent
     steps: the system can always answer along a path where the other
     process has not yet queued.
   With one process there is no order to fix, and the two are bisimilar.
   Stating no starvation needs a spec that sees the queue ([Fifo.v],
   [examples/README.md]).

   Bounds are the measured count minus one ([Solve N] permits N + 1
   steps). Until 2026-10-13 these were [Size2/Norm/MutualExclusion],
   [Size3/Norm/MutualExclusion] and [Size2/Glued/MutualExclusion]. *)

MeBi Divider "Examples.CADP.MutualExclusionLarge".

MeBi Config Weak As Option label.

Require Import Logic.


(**************************************************)
(* Two processes, normalised.

   Proved over the normalised semantics [bigstep_norm] ([Norm.v]) and
   carried back to [bigstep] by its transfer theorems. [bigstep_norm c2]
   has 48 states against [bigstep]'s 772: dead locals and the resource's
   unread [mem_next]/[mem_locked] are erased. The direct proof of
   [weak_sim bigstep spec_lts] (below) takes 11128 steps, ~10 min and
   ~4.4GB; this takes ~15s.

   Measured 2026-10-11 (Auto, which takes the mutual cofix for both):
   [wsim_norm_2] 1038 steps, [wsim_norm_r_2] 153; forced [MutualCofix
   True] gives the same. Forced [False] (nested), [wsim_norm_2] was
   OOM-killed at 6GB, as the direct proof over [bigstep] is. *)

MeBi Config Bounds As Num States 100.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size2.bigstep_norm".
Example wsim_norm_2 : weak_sim bigstep_norm spec_lts c2 (make_spec 1).
Proof. MeBi Sim Begin bigstep_norm c2 And spec_lts (make_spec 1) Using bigstep.
  MeBi Sim Solve 1037. Qed.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size2.bigstep_norm.spec_lts".
Example wsim_norm_r_2 : weak_sim spec_lts bigstep_norm (make_spec 1) c2.
Proof. MeBi Sim Begin spec_lts (make_spec 1) And bigstep_norm c2 Using bigstep.
  MeBi Sim Solve 152. Qed.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size2.transfer".
(* The same statements over [bigstep], by [Norm]'s transfer theorems:
   [c2] is well formed, as every [composition_create] is. *)
Example wsim_bigstep_2 : weak_sim bigstep spec_lts c2 (make_spec 1).
Proof. apply wsim_transfer; [apply composition_create_wf | exact wsim_norm_2]. Qed.

Example wsim_spec_lts_2 : weak_sim spec_lts bigstep (make_spec 1) c2.
Proof.
  apply wsim_transfer_r; [apply composition_create_wf | exact wsim_norm_r_2].
Qed.

Example msim_bigstep_2 : mutual_sim bigstep spec_lts c2 (make_spec 1).
Proof. split; [exact wsim_bigstep_2 | exact wsim_spec_lts_2]. Qed.


(**************************************************)
(* Three processes, normalised.

   Over [bigstep_norm] (644 states; [bigstep] has 128044), carried back to
   [bigstep]. Only spec <= system is proved. Mutual exclusion proper,
   [weak_sim bigstep_norm spec_lts c3 (make_spec 2)], was OOM-killed on
   2026-10-11: at 8GB after ~11 min under Auto (mutual cofix over 644
   pairs, 1932 moves), and at 9GB after ~13 min with [Solver Answers
   Minimal]. No MeBi command decides a single simulation, but it is
   decided indirectly: [Explore.v] finds the system bisimilar to the FIFO
   server [spec_fifo] (with the queue label visible), which only lets the
   head of its queue enter. Hiding that label to get back to [bigstep] is
   a theorem ([Hidden.v], [wbis_hidden]); what is missing at three
   processes is a proof of the FIFO bisimilarity itself, which is only
   decided. For one and two processes both are proved, and [Fifo.v]
   derives mutual exclusion that way.

   Measured 2026-10-11 (Auto, mutual cofix): [wsim_norm_r_3] 326 steps. *)

MeBi Config Bounds As Num States 1000.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size3.spec_lts".
Example wsim_norm_r_3 : weak_sim spec_lts bigstep_norm (make_spec 2) c3.
Proof. MeBi Sim Begin spec_lts (make_spec 2) And bigstep_norm c3 Using bigstep.
  MeBi Sim Solve 325. Qed.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size3.transfer".
Example wsim_spec_lts_3 : weak_sim spec_lts bigstep (make_spec 2) c3.
Proof.
  apply wsim_transfer_r; [apply composition_create_wf | exact wsim_norm_r_3].
Qed.


(**************************************************)
(* Two processes, glued, proved directly. [bigstep c2] has 772 states.

   Measured 2026-10-11 (Auto): [wsim_bigstep_direct_2] 11128 steps, ~9 min
   solving + ~47s Qed, ~4.4GB peak; [wsim_spec_lts_direct_2] 405 steps,
   ~4s. [Auto] takes the mutual cofix for both (forced [MutualCofix True]
   gives the same counts). Forced [False] (nested),
   [wsim_spec_lts_direct_2] takes 1141 steps, and [wsim_bigstep_direct_2]
   was OOM-killed at 6GB after ~13 min, so this file does not compile with
   the nested strategy forced. *)

MeBi Config Bounds As Num States 2000.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size2.bigstep".
Example wsim_bigstep_direct_2 : weak_sim bigstep spec_lts c2 (make_spec 1).
Proof. MeBi Sim Begin bigstep c2 And spec_lts (make_spec 1) Using lts step.
  MeBi Sim Solve 11127. Qed.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size2.bigstep.spec_lts".
Example wsim_spec_lts_direct_2 : weak_sim spec_lts bigstep (make_spec 1) c2.
Proof. MeBi Sim Begin spec_lts (make_spec 1) And bigstep c2 Using lts step.
  MeBi Sim Solve 404. Qed.

MeBi Divider "Examples.CADP.MutualExclusionLarge.Size2.bigstep.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_bigstep_direct_2 : mutual_sim bigstep spec_lts c2 (make_spec 1).
Proof. split; [exact wsim_bigstep_direct_2 | exact wsim_spec_lts_direct_2]. Qed.
