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
Require Import MEBI.Examples.CADP_Norm.
Require Import MEBI.Examples.CADP_Obs.

Require Import MEBI.Examples.Bisimilarity.CADP.Size1.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.Fifo.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.Hidden.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Norm.Fifo.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 1000.

Require Import Logic.

(* The MCS lock is weakly bisimilar to the FIFO lock server [spec_fifo]
   ([Properties/Fifo.v]), with joining the queue visible ([bigstep_obs],
   [CADP_Obs.v]). Proved over the normalised semantics and carried back by
   [wbis_transfer_obs]. See [examples/README.md] for what this does and does
   not say about starvation.

   Measured 2026-10-11 (Auto and forced [MutualCofix True] alike; forced
   [False] leaves it unsolved, as for every [weak_bisimilar]): 119 steps. The bound is the
   measured count minus one ([Solve N] permits N + 1 steps). *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Norm.Fifo.bigstep_obs_norm".
Example wbis_norm : weak_bisimilar bigstep_obs_norm spec_fifo c1 (make_fifo 0).
Proof. MeBi Sim Begin bigstep_obs_norm c1 And spec_fifo (make_fifo 0) Using bigstep_obs.
  MeBi Sim Solve 118. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Norm.Fifo.transfer".
Example wbis_obs : weak_bisimilar bigstep_obs spec_fifo c1 (make_fifo 0).
Proof.
  apply wbis_transfer_obs; [apply composition_create_wf | exact wbis_norm].
Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Norm.Fifo.hidden".
(* Joining the queue hidden again ([Properties/Hidden.v]): plain [bigstep]
   is weakly bisimilar to the FIFO server with joining hidden. That server
   lets one process in at a time, so mutual exclusion for [bigstep] follows,
   by a route independent of [Size1/Glued/MutualExclusion].

   Measured 2026-10-11: [wsim_hidden_spec] 23 steps under Auto and forced
   [MutualCofix True], 23 under forced [False] (alone: this file stops at
   [wbis_norm] under [False]). The bound is the Auto count minus one. *)
Example wbis_bigstep_hidden : weak_bisimilar bigstep spec_fifo_hidden c1 (make_fifo 0).
Proof. exact (wbis_hidden c1 (make_fifo 0) wbis_obs). Qed.

Example wsim_hidden_spec : weak_sim spec_fifo_hidden spec_lts (make_fifo 0) (make_spec 0).
Proof. MeBi Sim Begin spec_fifo_hidden (make_fifo 0) And spec_lts (make_spec 0) Using spec_fifo_hidden spec_lts.
  MeBi Sim Solve 22. Qed.

Example wsim_mutex : weak_sim bigstep spec_lts c1 (make_spec 0).
Proof.
  apply (weak_sim_trans bigstep spec_fifo_hidden spec_lts c1 (make_fifo 0) (make_spec 0)).
  - apply weak_bisimilar_sim, wbis_bigstep_hidden.
  - exact wsim_hidden_spec.
Qed.
