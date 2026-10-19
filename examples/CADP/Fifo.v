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
Require Import MEBI.Examples.CADP.Obs.

Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecFifo.
Require Import MEBI.Examples.CADP.SpecMutex.
Require Import MEBI.Examples.CADP.Hidden.

(* The MCS lock is weakly bisimilar to the FIFO lock server [spec_fifo]
   ([SpecFifo.v]), with joining the queue visible ([bigstep_obs],
   [Obs.v]). Proved over the normalised semantics and carried back by
   [wbis_transfer_obs]. See [examples/README.md] for what this does and does
   not say about starvation.

   Then, with joining the queue hidden again ([Hidden.v]): plain [bigstep]
   is weakly bisimilar to the FIFO server with joining hidden. That server
   lets one process in at a time, so mutual exclusion for [bigstep]
   follows, by a route independent of [MutualExclusion.v].

   For three processes the FIFO bisimilarity is only decided ([Explore.v]).
   Bounds are the measured count minus one ([Solve N] permits N + 1 steps).
   Until 2026-10-13 these were [Size1/Norm/Fifo] and [Size2/Norm/Fifo]. *)

MeBi Divider "Examples.CADP.Fifo".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 1000.

Require Import Logic.


(**************************************************)
(* One process.

   Measured 2026-10-11 (Auto and forced [MutualCofix True] alike; forced
   [False] leaves it unsolved, as for every [weak_bisimilar]):
   [wbis_norm_1] 119 steps. [wsim_hidden_spec_1] 23 steps under Auto and
   forced [MutualCofix True], 23 under forced [False]. *)

MeBi Divider "Examples.CADP.Fifo.Size1.bigstep_obs_norm".
Example wbis_norm_1 : weak_bisimilar bigstep_obs_norm spec_fifo c1 (make_fifo 0).
Proof. MeBi Sim Begin bigstep_obs_norm c1 And spec_fifo (make_fifo 0) Using bigstep_obs.
  MeBi Sim Solve 118. Qed.

MeBi Divider "Examples.CADP.Fifo.Size1.transfer".
Example wbis_obs_1 : weak_bisimilar bigstep_obs spec_fifo c1 (make_fifo 0).
Proof.
  apply wbis_transfer_obs; [apply composition_create_wf | exact wbis_norm_1].
Qed.

MeBi Divider "Examples.CADP.Fifo.Size1.hidden".
Example wbis_bigstep_hidden_1 : weak_bisimilar bigstep spec_fifo_hidden c1 (make_fifo 0).
Proof. exact (wbis_hidden c1 (make_fifo 0) wbis_obs_1). Qed.

Example wsim_hidden_spec_1 : weak_sim spec_fifo_hidden spec_lts (make_fifo 0) (make_spec 0).
Proof. MeBi Sim Begin spec_fifo_hidden (make_fifo 0) And spec_lts (make_spec 0) Using spec_fifo_hidden spec_lts.
  MeBi Sim Solve 22. Qed.

Example wsim_mutex_1 : weak_sim bigstep spec_lts c1 (make_spec 0).
Proof.
  apply (weak_sim_trans bigstep spec_fifo_hidden spec_lts c1 (make_fifo 0) (make_spec 0)).
  - apply weak_bisimilar_sim, wbis_bigstep_hidden_1.
  - exact wsim_hidden_spec_1.
Qed.


(**************************************************)
(* Two processes.

   Measured 2026-10-11 (Auto and forced [MutualCofix True] alike; forced
   [False] leaves it unsolved): [wbis_norm_2] 2228 steps, ~24s.
   [wsim_hidden_spec_2] 98 steps under Auto and forced [MutualCofix True],
   321 under forced [False]. *)

MeBi Divider "Examples.CADP.Fifo.Size2.bigstep_obs_norm".
Example wbis_norm_2 : weak_bisimilar bigstep_obs_norm spec_fifo c2 (make_fifo 1).
Proof. MeBi Sim Begin bigstep_obs_norm c2 And spec_fifo (make_fifo 1) Using bigstep_obs.
  MeBi Sim Solve 2227. Qed.

MeBi Divider "Examples.CADP.Fifo.Size2.transfer".
Example wbis_obs_2 : weak_bisimilar bigstep_obs spec_fifo c2 (make_fifo 1).
Proof.
  apply wbis_transfer_obs; [apply composition_create_wf | exact wbis_norm_2].
Qed.

MeBi Divider "Examples.CADP.Fifo.Size2.hidden".
Example wbis_bigstep_hidden_2 : weak_bisimilar bigstep spec_fifo_hidden c2 (make_fifo 1).
Proof. exact (wbis_hidden c2 (make_fifo 1) wbis_obs_2). Qed.

Example wsim_hidden_spec_2 : weak_sim spec_fifo_hidden spec_lts (make_fifo 1) (make_spec 1).
Proof. MeBi Sim Begin spec_fifo_hidden (make_fifo 1) And spec_lts (make_spec 1) Using spec_fifo_hidden spec_lts.
  MeBi Sim Solve 97. Qed.

Example wsim_mutex_2 : weak_sim bigstep spec_lts c2 (make_spec 1).
Proof.
  apply (weak_sim_trans bigstep spec_fifo_hidden spec_lts c2 (make_fifo 1) (make_spec 1)).
  - apply weak_bisimilar_sim, wbis_bigstep_hidden_2.
  - exact wsim_hidden_spec_2.
Qed.
