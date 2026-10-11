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

Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.Fifo.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Norm.Fifo.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 1000.

Require Import Logic.

(* The MCS lock is weakly bisimilar to the FIFO lock server [spec_fifo]
   ([Properties/Fifo.v]), with joining the queue visible ([bigstep_obs],
   [CADP_Obs.v]). Proved over the normalised semantics and carried back by
   [wbis_transfer_obs]. See [examples/README.md] for what this does and does
   not say about starvation.

   Measured 2026-10-11 (Auto and forced [MutualCofix True] alike; forced
   [False] leaves it unsolved, as for every [weak_bisimilar]): 2228 steps. The bound is the
   measured count minus one ([Solve N] permits N + 1 steps). *)

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Norm.Fifo.bigstep_obs_norm".
Example wbis_norm : weak_bisimilar bigstep_obs_norm spec_fifo c2 (make_fifo 1).
Proof. MeBi Sim Begin bigstep_obs_norm c2 And spec_fifo (make_fifo 1) Using bigstep_obs.
  MeBi Sim Solve 2227. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Norm.Fifo.transfer".
Example wbis_obs : weak_bisimilar bigstep_obs spec_fifo c2 (make_fifo 1).
Proof.
  apply wbis_transfer_obs; [apply composition_create_wf | exact wbis_norm].
Qed.
