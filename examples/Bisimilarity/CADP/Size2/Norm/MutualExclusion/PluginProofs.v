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

Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Norm.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 100.

Require Import Logic.

(* [Glued/MutualExclusion] again, proved over the normalised semantics
   [bigstep_norm] ([CADP_Norm.v]) and carried back to [bigstep] by its
   transfer theorems. [bigstep_norm c2] has 48 states against [bigstep]'s
   772: dead locals and the resource's unread [mem_next]/[mem_locked] are
   erased. The direct proof of [weak_sim bigstep spec_lts] takes 11128
   steps, ~10 min and ~4.4GB; this file takes ~15s.

   Measured 2026-10-11 (Auto, which takes the mutual cofix for both):
   [wsim_norm] 1038 steps, [wsim_norm_r] 153; forced [MutualCofix True]
   gives the same. Forced [False] (nested), [wsim_norm] was OOM-killed at
   6GB, as the direct proof over [bigstep] is. Bounds are the measured count
   minus one ([Solve N] permits N + 1 steps). *)

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Norm.MutualExclusion.bigstep_norm".
Example wsim_norm : weak_sim bigstep_norm spec_lts c2 (make_spec 1).
Proof. MeBi Sim Begin bigstep_norm c2 And spec_lts (make_spec 1) Using bigstep.
  MeBi Sim Solve 1037. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Norm.MutualExclusion.spec_lts".
Example wsim_norm_r : weak_sim spec_lts bigstep_norm (make_spec 1) c2.
Proof. MeBi Sim Begin spec_lts (make_spec 1) And bigstep_norm c2 Using bigstep.
  MeBi Sim Solve 152. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Norm.MutualExclusion.transfer".
(* The same statements over [bigstep], by [CADP_Norm]'s transfer theorems:
   [c2] is well formed, as every [composition_create] is. *)
Example wsim_bigstep : weak_sim bigstep spec_lts c2 (make_spec 1).
Proof. apply wsim_transfer; [apply composition_create_wf | exact wsim_norm]. Qed.

Example wsim_spec_lts : weak_sim spec_lts bigstep (make_spec 1) c2.
Proof.
  apply wsim_transfer_r; [apply composition_create_wf | exact wsim_norm_r].
Qed.

Example msim_bigstep : mutual_sim bigstep spec_lts c2 (make_spec 1).
Proof. split; [exact wsim_bigstep | exact wsim_spec_lts]. Qed.
