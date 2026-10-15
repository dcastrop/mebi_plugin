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

Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecMutex.

(* Mutual exclusion against the lock server [spec_lts] ([SpecMutex.v]), for
   one process, in both semantics: layered ([lts] over [step]) and glued
   ([bigstep]). With one process the system and the spec are weakly
   bisimilar; with two or more they are only mutually similar
   ([MutualExclusionLarge.v], [examples/README.md]).

   Bounds are the measured count minus one ([Solve N] permits N + 1
   steps). The [weak_bisimilar] proofs need the mutual cofix, which [Auto]
   chooses for them; forced to the nested strategy the layered one is
   unfinished after 50000 steps. So they come last: a run with the strategy
   forced still reaches every [weak_sim] first.

   The layered proofs were in [Size1/MutualExclusion/PluginProofs.v] and
   the glued ones in [Size1/Glued/MutualExclusion/PluginProofs.v] until
   2026-10-13. The former held a copy of the glued-against-layered proofs
   ([GluedLayered.v]) from 2026-03-16 to 2026-10-11; these are the goals
   its name and its original version ([2da782e]) stated. *)

MeBi Divider "Examples.CADP.MutualExclusion".

MeBi Config Weak As Option label.

Require Import Logic.


(**************************************************)
(* One process, layered. *)

MeBi Config Bounds As Num States 100.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts".
Example wsim_lts_1 : weak_sim lts spec_lts c1 (make_spec 0).
Proof. MeBi Sim Begin lts c1 And spec_lts (make_spec 0) Using step.
  MeBi Sim Solve 258. Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts.spec_lts".
Example wsim_spec_lts_1 : weak_sim spec_lts lts (make_spec 0) c1.
Proof. MeBi Sim Begin spec_lts (make_spec 0) And lts c1 Using step.
  MeBi Sim Solve 211. Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_lts_1 : mutual_sim lts spec_lts c1 (make_spec 0).
Proof. split; [exact wsim_lts_1 | exact wsim_spec_lts_1]. Qed.


(**************************************************)
(* One process, glued. *)

MeBi Config Bounds As Num States 2000.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep".
Example wsim_bigstep_1 : weak_sim bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. MeBi Sim Begin bigstep (composition_create 0 Protocol.P) And spec_lts (make_spec 0) Using lts step.
  (* Iteration History: 82 <- _ <- _ <- _ *)
  MeBi Sim Solve 82. Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep.spec_lts".
Example wsim_spec_bigstep_1 : weak_sim spec_lts bigstep (make_spec 0) (composition_create 0 Protocol.P).
Proof. MeBi Sim Begin spec_lts (make_spec 0) And bigstep (composition_create 0 Protocol.P) Using lts step.
  (* Iteration History: 64 <- _ <- _ <- _ *)
  MeBi Sim Solve 64. Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_bigstep_1 : mutual_sim bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. split; [exact wsim_bigstep_1 | exact wsim_spec_bigstep_1]. Qed.


(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]), one per semantics -- stronger than the [mutual_sim]s
   above. *)

MeBi Config Bounds As Num States 100.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts.bisimilar".
Example wbis_lts_1 : weak_bisimilar lts spec_lts c1 (make_spec 0).
Proof. MeBi Sim Begin lts c1 And spec_lts (make_spec 0) Using step.
  MeBi Sim Solve 1441. Qed.

MeBi Config Bounds As Num States 2000.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep.bisimilar".
Example wbis_bigstep_1 : weak_bisimilar bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. MeBi Sim Begin bigstep (composition_create 0 Protocol.P) And spec_lts (make_spec 0) Using lts step.
  MeBi Sim Solve 184. Qed.
