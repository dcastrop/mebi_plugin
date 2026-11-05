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

Set MeBi MaxStates 100.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts".
Set MeBi MaxSteps 258.
Example wsim_lts_1 : weak_sim lts spec_lts c1 (make_spec 0).
Proof. sim. Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts.spec_lts".
Set MeBi MaxSteps 201.
Example wsim_spec_lts_1 : weak_sim spec_lts lts (make_spec 0) c1.
Proof. sim. Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_lts_1 : mutual_sim lts spec_lts c1 (make_spec 0).
Proof. split; [exact wsim_lts_1 | exact wsim_spec_lts_1]. Qed.


(**************************************************)
(* One process, glued. *)

Set MeBi MaxStates 2000.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep".
Set MeBi MaxSteps 82.
Example wsim_bigstep_1 : weak_sim bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. sim.
  (* Iteration History: 82 <- _ <- _ <- _ *)
   Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep.spec_lts".
Set MeBi MaxSteps 64.
Example wsim_spec_bigstep_1 : weak_sim spec_lts bigstep (make_spec 0) (composition_create 0 Protocol.P).
Proof. sim.
  (* Iteration History: 64 <- _ <- _ <- _ *)
   Qed.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_bigstep_1 : mutual_sim bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. split; [exact wsim_bigstep_1 | exact wsim_spec_bigstep_1]. Qed.


(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]), one per semantics -- stronger than the [mutual_sim]s
   above. *)

Set MeBi MaxStates 100.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.lts.bisimilar".
Set MeBi MaxSteps 1441.
Example wbis_lts_1 : weak_bisimilar lts spec_lts c1 (make_spec 0).
Proof. bisim. Qed.

Set MeBi MaxStates 2000.

MeBi Divider "Examples.CADP.MutualExclusion.Size1.bigstep.bisimilar".
Set MeBi MaxSteps 184.
Example wbis_bigstep_1 : weak_bisimilar bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. bisim. Qed.
