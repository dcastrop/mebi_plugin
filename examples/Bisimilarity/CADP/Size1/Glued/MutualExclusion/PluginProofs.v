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

Require Import MEBI.Examples.Bisimilarity.CADP.Size1.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 2000.

Require Import Logic.

(* MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion". *)
(* MeBi Run FSM (make_spec 0) Using spec_lts. *)
(* MeBi Run FSM (composition_create 0 Protocol.P) Using lts step. *)


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.bigstep".
Example wsim_bigstep : weak_sim bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. MeBi Sim Begin bigstep (composition_create 0 Protocol.P) And spec_lts (make_spec 0) Using lts step.
  (* Iteration History: 82 <- _ <- _ <- _ *)
  MeBi Sim Solve 82. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.spec_lts".
Example wsim_spec_lts : weak_sim spec_lts bigstep (make_spec 0) (composition_create 0 Protocol.P).
Proof. MeBi Sim Begin spec_lts (make_spec 0) And bigstep (composition_create 0 Protocol.P) Using lts step.
  (* Iteration History: 64 <- _ <- _ <- _ *)
  MeBi Sim Solve 64. Qed.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.PluginProofs.ProofTest.bigstep.mutual".
(* Mutual similarity: the two [weak_sim]s above, one each way. *)
Example msim_bigstep : mutual_sim bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. split; [exact wsim_bigstep | exact wsim_spec_lts]. Qed.

(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]), one per pair -- stronger than the [mutual_sim]s above.
   These need the mutual cofix, which [Auto] chooses for every one of them;
   forced to the nested strategy, all but [Glued/MutualExclusion]'s are
   unfinished after 20000 steps, which is why they come last: a run with the
   strategy forced still reaches every [weak_sim] above. Bounds are the
   measured count minus one ([Solve N] permits N + 1 steps). *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Glued.MutualExclusion.PluginProofs.ProofTest.bigstep.bisimilar".
Example wbis_bigstep : weak_bisimilar bigstep spec_lts (composition_create 0 Protocol.P) (make_spec 0).
Proof. MeBi Sim Begin bigstep (composition_create 0 Protocol.P) And spec_lts (make_spec 0) Using lts step.
  MeBi Sim Solve 184. Qed.
