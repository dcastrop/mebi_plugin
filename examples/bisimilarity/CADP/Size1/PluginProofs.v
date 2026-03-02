Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.bisimilarity.CADP.Size1.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs".

MeBi Config Weak As Option label.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.bigstep_lts".
Example wsim_bigstep_lts : weak_sim bigstep lts c1 c1. 
Proof. MeBiSim Begin bigstep c1 And lts c1 Using step.
  (* Iteration History: _ <- 300 <- _ *) 
  (* TODO: check if unfolding is correct. *)
  (* NOTE: saturation algorithm takes a long time with parl ones like this. *)
  MeBiSim Solve 300. Qed.

  (* 
MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.lts_bigstep".
Example wsim_lts_bigstep : weak_sim lts bigstep c1 c1. 
Proof. MeBiSim Begin lts c1 And bigstep c1 Using step. 
  (* TODO: inversion seems to be looping again *)
MeBiSim Solve 2000. Qed.
  (* MeBiSim Solve 15. *)
  (* MeBiSim Solve 8. *)
  (* MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.
  MeBiSim Solve 10.

  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step.
  MeBiSim Step. *)
Admitted. *)
