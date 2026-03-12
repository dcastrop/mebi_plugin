Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.bisimilarity.CADP.Size1.Terms.
Require Import MEBI.Examples.bisimilarity.CADP.properties.mutual_exclusion.
(* Require Import MEBI.Examples.bisimilarity.CADP.properties.no_starvation. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.PluginProofs.mutual_exclusion".

MeBi Config Weak As Option label.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.TermTests.c1".
MeBi Config Bounds As Num States 5000.
(* MeBi Run FSM (make_log c1) Using valid_lts lts step. *)
(* MeBi Run Saturate (make_log c1) Using valid_lts lts step. *)
(* MeBi Run Minimize (make_log c1) Using valid_lts lts step. *)

(* MeBi Run Bisim (make_log c1) With valid_lts And c1 With lts Using lts step. *)
(* MeBi Run Bisim c1 With lts And (make_log c1) With valid_lts Using lts step. *)


(* NOTE: issue is that [valid_lts] constructor has a non-lts premise 
  e.g., [True /\ True] which the plugin doesn't account for... yet *)
MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.mutual_exclusion.valid_lts.original".
Example wsim_lts_valid_lts : weak_sim lts valid_lts c1 (make_log c1). 
Proof. MeBi Sim Begin lts c1 And valid_lts (make_log c1) Using lts step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.mutual_exclusion.valid_lts.original".
(* Example wsim_valid_lts_lts : weak_sim valid_lts lts (make_log c1) c1. 
Proof. MeBi Sim Begin valid_lts (make_log c1) And lts c1 Using lts step. 
  (* Iteration History: _ <- _ <- _ <- _  *) 
  MeBi Sim Solve 1000. Qed.

(****************************)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.mutual_exclusion.valid_lts.glued".
Example wsim_bigstep_valid_lts : weak_sim bigstep valid_lts c1 (make_log c1). 
Proof. MeBi Sim Begin bigstep c1 And valid_lts (make_log c1) Using lts step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.mutual_exclusion.valid_lts.glued".
Example wsim_valid_lts_bigstep : weak_sim valid_lts bigstep (make_log c1) c1. 
Proof. MeBi Sim Begin valid_lts (make_log c1) And bigstep c1 Using lts step. 
  (* Iteration History: _ <- _ <- _ <- _  *) 
  MeBi Sim Solve 1000. Qed. *)

(**************************************************)

(* MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.TermTests.c1.bigstep". *)
(* MeBi Run FSM (make_log c1) Using valid_big bigstep lts step. *)
(* MeBi Run Saturate (make_log c1) Using valid_big bigstep lts step. *)
(* MeBi Run Minimize (make_log c1) Using valid_big bigstep lts step. *)
(* MeBi Run Bisim (make_log c1) With valid_lts And c1 With bigstep Using lts step. *)