Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.Bisimilarity.CADP.Size1.Terms.
Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.
(* Require Import MEBI.Examples.Bisimilarity.CADP.properties.no_starvation. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.PluginProofs".

MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 2000.

Require Import Logic.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.TermTests".

(* OK *)
(* MeBi Run FSM (compose (create 1 Protocol.P)) Using lts step. *)

(* OK *)
(* MeBi Run FSM (make_spec 1) Using spec_lts. *)

(* OK *)
(* MeBi Run Bisim (make_spec 1) With spec_lts And (compose (create 1 Protocol.P)) With lts Using step. *)



(* !!! state-explosion *)
(* MeBi Run FSM (compose (create 2 Protocol.P)) Using lts step. *) 

(* OK *)
MeBi Run FSM (make_spec 2) Using spec_lts. 

(* OK *)
MeBi Run FSM (compose (create 2 Protocol.P)) Using bigstep lts step. 

(* !!! not bisim ? *)
MeBi Run Bisim (make_spec 2) With spec_lts And (compose (create 2 Protocol.P)) With bigstep Using lts step.
























(* MeBi Run FSM (make_log c1) Using valid_lts lts step. *)
(* MeBi Run Saturate (make_log c1) Using valid_lts lts step. *)
(* MeBi Run Minimize (make_log c1) Using valid_lts lts step. *)

(* MeBi Run Bisim (make_log c1) With valid_lts And c1 With lts Using lts step. *)
(* MeBi Run Bisim c1 With lts And (make_log c1) With valid_lts Using lts step. *)


(* NOTE: issue is that [valid_lts] constructor has a non-lts premise 
  e.g., [True /\ True] which the plugin doesn't account for... yet *)
(* MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.mutual_exclusion.valid_lts.original".
Example wsim_lts_valid_lts : weak_sim lts valid_lts c1 (make_log c1). 
Proof. MeBi Sim Begin lts c1 And valid_lts (make_log c1) Using lts step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.mutual_exclusion.valid_lts.original". *)
(* Example wsim_valid_lts_lts : weak_sim valid_lts lts (make_log c1) c1. 
Proof. MeBi Sim Begin valid_lts (make_log c1) And lts c1 Using lts step. 
  (* Iteration History: _ <- _ <- _ <- _  *) 
  MeBi Sim Solve 1000. Qed.

(****************************)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.mutual_exclusion.valid_lts.glued".
Example wsim_bigstep_valid_lts : weak_sim bigstep valid_lts c1 (make_log c1). 
Proof. MeBi Sim Begin bigstep c1 And valid_lts (make_log c1) Using lts step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.mutual_exclusion.valid_lts.glued".
Example wsim_valid_lts_bigstep : weak_sim valid_lts bigstep (make_log c1) c1. 
Proof. MeBi Sim Begin valid_lts (make_log c1) And bigstep c1 Using lts step. 
  (* Iteration History: _ <- _ <- _ <- _  *) 
  MeBi Sim Solve 1000. Qed. *)

(**************************************************)

(* MeBi Divider "Examples.Bisimilarity.CADP.Size1.MutualExclusion.TermTests.c1.bigstep". *)
(* MeBi Run FSM (make_log c1) Using valid_big bigstep lts step. *)
(* MeBi Run Saturate (make_log c1) Using valid_big bigstep lts step. *)
(* MeBi Run Minimize (make_log c1) Using valid_big bigstep lts step. *)
(* MeBi Run Bisim (make_log c1) With valid_lts And c1 With bigstep Using lts step. *)