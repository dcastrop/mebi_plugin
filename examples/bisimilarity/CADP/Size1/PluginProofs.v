Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.bisimilarity.CADP.Size1.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs".

MeBi Config Weak As Option label.

Require Import Logic.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.bigstep_lts".
Example wsim_bigstep_lts : weak_sim bigstep lts c1 c1. 
Proof. MeBi Sim Begin bigstep c1 And lts c1 Using step.
  (* Iteration History: 267 <- 300 <- _ <- _ *) 
  MeBi Sim Solve 267. Qed.

  
MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.lts_bigstep".
Example wsim_lts_bigstep : weak_sim lts bigstep c1 c1. 
Proof. MeBi Sim Begin lts c1 And bigstep c1 Using step. 
  (* Iteration History: 395 <- _ <- _ <- _  *) 
  MeBi Sim Solve 395. Qed.

(****************************)

(* TODO: just copied from paper *)
(* Inductive act' : Type := | ENTER : nat -> nat -> label | LEAVE : nat -> nat -> label. *)

Inductive spec_state : Type :=
| Free : spec_state
| Held : nat -> spec_state
.

(* NOTE: needed to add pid *)
Inductive spec_lts : spec_state -> option label -> spec_state -> Prop :=
| SVC_ENTER : forall i, spec_lts Free (Some (ENTER, i)) (Held i)
| SVC_LEAVE : forall i, spec_lts (Held i) (Some (LEAVE, i)) Free
.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.bigstep_spec_lts".
(* Example wsim_bigstep_spec_lts : weak_sim bigstep spec_lts c1 Free. 
Proof. MeBi Sim Begin bigstep c1 And spec_lts Free Using step.
  (* Iteration History: _ <- _ <- _ <- _ *) 
  MeBi Sim Solve 1000. Qed. *)

  
MeBi Divider "Examples.Bisimilarity.CADP.Size1.PluginProofs.spec_lts_bigstep".
(* Example wsim_spec_lts_bigstep : weak_sim spec_lts bigstep Free c1. 
Proof. MeBi Sim Begin spec_lts Free And bigstep c1 Using step. 
  (* Iteration History: _ <- _ <- _ <- _  *) 
  MeBi Sim Solve 1000. Qed. *)
