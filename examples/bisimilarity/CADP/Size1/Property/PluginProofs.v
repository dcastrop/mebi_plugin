Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Require Import MEBI.Examples.bisimilarity.CADP.Size1.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.PluginProofs".

MeBi Config Weak As Option label.

Require Import Logic.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.PluginProofs.property".
Inductive spec_state : Type :=
| Free : nat -> spec_state
| Held : nat -> spec_state
.

Inductive spec_lts : spec_state -> option label -> spec_state -> Prop :=
| SVC_ENTER : forall i, spec_lts (Free i) (Some (ENTER, i)) (Held i)
| SVC_LEAVE : forall i, spec_lts (Held i) (Some (LEAVE, i)) (Free i)
.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.PluginProofs.original".
Example wsim_lts_spec_lts : weak_sim lts spec_lts c1 (Free 0). 
Proof. MeBi Sim Begin lts c1 And spec_lts (Free 0) Using step.
  (* Iteration History: 290 <- _ <- _ <- _ *) 
  MeBi Sim Solve 290. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.PluginProofs.original".
Example wsim_spec_lts_lts : weak_sim spec_lts lts (Free 0) c1. 
Proof. MeBi Sim Begin spec_lts (Free 0) And lts c1 Using step. 
  (* Iteration History: 207 <- _ <- _ <- _  *) 
  MeBi Sim Solve 207. Qed.

(****************************)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.PluginProofs.glued".
Example wsim_bigstep_spec_lts : weak_sim bigstep spec_lts c1 (Free 0). 
Proof. MeBi Sim Begin bigstep c1 And spec_lts (Free 0).
  (* Iteration History: 82 <- _ <- _ <- _ *) 
  MeBi Sim Solve 82. Qed.


MeBi Divider "Examples.Bisimilarity.CADP.Size1.Property.PluginProofs.glued".
Example wsim_spec_lts_bigstep : weak_sim spec_lts bigstep (Free 0) c1. 
Proof. MeBi Sim Begin spec_lts (Free 0) And bigstep c1. 
  (* Iteration History: 59 <- _ <- _ <- _  *) 
  MeBi Sim Solve 59. Qed.
