Require Import MEBI.loader.


Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Complex.

Require Import MEBI.Examples.Bisimilarity.Proc.Test2.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs".


Require Import Logic.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pq.mutual".


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qr.mutual".


MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pr.mutual".

(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]), one per pair -- stronger than the [mutual_sim]s above.
   (Until 2026-11-08 these were the costly proofs of the removed solver,
   which needed its mutual cofix; git history has its step counts.) *)

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pq.bisimilar".
Example wbis_pq : weak_bisimilar termLTS termLTS p q.
Proof. weak_bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.qr.bisimilar".
Example wbis_qr : weak_bisimilar termLTS termLTS q r.
Proof. weak_bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.PluginProofs.ProofTest.pr.bisimilar".
Example wbis_pr : weak_bisimilar termLTS termLTS p r.
Proof. weak_bisim. Qed.
