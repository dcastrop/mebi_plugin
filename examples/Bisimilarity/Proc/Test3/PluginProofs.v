Require Import MEBI.loader.


Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Stacked.

Require Import MEBI.Examples.Bisimilarity.Proc.Test3.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs".


(* Until 2026-11-08 this file needed the removed solver's mutual cofix
   over the whole product relation (without it [wsim_pq] exhausted 100000
   steps; ASSISTED-CHANGES.md, 2026-09-29, backlog item B2). With
   certificates it takes about a second. *)

Require Import Logic.

Example s0 : term := (tact (send A) tend).
Example r0 : term := (tact (recv A) tend).
Example p0 : comp := cpar (cprc s0) (cprc r0). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p0".
(* Example wsim_p0 : weak_sim compLTS compLTS p0 p0.
Proof. weak_sim. Qed. *)

Example s1 : term := tfix (tact (send A) trec).
Example r1 : term := tfix (tact (recv A) trec).
Example p1 : comp := cpar (cprc s1) (cprc r1). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p1".
(* Example wsim_p1 : weak_sim compLTS compLTS p1 p1.
Proof. weak_sim. Qed. *)

Example s1b : term := tfix (tact (send A) trec).
Example r1b : term := tfix (tact (recv A) trec).
Example p1b : comp := cpar (cpar (cprc tend) (cprc s1b)) (cprc r1b). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p1b".
(* Example wsim_p1b : weak_sim compLTS compLTS p1b p1b.
Proof. weak_sim. Qed. *)

Example s2 : term := tfix (tseq (tact (send A) tend) trec).
Example r2 : term := tfix (tseq (tact (recv A) tend) trec).
Example p2 : comp := cpar (cprc s2) (cprc r2). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p2".
(* Example wsim_p2 : weak_sim compLTS compLTS p2 p2.
Proof. weak_sim. Qed. *)

Example s3 : term := tfix (tseq (tact (send A) tend) trec).
Example r3 : term := tfix (tseq (tact (recv A) tend) trec).
Example p3a : comp := cpar (cprc s3) (cprc r3). 
Example p3b : comp := cpar (cprc r3) (cprc s3). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p3".
(* Was: unfinished after 500000, crashed on 1000000. 42 pairs. *)
Example wsim_p3 : weak_sim compLTS compLTS p3a p3b. 
Proof. weak_sim. Qed.

(**************************************************)
MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pq".
Example wsim_pq : weak_sim compLTS compLTS p q. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qp".
Example wsim_qp : weak_sim compLTS compLTS q p. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pq.mutual".


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qr".
Example wsim_qr : weak_sim compLTS compLTS q r. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rq".
Example wsim_rq : weak_sim compLTS compLTS r q. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qr.mutual".


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pr".
Example wsim_pr : weak_sim compLTS compLTS p r. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rp".
Example wsim_rp : weak_sim compLTS compLTS r p. 
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pr.mutual".


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rs".
Example wsim_rs : weak_sim compLTS compLTS r s.
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.sr".
Example wsim_sr : weak_sim compLTS compLTS s r.
Proof. weak_sim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rs.mutual".

(**************************************************)
(* Weak bisimilarity: one relation, both directions, in one proof
   ([weak_bisimilar]), one per pair -- stronger than the [mutual_sim]s above.
   (Until 2026-11-08 these were the costly proofs of the removed solver,
   which needed its mutual cofix; git history has its step counts.) *)

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p3.bisimilar".
Example wbis_p3 : weak_bisimilar compLTS compLTS p3a p3b.
Proof. weak_bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pq.bisimilar".
Example wbis_pq : weak_bisimilar compLTS compLTS p q.
Proof. weak_bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qr.bisimilar".
Example wbis_qr : weak_bisimilar compLTS compLTS q r.
Proof. weak_bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pr.bisimilar".
Example wbis_pr : weak_bisimilar compLTS compLTS p r.
Proof. weak_bisim. Qed.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rs.bisimilar".
Example wbis_rs : weak_bisimilar compLTS compLTS r s.
Proof. weak_bisim. Qed.
