Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Layered.

Require Import MEBI.Examples.bisimilarity.Proc.Test3.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs".

MeBi Config Weak As Option label.

Require Import Logic.

Example s0 : term := (tact (send A) tend).
Example r0 : term := (tact (recv A) tend).
Example p0 : comp := cpar (cprc s0) (cprc r0). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p0".
(* Example wsim_p0 : weak_sim compLTS compLTS p0 p0. 
Proof. MeBi Sim Begin compLTS p0 And compLTS p0 Using compLTS termLTS. 
  (* Iteration History: 2 <- 283 <- 425 <- _ *) 
  MeBi Sim Solve 2.
Qed. *)

Example s1 : term := tfix (tact (send A) trec).
Example r1 : term := tfix (tact (recv A) trec).
Example p1 : comp := cpar (cprc s1) (cprc r1). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p1".
(* Example wsim_p1 : weak_sim compLTS compLTS p1 p1. 
Proof. MeBi Sim Begin compLTS p1 And compLTS p1 Using compLTS termLTS. 
  (* Iteration History: 2 <- 2835 <- _ <- _ *) 
  MeBi Sim Solve 2.
Qed. *)

Example s1b : term := tfix (tact (send A) trec).
Example r1b : term := tfix (tact (recv A) trec).
Example p1b : comp := cpar (cpar (cprc tend) (cprc s1b)) (cprc r1b). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p1b".
(* Example wsim_p1b : weak_sim compLTS compLTS p1b p1b. 
Proof. MeBi Sim Begin compLTS p1b And compLTS p1b Using compLTS termLTS. 
  (* Iteration History: 2 <- _ <- _ <- _ *) 
  MeBi Sim Solve 2.
Qed. *)

Example s2 : term := tfix (tseq (tact (send A) tend) trec).
Example r2 : term := tfix (tseq (tact (recv A) tend) trec).
Example p2 : comp := cpar (cprc s2) (cprc r2). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p2".
(* Example wsim_p2 : weak_sim compLTS compLTS p2 p2. 
Proof. MeBi Sim Begin compLTS p2 And compLTS p2 Using compLTS termLTS. 
  (* Iteration History: 2 <- _ <- _ <- _ *) 
  MeBi Sim Solve 2.
Qed. *)

Example s3 : term := tfix (tseq (tact (send A) tend) trec).
Example r3 : term := tfix (tseq (tact (recv A) tend) trec).
Example p3a : comp := cpar (cprc s3) (cprc r3). 
Example p3b : comp := cpar (cprc r3) (cprc s3). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.p3".
(* Example wsim_p3 : weak_sim compLTS compLTS p3a p3b. 
Proof. MeBi Sim Begin compLTS p3a And compLTS p3b Using compLTS termLTS. 
  (* unfinished after 500000, crashed on 1000000. *)
  MeBi Sim Solve 100000.
  (* MeBi Sim Solve 100000. *)
  (* MeBi Sim Solve 100000. *)
  (* MeBi Sim Solve 100000. *)
  (* MeBi Sim Solve 100000. *)
  (* MeBi Sim Solve 100000. *)
  (* MeBi Sim Solve 100000. Qed. *)
Admitted. *)

(**************************************************)
MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pq".
(* Example wsim_pq : weak_sim compLTS compLTS p q. 
Proof. MeBi Sim Begin compLTS p And compLTS q Using compLTS termLTS. 
  MeBi Sim Solve 100000. Qed. *)
  (* MeBi Sim Solve 500000. Qed. *)

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qp".
(* Example wsim_qp : weak_sim compLTS compLTS q p. 
Proof. MeBi Sim Begin compLTS q And compLTS p Using compLTS termLTS. 
  MeBi Sim Solve 100000.
Qed. *)


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.qr".
(* Example wsim_qr : weak_sim compLTS compLTS q r. 
Proof. MeBi Sim Begin compLTS q And compLTS r Using compLTS termLTS. 
  MeBi Sim Solve 100000.
Qed. *)

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rq".
(* Example wsim_rq : weak_sim compLTS compLTS r q. 
Proof. MeBi Sim Begin compLTS r And compLTS q Using compLTS termLTS. 
  MeBi Sim Solve 100000.
Qed. *)


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.pr".
(* Example wsim_pr : weak_sim compLTS compLTS p r. 
Proof. MeBi Sim Begin compLTS p And compLTS r Using compLTS termLTS. 
  MeBi Sim Solve 100000.
Qed. *)

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rp".
(* Example wsim_rp : weak_sim compLTS compLTS r p. 
Proof. MeBi Sim Begin compLTS r And compLTS p Using compLTS termLTS. 
  MeBi Sim Solve 100000.
Qed. *)


MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.rs".
(* Example wsim_rp : weak_sim compLTS compLTS r s. 
Proof. MeBi Sim Begin compLTS r And compLTS s Using compLTS termLTS. 
  MeBi Sim Solve 100000.
Qed. *)

MeBi Divider "Examples.Bisimilarity.Proc.Test3.PluginProofs.ProofTest.sr".
Example wsim_pr : weak_sim compLTS compLTS s r. 
Proof. MeBi Sim Begin compLTS s And compLTS r Using compLTS termLTS. 
  MeBi Sim Solve 100000.
Qed. 
