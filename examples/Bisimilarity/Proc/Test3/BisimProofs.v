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
Require Import MEBI.Examples.Proc.
Import Layered.

Require Import MEBI.Examples.Bisimilarity.Proc.Test3.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.BisimProofs".

MeBi Config Weak As Option label.

(* One [weak_bisimilar] proof per pair, where [PluginProofs.v] proves two
   separate [weak_sim]s (mutual similarity, a weaker statement). These need
   the mutual cofix, which [Auto] chooses for every one of them: forced to
   the nested strategy, all but [Glued/MutualExclusion] are unfinished after
   20000 steps. Bounds are the measured count minus one ([Solve N] permits
   N + 1 steps). See ASSISTED-CHANGES.md, 2026-10-02, option A part 2. *)

(* This file needs one mutual cofix over the whole precomputed product
   relation, instead of a fresh nested cofix per newly-seen pair. Without it
   it does not finish: [wsim_pq] exhausted [Solve 100000] and [wsim_p3] was
   recorded as unfinished after 500000 and crashing at 1000000.

   The default strategy is [Auto], which measures both on the model before
   the proof starts and takes the mutual path here on its own -- it says so
   at Notice. The setting below is left explicit so the file still works if
   someone changes that default, and as a record of what it depends on. See
   ASSISTED-CHANGES.md, 2026-09-29, and backlog item B2. *)
MeBi Config Solver MutualCofix True.

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

MeBi Divider "Examples.Bisimilarity.Proc.Test3.BisimProofs.ProofTest.p3".
Example wbis_p3 : weak_bisimilar compLTS compLTS p3a p3b.
Proof. MeBi Sim Begin compLTS p3a And compLTS p3b Using compLTS termLTS.
  MeBi Sim Solve 14426. Qed.

(**************************************************)
MeBi Divider "Examples.Bisimilarity.Proc.Test3.BisimProofs.ProofTest.pq".
Example wbis_pq : weak_bisimilar compLTS compLTS p q.
Proof. MeBi Sim Begin compLTS p And compLTS q Using compLTS termLTS.
  MeBi Sim Solve 6786. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.BisimProofs.ProofTest.qr".
Example wbis_qr : weak_bisimilar compLTS compLTS q r.
Proof. MeBi Sim Begin compLTS q And compLTS r Using compLTS termLTS.
  MeBi Sim Solve 10242. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.BisimProofs.ProofTest.pr".
Example wbis_pr : weak_bisimilar compLTS compLTS p r.
Proof. MeBi Sim Begin compLTS p And compLTS r Using compLTS termLTS.
  MeBi Sim Solve 4466. Qed.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.BisimProofs.ProofTest.rs".
Example wbis_rs : weak_bisimilar compLTS compLTS r s.
Proof. MeBi Sim Begin compLTS r And compLTS s Using compLTS termLTS.
  MeBi Sim Solve 6754. Qed.

