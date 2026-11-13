(* Test4's goals, under its own semantics [compLTS], proved up to
   reordering (doc/upto.md). Not in the default build: three lemmas, each
   in about 2 minutes and 1.5GB at most; run it with [bench/prooflog.py],
   which caps its memory.

   - p, q and r each reach the same 9,720 states: 3^4 local
     configurations x 120 tree shapes, because do_comm/do_assocl/
     do_assocr rebracket and reorder the four parallel components at any
     depth. Up to that reordering (the rules shown to commute with it),
     the game runs on one state per class: 81 states, 6,561 pairs.
   - [MeBi Prove]: each piece of the proof a constant the kernel checks as
     it is declared ([Proof. weak_bisim. Qed.] proves each too, with
     3.9GB at most). Weak bisimilarity is symmetric, so the three pairs
     give the other three directions. *)

Require Import MEBI.loader.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Stacked.
Require Import MEBI.Examples.Bisimilarity.Proc.Test4.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests".

MeBi Prove wbis_pq : weak_bisimilar compLTS compLTS p q.
MeBi Prove wbis_qr : weak_bisimilar compLTS compLTS q r.
MeBi Prove wbis_pr : weak_bisimilar compLTS compLTS p r.
