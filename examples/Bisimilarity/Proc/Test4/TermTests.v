(* KNOWN LIMIT: the direct proofs of Test4, under its own semantics
   [compLTS]. Not in any build.

   - p, q and r each reach the same 9,720 states (87,480 steps, 85% silent):
     3^4 local configurations x 120 tree shapes, because do_comm/do_assocl/
     do_assocr rebracket and reorder the four parallel components at any
     depth. In general 3^k * (2k-2)!/(k-1)! for k components: 18, 324,
     9720, 408240.
   - All 9,720^2 pairs are weakly bisimilar. The way on is to prove up
     to that reordering (doc/optimisations.md, 5; doc/upto.md). *)

Require Import MEBI.loader.


Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Stacked.

Require Import MEBI.Examples.Bisimilarity.Proc.Test4.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests".
Set MeBi Log Mode "keep".

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.p".

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.q".

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.r".

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.pq".
Example wbis_pq : weak_bisimilar compLTS compLTS p q.
Proof. weak_bisim. Abort.
MeBi Check Log ".TermTests.v.mebi/wbis_pq.log".
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.qp".
Example wbis_qp : weak_bisimilar compLTS compLTS q p.
Proof. weak_bisim. Abort.
MeBi Check Log ".TermTests.v.mebi/wbis_qp.log".


MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.qr".
Example wbis_qr : weak_bisimilar compLTS compLTS q r.
Proof. weak_bisim. Abort.
MeBi Check Log ".TermTests.v.mebi/wbis_qr.log".
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.rq".
Example wbis_rq : weak_bisimilar compLTS compLTS r q.
Proof. weak_bisim. Abort.
MeBi Check Log ".TermTests.v.mebi/wbis_rq.log".


MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.pr".
Example wbis_pr : weak_bisimilar compLTS compLTS p r.
Proof. weak_bisim. Abort.
MeBi Check Log ".TermTests.v.mebi/wbis_pr.log".
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.rp".
Example wbis_rp : weak_bisimilar compLTS compLTS r p.
Proof. weak_bisim. Abort.
MeBi Check Log ".TermTests.v.mebi/wbis_rp.log".
