Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Simple.

Require Import MEBI.Examples.bisimilarity.Proc.Test1.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests".
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
MeBi Config Output Debug Enable.
MeBi Config Output Info Enable.
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
(* MeBi Config Bound 100. *)
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)

MeBi Config Output Disable.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests".
MeBi FSM p Using termLTS. MeBi Saturate p Using termLTS.
MeBi FSM q Using termLTS. MeBi Saturate q Using termLTS.
MeBi FSM r Using termLTS. MeBi Saturate r Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.pq".
MeBi Bisim p With termLTS And q With termLTS Using termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.qp".
MeBi Bisim q With termLTS And p With termLTS Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.qr".
MeBi Bisim q With termLTS And r With termLTS Using termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.rq".
MeBi Bisim r With termLTS And q With termLTS Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.pr".
MeBi Bisim p With termLTS And r With termLTS Using termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.rp".
MeBi Bisim r With termLTS And p With termLTS Using termLTS.
