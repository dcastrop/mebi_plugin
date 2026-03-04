Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Layered.

Require Import MEBI.Examples.bisimilarity.Proc.Test4.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests".
MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.p".
MeBi Run FSM p Using compLTS termLTS. 
MeBi Run Saturate p Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.q".
MeBi Run FSM q Using compLTS termLTS. 
MeBi Run Saturate q Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.r".
MeBi Run FSM r Using compLTS termLTS. 
MeBi Run Saturate r Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.pq".
MeBi Run Bisim p With compLTS And q With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.qp".
MeBi Run Bisim q With compLTS And p With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.qr".
MeBi Run Bisim q With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.rq".
MeBi Run Bisim r With compLTS And q With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.pr".
MeBi Run Bisim p With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test4.TermTests.Bisim.rp".
MeBi Run Bisim r With compLTS And p With compLTS Using compLTS termLTS.
