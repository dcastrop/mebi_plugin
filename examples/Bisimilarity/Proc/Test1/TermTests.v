Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Simple.

Require Import MEBI.Examples.Bisimilarity.Proc.Test1.Terms.

MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests".
MeBi Run FSM p Using termLTS. MeBi Run Saturate p Using termLTS.
MeBi Run FSM q Using termLTS. MeBi Run Saturate q Using termLTS.
MeBi Run FSM r Using termLTS. MeBi Run Saturate r Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.pq".
MeBi Run Bisim p With termLTS And q With termLTS Using termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.qp".
MeBi Run Bisim q With termLTS And p With termLTS Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.qr".
MeBi Run Bisim q With termLTS And r With termLTS Using termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.rq".
MeBi Run Bisim r With termLTS And q With termLTS Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.pr".
MeBi Run Bisim p With termLTS And r With termLTS Using termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test1.TermTests.Bisim.rp".
MeBi Run Bisim r With termLTS And p With termLTS Using termLTS.
