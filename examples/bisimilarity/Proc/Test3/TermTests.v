Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Layered.

Require Import MEBI.Examples.bisimilarity.Proc.Test3.Terms.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests".
MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.p".
MeBi FSM p Using compLTS termLTS. 
MeBi Saturate p Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.q".
MeBi FSM q Using compLTS termLTS. 
MeBi Saturate q Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.r".
MeBi FSM r Using compLTS termLTS. 
MeBi Saturate r Using compLTS termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.s".
MeBi FSM s Using compLTS termLTS. 
MeBi Saturate s Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.pq".
MeBi Bisim p With compLTS And q With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.qp".
MeBi Bisim q With compLTS And p With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.qr".
MeBi Bisim q With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.rq".
MeBi Bisim r With compLTS And q With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.pr".
MeBi Bisim p With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.rp".
MeBi Bisim r With compLTS And p With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.rs".
MeBi Bisim r With compLTS And s With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.TermTests.Bisim.sr".
MeBi Bisim s With compLTS And r With compLTS Using compLTS termLTS. 
