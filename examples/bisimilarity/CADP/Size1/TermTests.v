Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.
Require Import MEBI.Examples.bisimilarity.CADP.Size1.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests".
MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.p1".
MeBi Run FSM p1 Using step.
MeBi Run Saturate p1 Using step.
MeBi Run Minimize p1 Using step.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.c1.step".
MeBi Run FSM c1 Using lts step.
MeBi Run Saturate c1 Using lts step.
MeBi Run Minimize c1 Using lts step.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.c1.bigstep".
MeBi Run FSM c1 Using bigstep lts step.
MeBi Run Saturate c1 Using bigstep lts step.
MeBi Run Minimize c1 Using bigstep lts step.
MeBi Run Bisim c1 With bigstep And c1 With lts Using step.
