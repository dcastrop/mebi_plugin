Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.
Require Import MEBI.Examples.bisimilarity.CADP.Size2.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests".
MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.p1".
MeBi Config Bounds As Num States 100.
MeBi Run FSM p1 Using step.
MeBi Run Saturate p1 Using step.
MeBi Run Minimize p1 Using step.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.c1.step".
MeBi Config Bounds As Num States 5000.
MeBi Run FSM c1 Using lts step.
MeBi Run Saturate c1 Using lts step.
MeBi Run Minimize c1 Using lts step.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.c1.bigstep".
MeBi Run FSM c1 Using bigstep lts step.
MeBi Run Saturate c1 Using bigstep lts step.
MeBi Run Minimize c1 Using bigstep lts step.
MeBi Run Bisim c1 With bigstep And c1 With lts Using step.
