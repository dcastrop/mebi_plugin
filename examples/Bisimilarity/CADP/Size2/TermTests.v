Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.
Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests".
MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.c2.step".
MeBi Config Bounds As Num States 5000.
MeBi Run FSM c2 Using lts step.
MeBi Run Saturate c2 Using lts step.
MeBi Run Minimize c2 Using lts step.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.c2.bigstep".
MeBi Run FSM c2 Using bigstep lts step.
MeBi Run Saturate c2 Using bigstep lts step.
MeBi Run Minimize c2 Using bigstep lts step.
MeBi Run Bisim c2 With bigstep And c2 With lts Using step.
