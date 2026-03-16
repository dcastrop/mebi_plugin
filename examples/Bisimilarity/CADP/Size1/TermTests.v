Require Import MEBI.loader.

MeBi Config Output "Debug" False.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" False.
MeBi Config Output "Result" True.
MeBi Config Output "Show" False.
MeBi Config Output "DecodeResults" False.
MeBi Config Output "DumpResults" False.

Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.
Require Import MEBI.Examples.Bisimilarity.CADP.Size1.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests".
MeBi Config Weak As Option label.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.step".
MeBi Run FSM p1 Using step.
MeBi Run Saturate p1 Using step.
MeBi Run Minimize p1 Using step.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.lts".
MeBi Run FSM c1 Using lts step.
MeBi Run Saturate c1 Using lts step.
MeBi Run Minimize c1 Using lts step.
