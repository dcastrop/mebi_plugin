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
Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests".
MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 5000.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.bigstep".
MeBi Run FSM c2 Using bigstep lts step.
MeBi Run Saturate c2 Using bigstep lts step.
MeBi Run Minimize c2 Using bigstep lts step.
MeBi Run Bisim c2 With bigstep And c2 With lts Using step.
