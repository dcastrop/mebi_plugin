Require Import MEBI.loader.

MeBi Config Output "Debug" False.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" False.
MeBi Config Output "Result" False.
MeBi Config Output "Show" False.
MeBi Config Output "DecodeResults" False.
MeBi Config Output "DumpResults" False.

MeBi Config Output "Info" True.
MeBi Config Output "DecodeResults" True.
MeBi Config Output "DumpResults" True.

Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.
Require Import MEBI.Examples.Bisimilarity.CADP.Size2.Terms.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.TermTests".
MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 5000.


Require Import MEBI.Examples.Bisimilarity.CADP.Properties.MutualExclusion.
MeBi Run Minimize (make_spec 1) Using spec_lts.



MeBi Divider "Examples.Bisimilarity.CADP.Size2.Glued.TermTests.bigstep".
MeBi Run FSM c2 Using bigstep.

(* !! takes very long time (4-12 hours) *)
(* MeBi Run Saturate c2 Using bigstep. *)
(* MeBi Run Minimize c2 Using bigstep. *)


(* !! state explosion *)
(* MeBi Run Bisim c2 With bigstep And c2 With lts Using step. *)
