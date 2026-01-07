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
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
(* MeBi Config Output Debug Enable. *)
(* MeBi Config Output Info Enable. *)
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
MeBi Config Bound 1000.
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)
(* MeBi Config Output Disable. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.p1".
MeBi FSM p1 Using step.
MeBi Saturate p1 Using step.
MeBi Minimize p1 Using step.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.c1.step".
MeBi FSM c1 Using lts step.
MeBi Saturate c1 Using lts step.
MeBi Minimize c1 Using lts step.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.TermTests.c1.bigstep".
MeBi FSM c1 Using bigstep lts step.
MeBi Saturate c1 Using bigstep lts step.
MeBi Minimize c1 Using bigstep lts step.
MeBi Bisim c1 With bigstep And c1 With lts Using step.
