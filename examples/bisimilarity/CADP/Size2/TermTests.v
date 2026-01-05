Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Import Protocol.

Require Import MEBI.Examples.CADP_Glued.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests".
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
(* MeBi Config Output Debug Enable. *)
(* MeBi Config Output Info Enable. *)
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
MeBi Config Bound 8000.
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)
(* MeBi Config Output Disable. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.p1".
Example p1 : tm * env := (P, Env.initial 2).
MeBi FSM p1 Using step.
MeBi Saturate p1 Using step.
MeBi Minimize p1 Using step.

(* 
MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.c1".
Example c1 : composition := compose (create 2 P).
MeBi FSM c1 Using lts step.
MeBi Saturate c1 Using lts step.
MeBi Minimize c1 Using lts step.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.TermTests.c1.semantics".
MeBi FSM c1 Using bigstep lts step.
MeBi Saturate c1 Using bigstep lts step.
MeBi Minimize c1 Using bigstep lts step.
MeBi Bisim c1 With bigstep And c1 With lts Using step. 

*)