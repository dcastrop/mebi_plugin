Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Terms".
MeBi Reset All.
MeBi Set ShowAny      True.
MeBi Set ShowNotices  True.
(* MeBi Set ShowDebug    True. *)
(* MeBi Set ShowDetails  True. *)
MeBi Set ShowResults  True.
MeBi Set ShowWarnings True.
MeBi Set Bound 1000.
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
MeBi Set FailIfIncomplete True.
MeBi Set FailIfNotBisim True.
(* MeBi See All.  *)
(* MeBi Set ShowAny False. *)

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Terms.p1".
Example p1 : tm * env := (P, Env.initial 1).
MeBi FSM p1 Using step.
MeBi Saturate p1 Using step.
MeBi Minimize p1 Using step.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Terms.c1".
Example c1 : composition := compose (create 1 P).
MeBi FSM c1 Using lts step.
MeBi Saturate c1 Using lts step.
MeBi Minimize c1 Using lts step.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Terms.c1.semantics".
MeBi FSM c1 Using bigstep lts step.
MeBi Saturate c1 Using bigstep lts step.
MeBi Minimize c1 Using bigstep lts step.
MeBi Bisim c1 With bigstep And c1 With lts Using step.
