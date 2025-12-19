Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Import Layered.

Example a1 : term := tfix (tseq (tact (send A) tend) trec).
Example a2 : term := tfix (tseq (tact (recv A) tend) trec).
Example b1 : term := tfix (tseq (tact (send B) tend) trec).
Example b2 : term := tfix (tseq (tact (recv B) tend) trec).

Example p : comp := cpar (cpar (cprc a1) (cprc a2)) (cpar (cprc b1) (cprc b2)). 
Example q : comp := cpar (cpar (cprc b2) (cprc a1)) (cpar (cprc a2) (cprc b1)). 
Example r : comp := cpar (cpar (cpar (cprc a2) (cprc b1)) (cprc a1)) (cprc b2). 

MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms".
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
MeBi Config Output Debug Enable.
MeBi Config Output Info Enable.
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
MeBi Config Bound 1000.
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)
(* MeBi Config Output Disable. *)


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms".

MeBi FSM p Using compLTS termLTS. 
MeBi Saturate p Using compLTS termLTS.

MeBi FSM q Using compLTS termLTS. 
MeBi Saturate q Using compLTS termLTS.

MeBi FSM r Using compLTS termLTS. 
MeBi Saturate r Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Bisim.pq".

MeBi Bisim p With compLTS And q With compLTS Using compLTS termLTS.
MeBi Bisim q With compLTS And p With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Bisim.qr".

MeBi Bisim q With compLTS And r With compLTS Using compLTS termLTS.
MeBi Bisim r With compLTS And q With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Bisim.pr".

MeBi Bisim p With compLTS And r With compLTS Using compLTS termLTS.
MeBi Bisim r With compLTS And p With compLTS Using compLTS termLTS.


