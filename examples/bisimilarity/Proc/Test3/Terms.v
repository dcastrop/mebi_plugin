Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Import Layered.

Example s1 : term := tfix (tseq (tact (send A) tend) trec).
Example s2 : term := tfix (tseq (tact (send A) tend) 
                                (tseq (tact (send A) tend) trec)).

Example r1 : term := tfix (tseq (tact (recv A) tend) trec).
Example r2 : term := tfix (tseq (tact (recv A) tend) 
                                (tseq (tact (recv A) tend) trec)).

Example p : comp := cpar (cprc s1) (cprc r1). 
Example q : comp := cpar (cprc s2) (cprc r2). 
Example r : comp := cpar (cprc s1) (cprc r2). 
Example s : comp := cpar (cprc s2) (cprc r1). 

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

MeBi FSM p Using compLTS termLTS. MeBi Saturate p Using compLTS termLTS.

MeBi FSM q Using compLTS termLTS. MeBi Saturate q Using compLTS termLTS.

MeBi FSM r Using compLTS termLTS. MeBi Saturate r Using compLTS termLTS.

MeBi FSM s Using compLTS termLTS. MeBi Saturate s Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.pq".
MeBi Bisim p With compLTS And q With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.qp".
MeBi Bisim q With compLTS And p With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.qr".
MeBi Bisim q With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.rq".
MeBi Bisim r With compLTS And q With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.pr".
MeBi Bisim p With compLTS And r With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.rp".
MeBi Bisim r With compLTS And p With compLTS Using compLTS termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.rs".
MeBi Bisim r With compLTS And s With compLTS Using compLTS termLTS.
MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms.Bisim.sr".
MeBi Bisim s With compLTS And r With compLTS Using compLTS termLTS.
