Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Import Flat.
Import Flat.Simple.

Example p : term := tfix (tseq (tpar (tact (send A) tend) 
                                     (tact (recv A) tend)) trec).

Example q : term := tfix (tseq (tpar (tact (recv A) tend) 
                                     (tact (send A) tend)) 
                               (tseq (tpar (tact (send A) tend) 
                                           (tact (recv A) tend)) trec)).

Example r : term := tseq (tpar (tact (recv A) tend) 
                               (tact (send A) tend)) 
                         (tfix (tseq (tpar (tact (send A) tend) 
                                           (tact (recv A) tend)) trec)).

MeBi Divider "Examples.Bisimilarity.Proc.Test3.Terms".
MeBi Reset All.
MeBi Set ShowAny      True.
MeBi Set ShowNotices  True.
MeBi Set ShowDebug    True.
MeBi Set ShowDetails  True.
MeBi Set ShowResults  True.
MeBi Set ShowWarnings True.
(* MeBi Set Bound 100. *)
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
MeBi Set FailIfIncomplete True.
MeBi Set FailIfNotBisim True.
(* MeBi See All.  *)
MeBi Set ShowAny False.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.Terms".

MeBi FSM p Using termLTS. MeBi Saturate p Using termLTS.
MeBi FSM q Using termLTS. MeBi Saturate q Using termLTS.
MeBi FSM r Using termLTS. MeBi Saturate r Using termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Bisim.pq".

MeBi Bisim p With termLTS And q With termLTS Using termLTS.
MeBi Bisim q With termLTS And p With termLTS Using termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Bisim.qr".

MeBi Bisim q With termLTS And r With termLTS Using termLTS.
MeBi Bisim r With termLTS And q With termLTS Using termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test3.Bisim.pr".

MeBi Bisim p With termLTS And r With termLTS Using termLTS.
MeBi Bisim r With termLTS And p With termLTS Using termLTS.


