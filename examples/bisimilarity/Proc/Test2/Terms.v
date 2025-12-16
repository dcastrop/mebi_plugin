Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Import Flat.
Import Flat.Complex.

Example p : term := tpar (tfix (tact (send A) (tact (send B) trec))) 
                         (tfix (tact (recv A) (tact (recv B) trec))). 

Example q : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                         (tact (recv A) (tfix (tact (recv B) (tact (recv A) trec)))). 

Example r : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                         (tfix (tact (recv A) (tact (recv B) trec))). 
                  
MeBi Divider "Examples.Bisimilarity.Proc.Test2.Terms".
MeBi Reset All.
(* MeBi Set ShowAny      True. *)
(* MeBi Set ShowNotices  True. *)
(* MeBi Set ShowDebug    True. *)
(* MeBi Set ShowDetails  True. *)
(* MeBi Set ShowResults  True. *)
(* MeBi Set ShowWarnings True. *)
(* MeBi Set Bound 100. *)
MeBi Set WeakMode     True.
MeBi Set Weak Option label.
MeBi Set FailIfIncomplete True.
MeBi Set FailIfNotBisim True.
(* MeBi See All.  *)
MeBi Set ShowAny False.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.Terms".

MeBi FSM p Using termLTS. MeBi Saturate p Using termLTS.
MeBi FSM q Using termLTS. MeBi Saturate q Using termLTS.
MeBi FSM r Using termLTS. MeBi Saturate r Using termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.Bisim.pq".

MeBi Bisim p With termLTS And q With termLTS Using termLTS.
MeBi Bisim q With termLTS And p With termLTS Using termLTS.

MeBi Divider "Examples.Bisimilarity.Proc.Test2.Bisim.qr".

MeBi Bisim q With termLTS And r With termLTS Using termLTS.
MeBi Bisim r With termLTS And q With termLTS Using termLTS.


MeBi Divider "Examples.Bisimilarity.Proc.Test2.Bisim.pr".

MeBi Bisim p With termLTS And r With termLTS Using termLTS.
MeBi Bisim r With termLTS And p With termLTS Using termLTS.

