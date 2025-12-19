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

MeBi Divider "Examples.Bisimilarity.Proc.Test1.Terms".
MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
MeBi Config Output Debug Enable.
MeBi Config Output Info Enable.
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
(* MeBi Config Bound 100. *)
MeBi Config WeakMode Enable.
MeBi Config Weak As Option label.
MeBi Config Fail If Incomplete True.
MeBi Config Fail If NotBisim True.
(* MeBi See All.  *)
MeBi Config Output Disable.


MeBi Divider "Examples.Bisimilarity.Proc.Test1.Terms".

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




(* MeBi Config Output Enable True. *)



(* MeBi Divider "Testing". *)

(* Example e1 : term := (tseq (tseq (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend) tend) tend). *)
(* Example e1 : term := (tseq (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend) tend). *)
(* Example e1 : term := (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) tend). *)

(* MeBi FSM e1 Using termLTS.  *)
(* MeBi Saturate e1 Using termLTS. *)


(* MeBi FSM p Using termLTS. *)
(* MeBi FSM (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) (tfix (tseq (tpar (tact (send A) tend) (tact (recv A) tend)) trec))) Using termLTS. *)
