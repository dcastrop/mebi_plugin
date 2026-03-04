Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Layered.

(* Example s1 : term := tfix (tseq (tact (send A) tend) trec).
Example s2 : term := tfix (tseq (tact (send A) tend) 
                                (tseq (tact (send A) tend) trec)). *)
                                
Example s1 : term := tfix (tact (send A) trec).
Example s2 : term := tfix (tact (send A) (tact (send A) trec)).


(* Example r1 : term := tfix (tseq (tact (recv A) tend) trec).
Example r2 : term := tfix (tseq (tact (recv A) tend) 
                                (tseq (tact (recv A) tend) trec)). *)
                                
Example r1 : term := tfix (tact (recv A) trec).
Example r2 : term := tfix (tact (recv A) (tact (recv A) trec)).


Example p : comp := cpar (cprc s1) (cprc r1). 
Example q : comp := cpar (cprc s2) (cprc r2). 
Example r : comp := cpar (cprc s1) (cprc r2). 
Example s : comp := cpar (cprc s2) (cprc r1). 
