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
