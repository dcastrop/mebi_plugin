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
