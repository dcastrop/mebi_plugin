Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

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
