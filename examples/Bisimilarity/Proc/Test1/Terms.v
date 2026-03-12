Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

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
