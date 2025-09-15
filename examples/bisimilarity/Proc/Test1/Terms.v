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

MeBi Show (* Dump "proc_t1_p" *) LTS Bounded 1000 Of p Using termLTS.
(* MeBi Dump "proc_t1_q" LTS Bounded 1000 Of q Using termLTS. *)
(* MeBi Dump "proc_t1_r" LTS Bounded 1000 Of r Using termLTS. *)
