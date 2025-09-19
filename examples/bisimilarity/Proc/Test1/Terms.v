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

MeBi Set ShowDebug True.
MeBi Set WeakMode True.

(* TODO: fix the plugin so the below works *)
(* MeBi Set Weak Option (label). *)
(* MeBi Set Weak Option (option label). *)
MeBi Set Weak Option (Datatypes.option label).

MeBi Saturate p Using termLTS.


(* MeBi Dump "proc_t1_p" LTS Bounded 1000 Of p Using termLTS. *)
(* MeBi Dump "proc_t1_q" LTS Bounded 1000 Of q Using termLTS. *)
(* MeBi Dump "proc_t1_r" LTS Bounded 1000 Of r Using termLTS. *)


(* MeBi 
  Dump "proc_t1_p" 
  Minim Bounded 1000 Of p Label (option label) Using termLTS.  *)


(* MeBi 
  Dump "testA" 
  Bisim LTS Bounded 50 Of p  With termLTS
    And LTS Bounded 50 Of q  With termLTS
    Using termLTS.  *)


(* MeBi Dump "testB" Minim Bounded 5 Of p Weak None Of TESTACTION Using termLTS. 
MeBi Dump "testC" Minim Bounded 5 Of q Weak None Of TESTACTION Using termLTS.  *)


(* Print can_you_find_me_test. *)

(* About build_model. *)
(* Print build_model. *)
(* Check (loader.LTS term label p termLTS). *)
