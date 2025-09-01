Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import NoBranching.


(*************************************)
(** Basic: No recursion or branches **)
(*************************************)

Example proc0_send0 := tact ASend tend. (* empty lts *)
(* MeBi Show LTS Bounded 150 Of proc0_send0 Using termLTS. *)
(* MeBi Dump "proc0_send0" LTS Bounded 150 Of proc0_send0 Using termLTS. *)


Example proc0_send1a := tpar (tact ASend tend)
                             (tact ARecv tend).
(* MeBi Show LTS Bounded 150 Of proc0_send1a Using termLTS. *)
(* MeBi Dump "proc0_send1a" LTS Bounded 150 Of proc0_send1a Using termLTS. *)

Goal termLTS proc0_send1a true (tpar tend tend).
  unfold proc0_send1a. eapply do_senda. Qed.

Goal transitive_closure proc0_send1a.
  unfold proc0_send1a.
  eapply trans_step. eapply do_senda.
  constructor.
Qed.


Example proc0_send1b := tpar (tact ARecv tend)
                             (tact ASend tend).
(* MeBi Show LTS Bounded 150 Of proc0_send1a Using termLTS. *)
(* MeBi Dump "proc0_send1b" LTS Bounded 150 Of proc0_send1b Using termLTS. *)


Example proc0_send2 := tpar (tact ASend (tact ARecv tend))
                            (tact ARecv (tact ASend tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send2 Using termLTS. *)
(* MeBi Dump "proc0_send2" LTS Bounded 150 Of proc0_send2 Using termLTS. *)

(* Goal transitive_closure proc0_send2.
  unfold proc0_send2.
  eapply trans_step. apply do_senda.
  eapply trans_step. apply do_comm.
  eapply trans_step. apply do_senda.
  constructor.
Qed. *)


Example proc0_send3 := tpar (tact ASend (tact BSend tend))
                            (tact ARecv (tact BRecv tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send3 Using termLTS. *)
(* MeBi Dump "proc0_send3" LTS Bounded 150 Of proc0_send3 Using termLTS. *)


Example proc0_send4 := tpar (tact BSend (tact ARecv tend))
                            (tact BRecv (tact ASend tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send4 Using termLTS. *)
(* MeBi Dump "proc0_send4" LTS Bounded 150 Of proc0_send4 Using termLTS. *)



(*************************************)
(** Recursion ************************)
(*************************************)

Example proc1_rec1 := tfix trec.
(* MeBi Show LTS Bounded 150 Of proc1_rec1 Using termLTS. *)
(* MeBi Dump "proc1_rec1" LTS Bounded 150 Of proc1_rec1 Using termLTS. *)


Example proc1_rec2 := tpar (tfix (tact ASend trec))
                           (tfix (tact ARecv trec)).
(* MeBi Show LTS Bounded 150 Of proc1_rec2 Using termLTS. *)
(* MeBi Dump "proc1_rec2" LTS Bounded 150 Of proc1_rec2 Using termLTS. *)


Example proc1_rec3 := tpar (tfix (tact ASend (tact BSend trec)))
                           (tfix (tact ARecv (tact BRecv trec))).
(* MeBi Show LTS Bounded 150 Of proc1_rec3 Using termLTS. *)
(* MeBi Dump "proc1_rec3" LTS Bounded 150 Of proc1_rec3 Using termLTS. *)


Example proc1_rec4 := tpar (tact ASend (tfix (tact BSend trec)))
                           (tact ARecv (tfix (tact BRecv trec))).
(* MeBi Show LTS Bounded 150 Of proc1_rec4 Using termLTS. *)
(* MeBi Dump "proc1_rec4" LTS Bounded 150 Of proc1_rec4 Using termLTS. *)


Example proc1_rec5 := tpar (tfix (tact ASend (tact ASend trec)))
                           (tfix (tact ARecv trec)).
(* MeBi Show LTS Bounded 150 Of proc1_rec5 Using termLTS. *)
(* MeBi Dump "proc1_rec5" LTS Bounded 150 Of proc1_rec5 Using termLTS. *)


Example proc1_rec6 := tpar (tact ASend (tact BSend (tfix (tact ASend trec))))
                           (tpar (tfix (tact ARecv trec))
                                 (tact BSend tend)).
(* MeBi Show LTS Bounded 150 Of proc1_rec6 Using termLTS. *)
(* MeBi Dump "proc1_rec6" LTS Bounded 150 Of proc1_rec6 Using termLTS. *)


(* FIXME: grows forever by accumulating [tend]. (need structural equiv.) *)
Example proc1_rec7 := tfix (tpar (tact ASend (tact BSend trec))
                                 (tact ARecv (tact BRecv tend))).
(* MeBi Show LTS Bounded 150 Of proc1_rec7 Using termLTS. *)
(* MeBi Dump "proc1_rec7" LTS Bounded 150 Of proc1_rec7 Using termLTS. *)


(*************************************)
(** Basic: No recursion or branches **)
(*************************************)

Example comp1_rec1 := cpar (cterm proc1_rec1) (cterm proc1_rec1).
(* MeBi Show LTS Bounded 150 Of comp0_send1 Using termLTS compLTS. *)
(* MeBi Dump "comp1_rec1" LTS Bounded 350 Of comp1_rec1 Using termLTS compLTS. *)

(*************************************)
(** Recursion ************************)
(*************************************)

Example comp0_send1 := cpar (cterm proc0_send1a) (cterm proc0_send1b).
(* MeBi Show LTS Bounded 150 Of comp1_rec1 Using termLTS compLTS. *)
(* MeBi Dump "comp0_send1" LTS Bounded 350 Of comp0_send1 Using termLTS compLTS. *)

