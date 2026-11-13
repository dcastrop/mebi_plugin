Require Import MEBI.loader.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecMutex.

(* [MutualExclusion.v] for three processes (7,935,608 states): the lock
   server simulates the processes, so their traces are mutually exclusive
   ([weak_sim_safe]). Decided on Sylvan's diagrams and proved from
   Sylvan's certificate of the decision (doc/certificate.md), with
   [MeBi Prove]: each piece of the proof a constant the kernel checks when
   it is declared, about 4.5 minutes and 2.6GB at most ([Proof. weak_sim.
   Qed.] proves it too, but needs 6.6GB). Not in the default build: run it
   with [bench/prooflog.py], which caps its memory. *)

MeBi Divider "Examples.CADP.ThreeProcesses".

MeBi Prove wsim_lts_3 : weak_sim lts spec_lts c3 (make_spec 2).
