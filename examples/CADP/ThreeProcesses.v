Require Import MEBI.loader.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecMutex.

(* [MutualExclusion.v] for three processes (7,935,608 states): the lock
   server simulates the processes, so their traces are mutually exclusive
   ([weak_sim_safe]). Decided on Sylvan's diagrams and proved from
   Sylvan's certificate of the decision (doc/certificate.md): the proof is
   built in about 2 minutes and checked at [Qed] in about 2.5 minutes,
   6.6GB at most. Not in the default build: run it with
   [bench/prooflog.py], which caps its memory. *)

MeBi Divider "Examples.CADP.ThreeProcesses".

Example wsim_lts_3 : weak_sim lts spec_lts c3 (make_spec 2).
Proof. weak_sim. Qed.
