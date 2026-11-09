Require Import MEBI.loader.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecMutex.

(* [MutualExclusion.v] for two processes (12,027 states): the lock server
   simulates the processes, which is what mutual exclusion needs. Decided
   on Sylvan's diagrams and proved from Sylvan's certificate of the
   decision (doc/certificate.md): the proof is built in about 3s and
   checked at [Qed] in about 2s, under 0.7GB in all. The system and the spec
   are not weakly bisimilar. *)

MeBi Divider "Examples.CADP.TwoProcesses".

Example wsim_lts_2 : weak_sim lts spec_lts c2 (make_spec 1).
Proof. weak_sim. Qed.
