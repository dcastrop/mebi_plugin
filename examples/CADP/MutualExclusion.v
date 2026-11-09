Require Import MEBI.loader.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Terms.
Require Import MEBI.Examples.CADP.SpecMutex.

(* Mutual exclusion: the MCS queue lock ([Model.v], one step per
   instruction) against the lock server [spec_lts] ([SpecMutex.v]), one
   process. [weak_sim lts spec_lts c (s)]: the spec simulates the
   processes, so every trace of theirs is one of the spec's, and mutual
   exclusion follows ([SpecMutex.v], [MEBI.Traces]). Only that direction
   is needed for verification; the other ([weak_sim spec_lts lts]) is not
   proved. With one process the two are also weakly bisimilar; with two or
   more they are not ([TwoProcesses.v], [ThreeProcesses.v]). *)

MeBi Divider "Examples.CADP.MutualExclusion".

Example wsim_lts_1 : weak_sim lts spec_lts c1 (make_spec 0).
Proof. weak_sim. Qed.

Example wbis_lts_1 : weak_bisimilar lts spec_lts c1 (make_spec 0).
Proof. weak_bisim. Qed.
