(* Evaluation series: width. [spawn n P] runs [n + 1] copies of [P] in
   parallel, beside [Q]: [2 * 3^(n+1)] states, from interleaving alone. The
   proof is [weak_bisimilar] between [wl n] and [wr n] ([Base.v]).
   Sizes are pinned with least bounds, as in [Test.v]'s [ExtractionSizes];
   each [Solve] with the least bound that completes (under the default
   [MeBi Config Solver MutualCofix Auto]), as in
   [Bisimilarity/CCS/LawProofs.v], so any increase fails the build. *)
Require Import MEBI.loader.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Result" False.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Evaluation.Base.

MeBi Divider "Examples.Evaluation.Width".
MeBi Config Weak As Option act.

(* n = 0: 6 states. *)
Set MeBi MaxStates 6.
MeBi Run LTS (wl 0) Using procLTS.
Set MeBi MaxStates 5.
Fail MeBi Run LTS (wl 0) Using procLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 76.
Example wbis_w0 : weak_bisimilar procLTS procLTS (wl 0) (wr 0).
Proof. bisim. Qed.

(* n = 1: 18 states. *)
Set MeBi MaxStates 18.
MeBi Run LTS (wl 1) Using procLTS.
Set MeBi MaxStates 17.
Fail MeBi Run LTS (wl 1) Using procLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 973.
Example wbis_w1 : weak_bisimilar procLTS procLTS (wl 1) (wr 1).
Proof. bisim. Qed.

(* n = 2: 54 states. *)
Set MeBi MaxStates 54.
MeBi Run LTS (wl 2) Using procLTS.
Set MeBi MaxStates 53.
Fail MeBi Run LTS (wl 2) Using procLTS.
MeBi Config Reset Bounds.
Set MeBi MaxSteps 7663.
Example wbis_w2 : weak_bisimilar procLTS procLTS (wl 2) (wr 2).
Proof. bisim. Qed.

MeBi Config Reset Weak.
