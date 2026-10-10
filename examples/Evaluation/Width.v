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
MeBi Config Bounds As Num States 6.
MeBi Run LTS (wl 0) Using procLTS.
MeBi Config Bounds As Num States 5.
Fail MeBi Run LTS (wl 0) Using procLTS.
MeBi Config Reset Bounds.
Example wbis_w0 : weak_bisimilar procLTS procLTS (wl 0) (wr 0).
Proof. MeBi Sim Begin procLTS (wl 0) And procLTS (wr 0) Using procLTS.
  MeBi Sim Solve 76. Qed.

(* n = 1: 18 states. *)
MeBi Config Bounds As Num States 18.
MeBi Run LTS (wl 1) Using procLTS.
MeBi Config Bounds As Num States 17.
Fail MeBi Run LTS (wl 1) Using procLTS.
MeBi Config Reset Bounds.
Example wbis_w1 : weak_bisimilar procLTS procLTS (wl 1) (wr 1).
Proof. MeBi Sim Begin procLTS (wl 1) And procLTS (wr 1) Using procLTS.
  MeBi Sim Solve 1162. Qed.

(* n = 2: 54 states. *)
MeBi Config Bounds As Num States 54.
MeBi Run LTS (wl 2) Using procLTS.
MeBi Config Bounds As Num States 53.
Fail MeBi Run LTS (wl 2) Using procLTS.
MeBi Config Reset Bounds.
Example wbis_w2 : weak_bisimilar procLTS procLTS (wl 2) (wr 2).
Proof. MeBi Sim Begin procLTS (wl 2) And procLTS (wr 2) Using procLTS.
  MeBi Sim Solve 8255. Qed.

MeBi Config Reset Weak.
