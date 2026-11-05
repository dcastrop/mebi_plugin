(* Evaluation series, scaled up: width ([Width.v]) at n = 3, 162 states a
   side. Not built by default (not in [examples/dune]); run it with
   [bench/proofs.sh -s eval-large]. Bounds as in [Width.v]. *)
Require Import MEBI.loader.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Result" False.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Evaluation.Base.

MeBi Divider "Examples.Evaluation.WidthLarge".
MeBi Config Weak As Option act.

(* n = 3: 162 states. *)
Set MeBi MaxStates 162.
MeBi Run LTS (wl 3) Using procLTS.
Set MeBi MaxStates 161.
Fail MeBi Run LTS (wl 3) Using procLTS.
(* Above the default bound of 100 states: keep it raised for the proof. *)
Set MeBi MaxStates 162.
Set MeBi MaxSteps 54893.
Example wbis_w3 : weak_bisimilar procLTS procLTS (wl 3) (wr 3).
Proof. bisim. Qed.
MeBi Config Reset Bounds.

MeBi Config Reset Weak.
