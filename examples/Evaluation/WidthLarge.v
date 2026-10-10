(* Evaluation series, scaled up: width ([Width.v]) at n = 3, 162 states a
   side. Not built by default (see [_CoqProject]); run it with
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
MeBi Config Bounds As Num States 162.
MeBi Run LTS (wl 3) Using procLTS.
MeBi Config Bounds As Num States 161.
Fail MeBi Run LTS (wl 3) Using procLTS.
(* Above the default bound of 100 states: keep it raised for the proof. *)
MeBi Config Bounds As Num States 162.
Example wbis_w3 : weak_bisimilar procLTS procLTS (wl 3) (wr 3).
Proof. MeBi Sim Begin procLTS (wl 3) And procLTS (wr 3) Using procLTS.
  MeBi Sim Solve 55962. Qed.
MeBi Config Reset Bounds.

MeBi Config Reset Weak.
