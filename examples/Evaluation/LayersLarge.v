(* Evaluation series, scaled up: semantic layers ([Layers.v]), [Nested]
   only, at 16, 32 and 64 layers ([Stacked] needs one inductive a layer, written
   out by hand). Not built by default (see [_CoqProject]); run it with
   [bench/proofs.sh -s eval-large]. Bounds as in [Width.v].
   Qed grows much faster than the solver here: each doubling of the layers
   costs the kernel ~13 times as much (0.7s, 5.5s, 71s), the solver ~3-4
   times; 128 layers was stopped for that. *)
Require Import MEBI.loader.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Result" False.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Evaluation.Base.
Require Import MEBI.Examples.Evaluation.Layers.

MeBi Divider "Examples.Evaluation.LayersLarge".
MeBi Config Weak As Option act.

Module NestedLarge.
  Import Nested.

  (* 6 states at every depth. *)
  MeBi Config Bounds As Num States 6.
  MeBi Run LTS (l 64) Using layLTS procLTS.
  MeBi Config Bounds As Num States 5.
  Fail MeBi Run LTS (l 64) Using layLTS procLTS.
  MeBi Config Reset Bounds.

  Example wbis_n16 : weak_bisimilar layLTS layLTS (l 16) (r 16).
  Proof. MeBi Sim Begin layLTS (l 16) And layLTS (r 16) Using layLTS procLTS.
    MeBi Sim Solve 315. Qed.
  Example wbis_n32 : weak_bisimilar layLTS layLTS (l 32) (r 32).
  Proof. MeBi Sim Begin layLTS (l 32) And layLTS (r 32) Using layLTS procLTS.
    MeBi Sim Solve 539. Qed.
  Example wbis_n64 : weak_bisimilar layLTS layLTS (l 64) (r 64).
  Proof. MeBi Sim Begin layLTS (l 64) And layLTS (r 64) Using layLTS procLTS.
    MeBi Sim Solve 987. Qed.
End NestedLarge.

MeBi Config Reset Weak.
