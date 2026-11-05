(* Evaluation series, scaled up: semantic layers ([Layers.v]), [Nested]
   only, at 16, 32 and 64 layers ([Stacked] needs one inductive a layer, written
   out by hand). Not built by default (not in [examples/dune]); run it with
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
  Set MeBi MaxStates 6.
  MeBi Run LTS (l 64) Using layLTS procLTS.
  Set MeBi MaxStates 5.
  Fail MeBi Run LTS (l 64) Using layLTS procLTS.
  MeBi Config Reset Bounds.

  Set MeBi MaxSteps 315.
  Example wbis_n16 : weak_bisimilar layLTS layLTS (l 16) (r 16).
  Proof. bisim. Qed.
  Set MeBi MaxSteps 539.
  Example wbis_n32 : weak_bisimilar layLTS layLTS (l 32) (r 32).
  Proof. bisim. Qed.
  Set MeBi MaxSteps 987.
  Example wbis_n64 : weak_bisimilar layLTS layLTS (l 64) (r 64).
  Proof. bisim. Qed.
End NestedLarge.

MeBi Config Reset Weak.
