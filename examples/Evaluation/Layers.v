(* Evaluation series: semantic layers. The state count stays fixed while
   each derivation gets deeper, so what grows is the cost of a layer, in
   extraction (premise search) and in the proof (inversion depth):
   - [Nested]: one LTS wrapping its own steps, [wrap^n (base p)];
   - [Stacked]: one LTS per layer, each with a premise on the one below,
     as [Proc.Layered] and [Test.v]'s [ExtractionSizes] [sysLTS].
   The base is [wl 0] against [wr 0] ([Base.v], 6 states each); each
   proof is [weak_bisimilar]. Bounds as in [Width.v]. *)
Require Import MEBI.loader.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Result" False.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Evaluation.Base.

MeBi Divider "Examples.Evaluation.Layers".
MeBi Config Weak As Option act.

Module Nested.
  Inductive term : Set := base (p : proc) | wrap (t : term).

  Inductive layLTS : term -> option act -> term -> Prop :=
  | l_base p a p' : procLTS p a p' -> layLTS (base p) a (base p')
  | l_wrap t a t' : layLTS t a t' -> layLTS (wrap t) a (wrap t').

  Fixpoint wrapn (n : nat) (t : term) : term :=
    match n with O => t | S k => wrap (wrapn k t) end.

  Definition l (n : nat) := wrapn n (base (wl 0)).
  Definition r (n : nat) := wrapn n (base (wr 0)).

  (* 6 states at every depth. *)
  Set MeBi MaxStates 6.
  MeBi Run LTS (l 8) Using layLTS procLTS.
  Set MeBi MaxStates 5.
  Fail MeBi Run LTS (l 8) Using layLTS procLTS.
  MeBi Config Reset Bounds.

  Set MeBi MaxSteps 91.
  Example wbis_n0 : weak_bisimilar layLTS layLTS (l 0) (r 0).
  Proof. bisim. Qed.
  Set MeBi MaxSteps 119.
  Example wbis_n2 : weak_bisimilar layLTS layLTS (l 2) (r 2).
  Proof. bisim. Qed.
  Set MeBi MaxSteps 147.
  Example wbis_n4 : weak_bisimilar layLTS layLTS (l 4) (r 4).
  Proof. bisim. Qed.
  Set MeBi MaxSteps 203.
  Example wbis_n8 : weak_bisimilar layLTS layLTS (l 8) (r 8).
  Proof. bisim. Qed.
End Nested.

Module Stacked.
  Inductive sys1 : Set := s1 (p : proc).
  Inductive l1LTS : sys1 -> option act -> sys1 -> Prop :=
  | l1 p a p' : procLTS p a p' -> l1LTS (s1 p) a (s1 p').
  Inductive sys2 : Set := s2 (x : sys1).
  Inductive l2LTS : sys2 -> option act -> sys2 -> Prop :=
  | l2 x a x' : l1LTS x a x' -> l2LTS (s2 x) a (s2 x').
  Inductive sys3 : Set := s3 (x : sys2).
  Inductive l3LTS : sys3 -> option act -> sys3 -> Prop :=
  | l3 x a x' : l2LTS x a x' -> l3LTS (s3 x) a (s3 x').

  Set MeBi MaxSteps 90.
  Example wbis_s1 : weak_bisimilar l1LTS l1LTS (s1 (wl 0)) (s1 (wr 0)).
  Proof. bisim. Qed.
  Set MeBi MaxSteps 104.
  Example wbis_s2 :
    weak_bisimilar l2LTS l2LTS (s2 (s1 (wl 0))) (s2 (s1 (wr 0))).
  Proof. bisim. Qed.
  Set MeBi MaxSteps 118.
  Example wbis_s3 :
    weak_bisimilar l3LTS l3LTS (s3 (s2 (s1 (wl 0)))) (s3 (s2 (s1 (wr 0)))).
  Proof. bisim. Qed.
End Stacked.

MeBi Config Reset Weak.
