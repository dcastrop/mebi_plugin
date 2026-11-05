Require Import MEBI.loader.

MeBi Config Output "Debug" False.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" True.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" False.
MeBi Config Output "Result" False.
MeBi Config Output "Show" False.
MeBi Config Output "DecodeResults" False.
MeBi Config Output "DumpResults" False.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CCS.

(* Instances of the CCS laws that CTrees' CCS case study proves by hand
   (vellvm/ctrees, examples/CCS/Denotation.v, section Theory), as comparison
   cases (note 10, A.4). The comparison is not like for like, in two ways:

   - CTrees proves each law for all [p], [q], [r]; MeBi proves one closed
     instance per command.
   - CTrees' [~] is strong bisimilarity, MeBi's [weak_bisimilar] is weak
     (and divergence-insensitive), a weaker statement on the same pair.

   The instances are chosen so the laws are not trivially syntactic: [P],
   [Q] and [R] synchronise in a cycle (P !a -> Q, Q !c -> R, R !b -> P), so
   regrouping a parallel composition moves handshakes across the brackets.

   CTrees' last law, [unfold_bang' : !p ~ !p | p], is out of scope:
   replication makes the reachable state space infinite ([!a] grows a new
   [0] at each [a]), so there is no finite LTS to extract. It is a boundary,
   not a test, like rocq-sims' codata counterexample (note 10, A.3).

   Beyond CTrees' list: Milner's tau-laws, which hold weakly but not
   strongly; an instance of the expansion law; and negative neighbours that
   must be refused ([Begin] decides first, so a [Fail] there is a decided
   "no"). *)

MeBi Divider "Examples.Bisimilarity.CCS.LawProofs".
MeBi Config Weak As Option act.

Definition P := ! a . ? b . pnil.
Definition Q := ? a . ! c . pnil.
Definition R := ? c . tau. ! b . pnil.

(* 1. Choice: CTrees' [plsC], [plsA], [pls0p], [plsp0], [plsidem]. *)
MeBi Divider "Examples.Bisimilarity.CCS.LawProofs.sum".
Set MeBi MaxSteps 35.
Example plsC : weak_bisimilar step step (sum P Q) (sum Q P).
Proof. bisim. Qed.
Set MeBi MaxSteps 57.
Example plsA : weak_bisimilar step step (sum P (sum Q R)) (sum (sum P Q) R).
Proof. bisim. Qed.
Set MeBi MaxSteps 18.
Example pls0p : weak_bisimilar step step (sum pnil P) P.
Proof. bisim. Qed.
Set MeBi MaxSteps 18.
Example plsp0 : weak_bisimilar step step (sum P pnil) P.
Proof. bisim. Qed.
Set MeBi MaxSteps 55.
Example plsidem : weak_bisimilar step step (sum (sum P Q) (sum P Q)) (sum P Q).
Proof. bisim. Qed.

(* 2. Milner 1989, ch. 7: the three tau-laws and [tau.p ~ p], which hold
   weakly only; and the expansion law (strong too), whose right side has a
   tau the left reaches by a handshake. *)
MeBi Divider "Examples.Bisimilarity.CCS.LawProofs.tau".
Set MeBi MaxSteps 41.
Example tau1 : weak_bisimilar step step (? a . tau. P) (? a . P).
Proof. bisim. Qed.
Set MeBi MaxSteps 26.
Example tau2 : weak_bisimilar step step (sum P (tau. P)) (tau. P).
Proof. bisim. Qed.
Set MeBi MaxSteps 27.
Example tau3 :
  weak_bisimilar step step (sum (? a . sum P (tau. Q)) (? a . Q)) (? a . sum P (tau. Q)).
Proof.
  bisim.
Qed.
Set MeBi MaxSteps 14.
Example tau0 : weak_bisimilar step step (tau. P) P.
Proof. bisim. Qed.
Set MeBi MaxSteps 97.
Example expansion :
  weak_bisimilar step step (par (? a . pnil) (! a . pnil))
    (sum (? a . ! a . pnil) (sum (! a . ? a . pnil) (tau. pnil))).
Proof.
  bisim.
Qed.

(* 3. Negative neighbours. Restriction's scope matters: [res a (P | Q) | R]
   hides P and Q's handshake on [a], [P | res a (Q | R)] forbids it (no one
   inside offers [!a]). Dropping the tau from the expansion law loses the
   handshake. Moving a tau into a choice ([tau.(P + Q)] vs [P + tau.Q]) is
   Milner's pair again. *)
MeBi Divider "Examples.Bisimilarity.CCS.LawProofs.negative".
Example scope_neg :
  weak_bisimilar step step (par (res (cons a nil) (par P Q)) R) (par P (res (cons a nil) (par Q R))).
Proof.
  Fail bisim.
Abort.
Example expansion_neg :
  weak_bisimilar step step (par (? a . pnil) (! a . pnil))
    (sum (? a . ! a . pnil) (! a . ? a . pnil)).
Proof.
  Fail bisim.
Abort.
Example tau_choice_neg : weak_bisimilar step step (tau. sum P Q) (sum P (tau. Q)).
Proof. Fail bisim. Abort.

(* 4. Parallel composition: CTrees' [paraC], [para0p], [parap0], [paraA].
   Last because they depend most on the solver strategy: with [MutualCofix
   False] forced they take 2218, 3127 and 2853 steps, and [paraA] was
   OOM-killed at 5GB after two minutes (ASSISTED-CHANGES.md, 2026-10-03).
   Under the default [Auto] the plugin picks mutual for all four. *)
MeBi Divider "Examples.Bisimilarity.CCS.LawProofs.par".
Set MeBi MaxSteps 277.
Example paraC : weak_bisimilar step step (par P Q) (par Q P).
Proof. bisim. Qed.
Set MeBi MaxSteps 356.
Example para0p : weak_bisimilar step step (par pnil (par P Q)) (par P Q).
Proof. bisim. Qed.
Set MeBi MaxSteps 324.
Example parap0 : weak_bisimilar step step (par (par P Q) pnil) (par P Q).
Proof. bisim. Qed.
Set MeBi MaxSteps 2835.
Example paraA : weak_bisimilar step step (par P (par Q R)) (par (par P Q) R).
Proof. bisim. Qed.

MeBi Config Reset Weak.
