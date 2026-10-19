(* A small calculus for the evaluation series ([Width.v], [Layers.v]): each
   series varies one lever while the others stay fixed, so its cost can be
   measured on its own (note 12; [bench/]). Action prefix and parallel
   composition by interleaving, no communication; labels are [option act],
   as [weak_sim] and [weak_bisimilar] need, with no silent step.

   The size of an instance is an argument of a *term* builder ([spawn],
   [wrap]), never of the LTS: MeBi cannot take an LTS with parameters
   (Test.v, [ParameterisedLTS]). *)

Inductive act : Set := A | B.

Inductive proc : Set :=
| pnil
| pact (a : act) (p : proc)
| ppar (p q : proc).

Inductive procLTS : proc -> option act -> proc -> Prop :=
| p_act a p : procLTS (pact a p) (Some a) p
| p_parl p p' q a : procLTS p a p' -> procLTS (ppar p q) a (ppar p' q)
| p_parr p q q' a : procLTS q a q' -> procLTS (ppar p q) a (ppar p q').

(* The component every series uses: three states. *)
Definition P : proc := pact A (pact B pnil).

(* A second component, so the two sides of a proof always differ: two
   identical states are closed in one step (the solver's reflexive pairs),
   which would measure nothing. *)
Definition Q : proc := pact B pnil.

(* [spawn n p] is [n + 1] copies of [p] in parallel. *)
Fixpoint spawn (n : nat) (p : proc) : proc :=
  match n with O => p | S k => ppar p (spawn k p) end.

(* The pair every proof uses: [Q] beside [spawn n P], on either side.
   Bisimilar, not equal: [2 * 3^(n+1)] states each. *)
Definition wl (n : nat) : proc := ppar (spawn n P) Q.
Definition wr (n : nat) : proc := ppar Q (spawn n P).
