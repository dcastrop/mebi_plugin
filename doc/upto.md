# Structural congruence, and proofs up to it

Detect reorderings in the semantics (associativity, commutation, swaps
and the like), take them as structural congruence, give it by hand when
detection fails, and prove up to it, from certificates the engine makes
(doc/certificate.md, doc/optimisations.md, 5), with nothing recomputed in
Rocq.

**Status:** detection is built; proofs up to congruence are designed
below.

## The problem

Proc's `compLTS` writes structural congruence as silent rules:
`do_comm` (`l | r -> r | l`), `do_assocl`, `do_assocr`. Four processes
then give 120 bracketings and orders of the same 3^4 local
configurations: `Test4`'s 9,720 states. Every state of a class is
weakly bisimilar to every other state of the class, since each reaches
the others silently. A certificate over states therefore pays 120 times
for each class.

## Detection (`lib/skeleton/skeleton.ml`, `Skeleton.reorderings`)

A rule is a **reordering** if:
- it has no premise and its label is `None`;
- its source is a structure (not a variable) over pattern variables,
  each used once;
- its target uses the same variables, each once, and differs from the
  source.

Such a rule permutes positions under constructors: a container morphism
that only renumbers positions (`doc/skeleton.md`, "Terms as
containers"). Commutation, associativity, a swap inside a structure and
a rotation all qualify. A reordering is kept only if its inverse (up to
the names of the variables) is a reordering too, so that the kept ones
are reversible: `do_comm` is its own inverse, `do_assocl` and
`do_assocr` are each other's. `do_par_end` (`0 | 0 -> 0`) is not a
reordering: it drops a value. `Print MeBi Skeleton` lists them:

```
reorderings (structural congruence): do_comm, do_assocl, do_assocr
```

**By hand** (`MeBi Congruence <lts> <rule> ...`, `src/g_mebi.mlg`): the
silent rules to take as the congruence, when detection finds none or
the wrong ones (a rotation without its inverse, a reordering behind a
premise). Each name is checked to be a constructor of the LTS.
`test/rocq/CongruenceTest.v` pins both: found, a swap, a rotation given
by hand, and a name refused.

## The certificate up to congruence: to design

What the proof needs, and the constraint on it:
- **Classes:** the states that reach each other by the congruence's
  silent steps. For each state, the engine knows its class and a
  shortest silent path to the class's representative.
- **Every member's moves are answered, not only the representative's.**
  Checking the representatives alone is unsound. Take `b -τ-> a -x-> 0`
  and `a -τ-> b`: if only `a`'s moves are checked, the class `{a, b}`
  "matches" an inert state, because `a`'s one move is silent and is
  answered by staying in the class. With every member checked, `b`'s `x`
  must be answered. This is weak bisimulation up to a relation contained
  in mutual silent reachability, with the whole class's moves checked
  (sound where "up to ≈" is not; Pous and Sangiorgi).
- **Weak goals only:** going silently to the representative is not a
  one-step answer.
- **Rule 1:** the engine (Sylvan) produces the certificate: the pairs of
  classes, each member's path to its representative, and each move's
  answer. The plugin loads it as a proof term with one lemma application
  per step, as for any certificate (`lib/certify/dd_cert.ml`,
  `dd_read.ml`). Rocq neither explores the systems nor computes the
  classes, nor replays the certificate.
