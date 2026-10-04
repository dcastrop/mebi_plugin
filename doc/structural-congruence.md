# Structural congruence: proving through a normalised semantics

This guide is for semantics that build **structural congruence** into their
transition relation as silent steps: rules that reorder, rebracket or tidy
up parallel components. They are a natural way to write a process calculus,
and they make the state space explode. The recipe below keeps your
semantics as it is, has the plugin prove `weak_sim` or `weak_bisimilar`
over a much smaller *normalised* semantics, and transfers the result back
with one theorem you prove by hand, once, for all terms.

The worked example is `examples/ProcCongruence.v`, applied to
`examples/Bisimilarity/Proc/Test4` in `NormProofs.v` and
`NormBisimProofs.v`. Its file and theorem names are used throughout.

## The problem

`Layered.compLTS` (`examples/Proc.v`) has three ordinary rules, `do_t`,
`do_parl` and `do_parr`, which run one component. It also has four
congruence rules, all silent:

```coq
| do_par_end : compLTS (cpar (cprc tend) (cprc tend)) None (cprc tend)
| do_comm    : forall l r,   compLTS (cpar l r) None (cpar r l)
| do_assocl  : forall x y z, compLTS (cpar x (cpar y z)) None (cpar (cpar x y) z)
| do_assocr  : forall x y z, compLTS (cpar (cpar x y) z) None (cpar x (cpar y z))
```

Every arrangement of the same components is then a state of its own, and
all of them are silently interconvertible. `Test4` runs four components
with 81 local configurations, so 81 × 120 arrangements give **9720
states**. The plugin can extract that and decide bisimilarity on demand,
but a proof has to cover every pair it visits, and that is out of reach.

Proving one representative per silent cycle inside the coinduction does not
help either. It is unsound: `theories/Test.v`'s `CircularTransfer` is a
counterexample.

## The recipe

### 1. Split off the core semantics

Write the original relation **without** its congruence rules:

```coq
Inductive core : comp -> option label -> comp -> Prop :=
| n_t    : forall t t' a, termLTS t a t' -> core (cprc t) a (cprc t')
| n_parl : forall l l' r a, core l a l' -> core (cpar l r) a (cpar l' r)
| n_parr : forall l r r' a, core r a r' -> core (cpar l r) a (cpar l r').
```

### 2. Choose an invariant and a canonical representative

Pick what a state *is*, up to congruence. Here that is the list of its
unfinished components, compared up to permutation:

```coq
Definition comps (c : comp) : list term := filter nonend (flat c).
```

Then write a function `norm` that picks **one** term per class:

```coq
Definition norm (c : comp) : comp := build (sort (comps c)).
```

It flattens the components, drops the finished ones, sorts them by a
computable total order and rebuilds them right-nested. Two requirements:

- **`norm` must compute.** The plugin evaluates it on closed terms while it
  extracts the LTS, so use structural recursion and decidable comparisons
  (`Nat.ltb`, a `lex` on codes). An opaque function or a `Prop`-valued
  order leaves the target stuck.
- **`norm` must keep the invariant**: `Permutation (comps (norm c)) (comps
  c)` (`comps_norm`). That is all the correctness proof needs from it.

### 3. Define the normalised semantics

Take a core step and normalise its target:

```coq
Inductive nLTS : comp -> option label -> comp -> Prop :=
| n_step : forall p a q, core p a q -> nLTS p a (norm q).
```

From `Test4`'s start state this has **82 states** instead of 9720.

**Pitfall: congruence must not be a premise.** It is tempting to write
`nLTS` with congruence premises, e.g. `p == p' -> core p' a q' -> q' == q
-> nLTS p a q`. Don't. The plugin decides premises by enumerating their
solutions, so it would enumerate every congruent target, and the 9720 states
come back. What shrinks the state space is the canonical representative,
not the explicit rules.

### 4. Prove the link, once

Prove that the two semantics agree on states with the same invariant:

```coq
Lemma link : forall c d, Permutation (comps c) (comps d) ->
  weak_bisimilar compLTS nLTS c d.
```

This is a coinduction (`cofix`) with one case per direction:

- **A `compLTS` step from `c`** is either a component step or a congruence
  step (`comp_cases`). A component step is matched by the same component
  stepping in `d` (`find_in` locates it, `core_at` moves it) followed by
  `norm`. A congruence step is silent and keeps the invariant, so `d`
  answers it by **not moving** (`wk_none`, `rt1n_refl`).
- **An `nLTS` step from `d`** moves exactly one component (`core_split`).
  The same component moves in `c` (`core_at`, `core_comp`).

In both cases the invariants still agree afterwards (`perm_replace`,
`comps_norm`), so the coinduction closes.

The step that makes this work is answering a congruence step by standing
still. It needs those steps to be **silent** and **invariant-preserving**;
if your calculus has a congruence rule that is visible, or that changes
behaviour, this recipe does not apply as is.

### 5. Derive the transfer theorems

Using `link` at a state and itself (`Permutation` is reflexive) and the
transitivity lemmas in `MEBI.Bisimilarity`, prove:

```coq
Theorem wsim_transfer : forall p q,
  weak_sim nLTS nLTS p q -> weak_sim compLTS compLTS p q.
Theorem wbis_transfer : forall p q,
  weak_bisimilar nLTS nLTS p q -> weak_bisimilar compLTS compLTS p q.
```

Each is a few lines: `compLTS ≈ nLTS` at `p`, the plugin's result over
`nLTS` from `p` to `q`, and `nLTS ≈ compLTS` at `q`, composed with
`weak_sim_trans` / `weak_bisimilar_trans` (plus `weak_bisimilar_sim` and
`weak_bisimilar_sym`).

### 6. Let the plugin prove it over the small semantics

```coq
MeBi Config Weak As Option label.
MeBi Config Bounds As Num States 200.

Example wsim_pq_norm : weak_sim nLTS nLTS p q.
Proof. MeBi Sim Begin nLTS p And nLTS q Using nLTS core termLTS.
       MeBi Sim Solve 48820. Qed.

Example wsim_pq : weak_sim compLTS compLTS p q.
Proof. exact (wsim_transfer p q wsim_pq_norm). Qed.
```

`Using` lists every relation a premise mentions: `nLTS` steps by `core`,
which steps by `termLTS`. `norm` needs no entry, because it is a function
and is evaluated, not explored.

## What it cost on `Test4`

| | states | solver steps | time | peak memory |
|---|---|---|---|---|
| `compLTS` directly | 9720 | out of reach | — | — |
| `weak_sim` over `nLTS` | 82 | 48,821 | ~7 min | ~3.8GB |
| `weak_bisimilar` over `nLTS` (`Answers Minimal`) | 82 | 61,161 | ~4 min | ~2.8GB |

Neither proof is built by default; run them under a memory cap
(`systemd-run --user --scope -p MemoryMax=6G -p MemorySwapMax=0 ...`).
The hand proof, `ProcCongruence.v` in full, is about 300 lines and is
written once for the calculus, not per example.

## Applying it to your own calculus

1. Separate the congruence rules from the rest (`core`).
2. Pick an invariant that captures a state up to congruence, and check that
   every congruence rule is silent and preserves it.
3. Write a computable `norm` that keeps the invariant.
4. Define `nLTS` as `core` with normalised targets. Do not use congruence
   premises.
5. Prove `link` by coinduction. The obligations are: a step of the original
   semantics is a component step or an invariant-preserving silent step; a
   core step moves one component; and that component can make the same
   move wherever it sits in a congruent term.
6. Derive `wsim_transfer` / `wbis_transfer`, then use `MeBi Sim` on
   `nLTS`.

The plugin needs no changes for any of this. If you apply the recipe to a
second calculus and find the same boilerplate recurring, tell us: a plugin
hook that applies a user-supplied `norm` and states the transfer
obligation is a possible future addition, but it has not been built.
