# Structural congruence, and proofs up to it

Detect reorderings in the semantics (associativity, commutation, swaps
and the like), take them as structural congruence, give it by hand when
detection fails, and prove up to it, from certificates the engine makes
(doc/certificate.md, doc/optimisations.md, 5), with nothing recomputed in
Rocq.

**Status (2026-11-13):** built, route B, on by default where it applies
(weak goals, an LTS with reorderings, no dynamic summand; off with
`MEBI_UPTO=off`). Two processes prove in about a second, three in 6s;
Test4's three pairs (`examples/Bisimilarity/Proc/Test4/TermTests.v`) by
`MeBi Prove`, the file in 493s, 3.3GB at most (by the tactic, one goal:
52s, 3.9GB). Not built: sorting for classes past the bound
(`MEBI_UPTO_BOUND`, 50,000 items). A proof up to reordering that cannot
be built (rules that do not commute with the reorderings) is replaced
by one without, with a notice.

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

## The certificate up to congruence: constraints

What the proof needs, and the constraint on it:
- **Classes:** the states that reach each other by the congruence's
  silent steps. For each state, the engine knows its class and a
  shortest silent path to the class's representative.
- **Without route B's commutation lemmas, every member's moves must be
  answered, not only the representative's.**
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

## Two routes for the certificate (2026-11-12); route B chosen (2026-11-13)

Write `s ≡ s'` when `s` reaches `s'` by reorderings (an equivalence:
each reordering has its inverse). Since reorderings are silent steps,
`≡` is contained in weak bisimilarity (each side reaches the other
silently).

**A (not taken). Every member checked, answers up to `≡`.** The relation
holds every state of a class on the mover's side, and one representative
per class on the answerer's side; each move of each member is answered
into a pair whose answerer state is `≡` to one in the relation. It needs
no lemma about the rules, but in a bisimilarity each side is the mover
once, so both sides hold every member: the pairs do not shrink.

**B (chosen). The semantics respects `≡`.** Prove, once per LTS and per
pair of a reordering and another rule, that when a state steps by the
rule, any reordering of it steps by a matching rule to a reordering of
the target (the rules commute with the reorderings: lemmas the plugin
generates and proves with the same terms as its case lemmas, no
computation). Then `≡` is a strong bisimulation, and weak bisimulation
up to strong bisimilarity is sound (Milner): the game runs on one
representative per class on both sides, for simulations and
bisimilarities alike. This is what makes "representatives only" sound;
without these lemmas it is not (the `b -τ-> a -x-> 0` example above:
there the lemma for `a -τ-> b` fails, `a` having an `x` step that `b`
lacks). When a pair of rules does not commute, the lemma fails and the
LTS falls back to no up-to.

## Route B: the design (2026-11-13)

**Representatives by saturation, per skeleton.** A reordering never
inspects a value, it only moves values between slots, so classes are
found once per skeleton, value-free:
1. **Saturate:** from each skeleton the game meets, fire every
   reordering at every place where it applies, to a fixpoint (a state
   seen again stops the search: no orientation of the rules is needed,
   and a reordering that is its own inverse, `l | r -> r | l`, is no
   problem). Each item found is a skeleton and a **slot map** (which
   slot's value goes where), with the **path** that reached it: the
   rules fired and where. Test4's four-leaf skeletons: one class of 120
   items (5 bracketings, 24 slot maps). Firing only the first rule that
   matches is not enough: with `s0 | (s1 | s2) -> (s0 | s1) | s2` and
   `(s1 | s2) | s0 -> s0 | (s1 | s2)` it cycles through the rotations.
2. **Choose:** a state's class is its skeleton's items applied to its
   values; its representative is the least of them in a fixed order
   (lexicographic on the vector), so that every member of a class
   chooses the same one (the first found would depend on where the
   search started: sound, but less reduction).
3. **Explore** from representatives only; every target is mapped to its
   class's representative, the goal's start states too.

**The proof:**
- per item used, a lemma: a state of skeleton `s` with any values is
  `≡` the state of skeleton `s'` with the values moved by the slot map,
  proved by the item's path (each step a reordering rule of the LTS
  applied to the values, in context): nothing per state;
- per LTS, the commutation lemmas (B above), by case analysis on the
  steps, as the case lemmas are proved;
- once, in `theories/`: weak (bi)simulation up to `≡`, with `≡` a strong
  bisimulation, implies weak (bi)similarity;
- the game on vectors as now, over representatives, with each target
  related to its representative by the item's lemma.

**What Rocq computes:** typing, and conversions of the kinds already
accepted: `val`, `tall_get`, and a slot map applied to a vector of known
length (of `projv`'s kind; to confirm when it is written).

**Larger classes: sorting.** A class has n! × Catalan(n-1) items for n
processes (120 for 4, 30,240 for 6), so saturation suits the examples'
sizes. For larger ones, the same saturated items serve a sorting
procedure: the skeleton brought to its representative by its path, then
neighbouring values compared and swapped, each swap an item's slot map
with its recorded path: about n² swaps per state instead of the whole
class. Not to be pushed far: larger systems are meant to come from
compositionality (`P <~ I` and `I || P <~ I` give `P^n <~ I`,
`doc/future-work.md`).

**Scope:** weak goals first (for a strong goal, a reordering move must
be answered by exactly one silent step, which need not exist).

## Route B: implementation plan (2026-11-13)

Stages, each built and checked before the next; the status of each is
kept here.

1. **Theory (done, 2026-11-13): `theories/DdUpto.v`.** `Cong Ro` (the
   reflexive-transitive closure of a one-step relation `Ro`) is an
   equivalence and a strong simulation (`cong_equiv`) given, for each
   `Ro` step, an inverse path and the commutation of the step with the
   LTS's steps. The game `VWSimN inv nsL nsR rs z`: every move from `z`
   answered into `z`, the answer's silent steps the answerer's or
   normalisation steps (the certificate's normalisation relations,
   logged backwards, either side's part): the answerer's relations are
   learned from representatives only, so an answer goes back to
   representatives as it goes. Its toolkit: `movewN_l`, `movewN_tau`,
   `all_back_oklN`, `all_back_reachN`, `all_back_reachN_nl`, `_nr`.
   The start: `NormZ` (normalisation steps into `z`).
   `cert_weak_sim_upto` and `cert_weak_bisim_upto`: `DdGame`'s
   completeness and soundness of the encodings as they are, plus each
   normalisation step sound (`SoundNL`, `SoundNR`: a state and one `Eq`
   to it); a real answer is carried along `Eq` at each normalisation
   (`taus_left_upto`, `taus_right_upto`). Closed under the global
   context.
2. **Engine (`lib/learn`; built and checked 2026-11-13).**
   - A summand is a *reordering summand* if its chain's innermost rule
     is one of the LTS's congruence rules (`Skeleton.congruence`); its
     target only moves slots: a shape and a slot map.
   - *Saturation*, per shape, value-free: the items (shape, slot map)
     reachable by reordering summands, each with its path; cached; a
     bound on the items (no up-to beyond it, or sorting).
   - *Representative* of a vector: the least, lexicographically, of its
     shape's items applied to its values.
   - *Normalisation relations* (one per system, every position read and
     written, label silent): a tuple `v -> rep v` per vector met that is
     not its own representative, learned as escaped projections are.
   - *Exploration* from representatives: each round's new vectors
     replaced by their representatives (the start too); reordering
     summands are kept (their targets normalise back to their source).
   - *Game*: the answerer's silent closure backwards takes the
     normalisation relations (each side) with its silent relations; the
     universe holds each side's representatives and their one-step
     targets (a move's target is not normalised); the mover's images are
     of `z`, as now. Logged as images, in the plan.
   - Applies when the LTS has congruence rules, its reachable shapes
     have no dynamic summand, the goal is weak, and the slots' types
     hold no structural constructor (a slot's value then never has
     structure); otherwise no up-to.
3. **Proof (`src/lts_proof.ml`, `src/game_proof.ml`).** Status
   (2026-11-13): the congruence is built and checked (`upto_cong`):
   `Ro` over the slots' numbers (a slot holds `T_v u`, so no set `V` is
   needed), its eliminator, inverse paths, commutation lemmas and
   `Equiv`; a rule needing a slot to start with a constructor no value
   starts with is refuted by the existing `ns_def`; a slot's own step
   keeps its premise, its target a table value by `Vn` (proved over the
   table by `val_all`). One congruence when both sides share the LTS.
   Two processes: 1 reordering; three: 7; Test4: 31, built in 18s,
   1.1GB. Then (built the same day): item lemmas (`item_lemma`, each by
   its path); normalisation soundness (`soundness_rel ~norm`: the
   relation `fun x _ y => Cong Ro x y` in the place of the LTS, each
   tuple's proof its item's lemma at its numbers; `soundNL_of`); the
   game with the normalisation lists (`game_proof.ml`, `~upto`); the
   start (`normz_l`, `normz_r`); `cert_weak_sim_upto`,
   `cert_weak_bisim_upto`. The plan below was written first; where it
   differs (a set `V` of values), this status is what is built.
   - `V`, per LTS: the slot values met, closed under the slots' own
     steps, as a disjunction of equalities; a lemma that `V` is closed
     (each value's steps by its step lemma, `as_lemma`).
   - `Ro`, per LTS, an inductive: one constructor per one-step
     reordering summand of a shape met, over the slots' values, each in
     `V`. Needed because a reordering need not commute with every rule
     for every value: Proc's `do_par_end` (`cprc tend | cprc tend ->
     cprc tend`) does not commute with associativity (the two leaves
     cease to be siblings), and is refuted because `tend` is in no
     reachable slot.
   - `Ro`'s inverse paths: each constructor's inverse summand.
   - *Commutation*, per constructor, by the small-inversion engine
     (`invert`, its `stuck`/`finish` callbacks): each case a summand
     `σ` of the source shape; a slot's own step kept as a hypothesis; the
     matching summand of the reordered shape (the same innermost rule at
     the mapped slots) applied to the same premise; the targets related
     by the same reordering (a local summand) or a saturation path; a
     reordering summand answered by the inverse step, back to the
     source; a case refuted by `V`, or else the LTS has no up-to.
   - Per item used, `Cong Ro (s v) (s' (π v))` for all values in `V`, by
     its path; per normalisation tuple, that lemma instantiated
     (`SoundNL`, `SoundNR`, as the answerer's soundness is proved, a
     lemma per relation).
   - The game by `VWSimP`'s toolkit; the start by its normalisation
     steps; the goal by `cert_weak_sim_upto` / `cert_weak_bisim_upto`.
4. **Examples (done 2026-11-13):** two processes (`do_comm` only),
   three (associativity), Test4 (`TermTests.v`, three lemmas by `MeBi
   Prove`); Proc Test3's 14 goals use it in the default build. Sorting
   for larger classes: not built. Where the facts go: Test4's 593K are
   mostly the images of the closure through the normalisation
   relations, which read and write every position (17 here, about 580
   tuples each): narrowing them to the positions a shape's slots use
   would cut them.
