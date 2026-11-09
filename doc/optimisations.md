# Optimisations on the certificate route (sketches)

The plugin proves a goal from Sylvan's own computation: the engine decides
on decision diagrams, Sylvan logs each operation's result, and the plugin
loads the log as a proof term that Rocq's kernel typechecks
([`certificate.md`](certificate.md)). Each optimisation below makes a goal
smaller to decide or to certify. For each: what the engine computes, what
Sylvan logs, what Rocq is given to typecheck, and what has to be built. None
asks Rocq to recompute anything: a fact is a lemma applied to the engine's
data.

## 1. Confluence reduction (silent steps taken first)

**Idea** (Groote and van de Pol, 2000). A silent step `s -τ-> s1` is
*confluent* in a set `T` of silent steps when every other step
`s -a-> s2` can be closed: some `s3` with `s1 -a-> s3` and `s2 -τ-> s3` in
`T`. The greatest such `T` is reached from all silent steps by removing,
until none is left, a step for which this fails. A state with a step of `T`
to a state outside its own cycle of `T` steps then takes only that step,
its *priority step*. The reduced system is weakly bisimilar to the full one
and often far smaller (three-process CADP: most interleavings of local
steps disappear).

**On the route.**
1. *Engine:* compute `T` on the diagrams, not on an explicit graph: the
   failing steps of a candidate `T` are a relational expression over the
   explored relations (an image, a preimage and differences per label),
   iterated to the greatest fixpoint like the game. Then the priority
   relation `P ⊆ T` and a rank per state (the length of its chain of
   priority steps), as a diagram from states to ranks.
2. *Sylvan logs* the last iteration of that fixpoint (its check that no
   step fails), the computation of `P`, and the ranks' check (each priority
   step lowers the rank), exactly as it logs the game's last iteration.
3. *Rocq typechecks:* facts that `T` is confluent, that `P ⊆ T`, and that
   ranks decrease along `P` (lemmas of `theories/DdTree.v`'s kind); one
   theorem proved once: a system reduced by a confluent priority relation
   with decreasing ranks is weakly bisimilar to the full one; the game's
   certificate on the reduced system; composition by transitivity
   (`theories/Bisimilarity.v`).
4. *To build:* the symbolic fixpoint for `T` and its logging, the ranks,
   the theorem.

## 2. Static confluence (per pair of rule chains)

**Idea:** decide
confluence once per pair of summands (rule chains), not per state: two
summands whose steps touch disjoint positions of the state vector commute,
whatever the state.

**On the route.** The summands' codes (which positions each reads and
writes: copy, read, write) are already in the log (the metas). A lemma
proved once says that two relations with disjoint written and read
positions commute; per pair, the fact that their codes are disjoint is
read off the codes (a list comparison by lemmas, no evaluation). This gives
`T` without exploring the full system, so 1 runs on the reduced system
only.

## 3. Reduction by movers

**Idea:** Lipton's movers. A step that commutes
to the right of every other step can be taken first; for weak
bisimilarity this reduces to confluence of the silent movers.

**On the route:** as 2, with the commutation per pair of summands, then
as 1.

## 4. Compositional proofs over shared state

**Idea:** prove a system made of
components from each component's certificate, when the components meet
only through a shared part of the state.

**On the route:** one certificate per component (each its own log and its
own chunks), and one lemma, proved once, that composition preserves weak
simulation under the stated interface; the interface's side conditions as
facts from the summands' codes.

## 5. Proofs up to structural congruence

**Idea** ([`upto.md`](upto.md)): states equal up to reordering
(commutation, associativity, a swap) are one state for the game. The engine
detects the reorderings in the rules (`Skeleton.reorderings`; `Print MeBi
Skeleton`; `MeBi Congruence` names them by hand).

**On the route.** The engine's classes as a relation diagram (each state to
its representative), the game played on representatives. Sylvan logs the
class relation's construction (images by the reordering relations to a
fixpoint) and the game on classes. Rocq typechecks: facts that every
member's reordering step stays in its class, each member's silent path to
its representative (the reorderings are silent steps of the LTS), and the
game on classes; one theorem, proved once: a weak simulation up to a
congruence made of silent reversible steps is contained in weak
similarity.

## 6. The state encoding: shapes, slots, learned projections

**Idea** ([`skeleton.md`](skeleton.md), [`learn.md`](learn.md)): a state is
a vector, a shape and a value per slot; each rule learns which positions it
reads and writes, so a relation is a small diagram over a few positions.

**On the route.** The theorem relates the LTS's states to vectors: the
encoding as a Gallina function from the engine's tables (shapes, values),
each relation's tuples shown to be the LTS's steps (each step's derivation
from the engine's rule interpreter, and the absence of other steps from
inversion lemmas), and the tables' facts given as terms
([`certificate.md`](certificate.md), parts B to D).

## 7. Large certificates in pieces

A certificate's facts are declared a chunk at a time, each chunk a constant
the kernel checks when it is declared, the largest first, with a tight GC
(`MeBi Check Log`; three-process CADP, 668,460 facts). The proved theorem
uses the same: its top level refers to the chunks' constants, so no `Qed`
holds the whole certificate.
