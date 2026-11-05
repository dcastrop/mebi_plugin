# Symbolic states and BDDs: a plan

A plan, written 2026-11-05 at @dcastrop's request. Nothing in it is
implemented.

**This is a new capability**, in the sense of `CLAUDE.md`'s working
assumption: most phases let the plugin do things it cannot do today. Each
phase says which kind it is. @dcastrop asked for the plan, which is the
agreement `CLAUDE.md` asks for before writing such code. Each capability
phase is still confirmed with him before it starts.

It supersedes the "symbolic extension" section of
`BITBOARD_LIKE_OPTIMISATION.md`. That plan's Phase A becomes this plan's
Phase S1, the first engine.

## Why

The plugin is explicit-state today, and the bottleneck is how many states
there are.
- **Extraction** lists every reachable state.
- **Saturation** lists every weak step.
- **The proof solver** proves one pair of states per step.

What that costs, measured:
- **Test4:** 9,720 states and 74.6M weak actions; its direct proof would
  need ~525k steps. Proved only by hand-normalising the semantics.
- **CADP:** two processes glued ≈ layered is out of memory at 24GiB.
- **Bigger systems:** three processes need a 31GB host.
- **ABP:** above 6GB to check one proof (`doc/measurements/bitboard-phase0.md`).

**The questions users want answered are beyond explicit states.**
- **Open terms and parameterised LTSs** are refused today
  (`doc/limitations.md`).
- **Compositional questions** are out of reach too. Example: *"for any pair
  of states satisfying these properties, are they bisimilar?"* The
  properties describe infinitely many, or very many, initial states, so
  they cannot be enumerated.

Symbolic states answer both. A set of states is a formula, not a list, so
its cost follows its *structure*, not its *size*.

## The idea in one paragraph

1. **Linearise the system.** Translate the Rocq step relation into a list
   of *rules*: each says when it applies (a guard over the state), what
   label it emits, and how the state changes (an update).
2. **Split each state's variables in two:**
   - **finite parameters** (control locations, booleans, enumerations,
     bounded counters) are encoded in bits, and sets and relations over
     them are BDDs;
   - **data parameters** (`nat`, lists, queues, open terms) stay as
     variables under constraints.
3. **Write each algorithm once, as fixpoints over sets and relations:**
   saturation, bisimilarity, simulation.
4. **Make the result checkable by Rocq:** instead of one proof step per
   pair of states, export the bisimulation *relation* as a Rocq predicate,
   and prove "this relation is a weak bisimulation" with one obligation per
   rule.

## Background: what exists already

- **Linear process equations (mCRL2):** a system as a vector of parameters
  and a list of guarded summands, which is exactly this plan's rule IR. mCRL2's
  symbolic tools explore it with decision diagrams (Sylvan's list decision
  diagrams).
- **Symbolic bisimulation over BDDs** (Bouali and de Simone, 1992): the
  greatest fixpoint of the bisimulation condition, computed on BDDs.
- **Symbolic bisimulation over data** (Hennessy and Lin, 1995): the
  relation is conditioned on boolean constraints over data variables.
- **BDDs for control, numerical domains for data** (BddApron): the two-tier
  split this plan adopts. It is the main user of `mlcuddidl`, the OCaml
  binding to CUDD.
- **Bisimulation up-to techniques** (Pous): they shrink the relation a
  proof has to exhibit; relevant to Phase S4.

## Architecture

```
Rocq step relation (inductive, M -> option A -> M -> Prop)
        |  S2: linearisation
        v
Rule IR: state schema + rules (guard, label, update, premises, tag)
        |  S2: encoding (variable order)
        v
Engine interface: sets, relations, image/preimage, closure, fixpoints
   |-- S1: explicit engine (Zarith bit-rows)      <- today's model, compacted
   |-- S3: BDD engine (CUDD, or a native OCaml BDD)
   `-- S6: constraint engine (data, via SMT or Rocq-decidable theories)
        |
        v
Algorithms, written once: saturation, bisimilarity (gfp), simulation,
property-initialised queries
        |
        v
Certificates: S4 relation + per-rule obligations  |  today's per-pair solver
```

### The state schema

- **Shape:** a state is a record of typed fields, not a term.
- **For parallel systems** (`Proc`, CCS, CADP), the schema is:
  - the *static structure*, meaning the parallel and restriction skeleton,
    normalised so that it does not change;
  - one field per sequential component, holding which of its finitely many
    derivatives it is in.
- **Structural congruence** (Test4's 120 tree shapes) is normalised away
  here. It does not get encoded. `Test4/NormProofs.v` does this by hand
  today.
- **Field kinds:**
  - finite (with its domain size, hence its bits);
  - data (with a theory: `nat` with linear arithmetic, lists, ...).
- **Variable order** is decided once, and recorded with the schema, so
  that results are reproducible:
  - current and next state interleaved;
  - both sides of a pair interleaved;
  - communicating components adjacent.

  CUDD's dynamic reordering stays off by default: it would make results
  depend on the order things happened to be computed in.

### Rules

Each constructor of the step relation becomes one or more rules:
- **guard(x):** when the rule applies;
- **label:** what it emits;
- **update(x, x'):** how the state changes;
- **premises:** the remaining conditions. Today's `Premise_search` decides
  them per state; symbolically they become part of the guard where
  decidable;
- **tag:** a few bits recording which constructor and which premise
  choices fired, from which a derivation tree is rebuilt for a proof.

The transition relation of label `a`, `T_a(x, x')`, is the disjunction of
its rules, kept *partitioned*: one part per rule, or per component, never
conjoined into one big BDD.

### Algorithms as fixpoints

| | symbolic form |
|---|---|
| post-image of a set `S` | `∃x. S(x) ∧ T_a(x, x')` |
| silent closure τ\* | least fixpoint of `Id ∨ (τ ∘ Z)`, by iterative squaring; per silent SCC where that is cheaper |
| weak step `=a=>` | `τ* ∘ T_a ∘ τ*` |
| reachable pairs from `I(x, y)` | least fixpoint of image over pairs |
| weak bisimilarity `R(x, y)` | greatest fixpoint, restricted to the reachable pairs: every move of `x` is answered by a weak move of `y` landing in `R`, and the other way round |
| weak simulation | the same fixpoint, one direction only |

Two guards belong in every fixpoint:
- **validity:** a bit pattern that encodes no real state is excluded
  explicitly;
- **reachability:** the fixpoint is computed on the region reachable from
  the query, not on every encodable state.

### Property-initialised queries (compositional)

The target query is: *for all `x`, `y` with `φ(x)` and `ψ(y)`, is `x`
weakly bisimilar to `y`?* It is answered in four steps:
1. **Initial set:** `I(x, y) = φ(x) ∧ ψ(y)`, built from the properties.
2. **Explore forward** from `I`, symbolically.
3. **Compute `R`** on the reachable region.
4. **Answer:**
   - `I ⊆ R` means yes, for every such pair;
   - otherwise the plugin reports the counterexample set `I \ R`, and the
     strongest condition `I ∧ R` under which the pairs are bisimilar.

**What the properties may be:**
- **Finite parameters:** a property is any boolean predicate, translated
  to a BDD.
- **Data:** a property is a constraint in a supported theory. Expanding a
  symbolic state from its constraints is *narrowing*: unify it with each
  rule, and branch with the rule's guard as a new constraint.
- **Termination:** subsumption, where one symbolic state covers another,
  and widening where subsumption alone does not end the exploration.
- **On the Rocq side:** properties are predicates that the plugin can
  translate: decidable boolean functions (`is_true (f x)`), or a fixed
  syntax of comparisons and connectives. A property outside that fragment
  is refused with a message, never assumed.

### Certificates: proving with a relation, not per pair

Today a proof is one `cofix` whose size grows with the number of pairs.
The symbolic route proves an invariant instead.
1. **Export `R`** (and the property `I`) as a Rocq predicate over the
   state type. For finite fields, the BDD becomes a boolean function; for
   data, a formula.
2. **Export an answer strategy** for each rule's case: the weak path
   the other side takes, as a function of `(x, y)`. Each `T_a` case of
   `x` needs a witness for `y`, and a set alone does not give one. This
   is where the tags come in.
3. **One generic lemma** in `theories/`, proved once: if `R` is closed
   under the bisimulation condition, then `R ⊆ weak_bisimilar`. One
   `cofix`, independent of the system.
4. **Per-system obligations:** one per (rule, label) case, each "if
   `R x y` and `x` steps by this rule, the strategy's answer is a weak
   step of `y` that lands in `R`". They are discharged by:
   - reflection (`vm_compute` on the boolean function), for finite fields;
   - `lia`, or the data theory's decision procedure, for data;
   - a fallback to today's per-pair solver on cases neither closes.
5. **Close the query:** `φ x → ψ y → I x y` gives the result for all such
   pairs.

The proof's size then depends on the number of rules and the size of `R`'s
formula, not on the number of states.
- **This is decision 3, done structurally.** It splits one proof into
  separately checked lemmas without needing mutual dependencies between
  lemmas, because the coinduction lives in one generic lemma.
- **The risk is in step 1:** exporting a BDD of 10⁵–10⁶ nodes as a
  Rocq term must stay checkable. Reflection over a verified BDD
  representation is the fallback. Phase S4 measures this first.

## Phases

Every phase keeps `CLAUDE.md`'s checks for code that touches the model or
the solver: `dune build`, `test/tests.exe`, `make`, `Test.v`'s counts, and
the proof suite under `Auto`, `True` and `False` with `bench/compare.sh`.
Explicit and symbolic engines must agree on every verdict in the suite and
in the fixtures (`bench/ocaml/fixtures/`) before a symbolic result is
trusted anywhere.

### S0 — prototype, outside the plugin (Tooling)

- **What:** a standalone OCaml binary, not linked into the plugin.
  - **Inputs:** normalised Test4, ABP, and CADP glued `Size2`, each with a
    hand-written schema and rules.
  - **Computes:** reachable states, τ\*, weak steps, and the bisimulation
    fixpoint, with `mlcuddidl`.
  - **Measures:** BDD node counts, time and memory, and the effect of
    variable order.
- **Also decides the library:**
  - `mlcuddidl` on OCaml 5.4 (does it build, and are its finalisers
    safe?);
  - against a small hash-consed BDD written in OCaml: deterministic, no C,
    a few hundred lines.
- **Done when:** we know whether the encodings compress, by how much, and
  which BDD library to use. If they do not compress, stop here and revise.

### S1 — the engine interface, and the explicit engine (Refactor + Optimization)

- **What:** define the engine interface: sets, relations, image,
  preimage, closure and fixpoints. Implement it on Zarith bit-rows. This
  is `BITBOARD_LIKE_OPTIMISATION.md`'s Phase A, moved behind the interface:
  - whole saturation by rows;
  - witnesses on demand;
  - saturation and the bisimilarity check ported onto the interface.
- **Done when:** step counts are identical, and Test4's whole saturation
  fits under 6G.
- **Waits on** @dcastrop's decision on `Bounds Saturation`, bytes or weak
  actions (`BITBOARD_LIKE_OPTIMISATION.md`, end).

### S2 — linearisation: Rocq step relation to rule IR (Refactor, new infrastructure)

- **What:** derive the schema and rules from the Rocq LTS. It reuses what
  extraction already knows: constructor bindings (`constructor_bindings.ml`)
  and premise search (`premise_search.ml`).
- **First class covered:** SOS-style relations over finite syntax,
  meaning CCS and `Proc`, whose sequential components have finitely many
  derivatives.
- **Normalisation:** structural congruence, either built in or taken from
  a user-declared congruence, as `ProcCongruence.v` provides today.
- **Check:** the IR expanded explicitly gives the same FSM as today's
  extraction on every fixture and suite example. That is the golden test.
- **Kind:** no new user-facing capability yet. It is a second route to
  the same FSMs.

### S3 — BDD engine, deciding only (New feature)

- **What:** the BDD engine behind the interface. `Run Bisim` and
  `Sim Begin`'s check can run symbolically, chosen by a config option in
  the style of `Saturation OnDemand`.
- **Unlocks:** deciding systems whose explicit state space does not fit:
  - Test4 under `compLTS` directly;
  - CADP glued ≈ layered at two and three processes;
  - larger instances of the evaluation levers.
- **Done when:** verdicts agree with the explicit engine wherever both
  finish, and the large cases are measured.

### S4 — certificates (New feature)

- **What:**
  - the generic lemma in `theories/`;
  - the export of `R` and of the answer strategies;
  - the per-rule obligations, with today's solver as fallback.
- **First target:** a proof the plugin cannot produce today. Test4's
  `weak_bisimilar` under `compLTS`, or CADP glued ≈ layered at two
  processes.
- **Done when:** that proof `Qed`s under the 6G cap, and every suite proof
  can be produced both ways with the same statement.
- **Relates to decision 3** (`doc/decisions.md`). This phase is its
  structural answer, so take that decision together with this phase.

### S5 — property-initialised queries, finite parameters (New feature)

- **What:** a command form for *for all `x`, `y` with `φ x` and `ψ y`*,
  over finite schemas. It reports one of:
  - a proof, through S4;
  - the counterexample set;
  - the strongest condition under which the pairs are bisimilar.
- **Lifts** the limits "goal shapes: closed terms only" (for finite
  schemas) and "LTSs with parameters" (finite parameters).

### S6 — data: constraints, narrowing, subsumption (New feature, research)

- **What:**
  - data fields in the schema;
  - constraints, through an SMT solver or Rocq-decidable theories (to be
    chosen in this phase);
  - symbolic states from patterns with constraints, expanded by narrowing,
    merged by subsumption, and widening where subsumption is not enough;
  - bisimulation over data in the style of Hennessy and Lin.
- **Lifts** "open terms" and "a premise matching an argument nothing fixes"
  (`doc/limitations.md`) for the theories covered.
- **Risk:** this is the research-grade part of the plan. Each theory added
  is its own piece of work.

### S7 — compositionality through congruence (optional)

- **What:**
  - prove, once per operator, that weak bisimilarity is a congruence
    for it (parallel and restriction; not choice, for weak bisimilarity);
  - minimise each component separately, then compose, as CADP's tools do;
  - certificates reuse the congruence lemmas.

## Determinism

The explicit solver's step counts depend on the order of iteration; the
whole proof-suite baseline rests on that.
- **Explicit engine (S1):** keeps today's order, so every count stays
  identical.
- **Symbolic engines:** produce relations, not step sequences. Their
  certificates are compared by verdict and by statement, not by step
  count.
- **When a symbolic engine enumerates** (to report counterexamples, or to
  fall back to the per-pair solver), it does so in a fixed variable order,
  so results are reproducible across runs.

## Decisions for @dcastrop

1. **BDD library:** `mlcuddidl` (CUDD, mature, C) or a native OCaml BDD
   (deterministic, no C, slower). S0 measures both.
2. **Data theories for S6:**
   - an external SMT solver (fast, but outside Rocq's trust base:
     certificates then have to be re-checked in Rocq);
   - or Rocq's decision procedures only (`lia`, reflection).
3. **Property syntax for S5:** boolean functions only, or a dedicated
   fragment.
4. **Order of S3 and S4:** deciding at scale first (sooner useful, but
   not proved), or certificates first (proved, but on small systems at
   first).
5. **Decision 3** (`doc/decisions.md`) taken together with S4.

## Risks

- **Bad encodings blow BDDs up:** queues, arithmetic encoded in bits, a
  wrong variable order. Mitigations: S0 measures before anything is built,
  and data stays out of BDDs (S6).
- **Linearisation will not cover every Rocq LTS.** It needs SOS-style
  rules over syntax with finitely many derivatives per component. Other
  LTSs keep the explicit route, which stays.
- **Certificates may be too large to check.** S4 measures on one hard
  case before generalising.
- **Scale:** S2, S4 and S6 are each months of work rather than weeks. S0
  and S1 are small, and worth doing whatever happens next.
