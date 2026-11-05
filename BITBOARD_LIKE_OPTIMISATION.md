# Bitboard-like storage for the model

A plan, written 2026-11-05. Phase 0 (measure) is done, and the plan was
revised after it, as Phase 0 requires: see "Revision after Phase 0" at the
end. Read that section first. The rest is the plan as first written.

Since 2026-11-05 `SYMBOLIC_PLAN.md` supersedes the symbolic extension
below, and Phase A is that plan's Phase S1: the explicit engine behind its
engine interface.

## Goal

Store states, sets of states and transitions much more cheaply.

The idea comes from chess engines' bitboards. A state is a small integer.
A set of states is a bitset. A transition relation is one bitset row per
state and label. Set operations then work on 64 states at a time.

The design must also leave room for **symbolic states**, so that the tool
can later handle infinite-state systems. That later step is a new
capability and is not part of this plan (see "The symbolic extension"
below).

## Where the cost is today

A state is already cheap: `{ base : enc }`, one integer encoding. It indexes
a two-way table to the Rocq term (`lib/rocq_tools/bi_encoding.ml`). The
cost is in what hangs off the states:

- **Sets of states** are OCaml `Set`s, which are balanced trees: a few
  words per element, and a pointer walk on every operation.
- **Edges** (`EdgeMap`, `lib/model/components.ml`) are a hash table from
  each state to a hash table from each action to a `Set` of target states.
- **Actions** (`Action`, `components.ml`) carry the label, the weak
  transition's witness (`Annotation`: the whole path) and the set of
  derivation trees. Each saturated edge stores its full path, and the same
  paths are stored again and again.
- **Saturation** (`lib/model/algorithms/saturation.ml`) builds those
  witnesses for every weak edge, whether or not a proof ever reads them.
  This is the known limit "memory quadratic in the number of silent SCCs"
  (`doc/limitations.md`).
- **Term comparison:** lookups in the Rocq-term table compare terms with
  `EConstr.eq_constr`, and hash them through `EConstr.to_constr`. This is
  the `econstr_eq` item in `doc/future-work.md`.

**Phase 0 must confirm this.** It comes from reading the code. Nothing has
been measured yet.

## The design

### Dense identifiers and an arena

- **States and labels** are numbered `0 … n-1` in the order they are found.
- **The arena** maps each state id to its *descriptor*, the thing the id
  stands for. Today a descriptor is an encoding (`enc`); later it can be
  something else (see "The symbolic extension").
- **The algorithms** work on ids only and never look inside a descriptor.

### Sets of states

- **Representation:** a bitset module, `Bits`, over `Bytes`, or an
  `int array` of 63-bit words.
- **Operations:** `union`, `inter`, `diff`, `subset`, `is_empty`,
  `cardinal` (popcount) and `iter` in ascending id order.
- **Where it applies:** blocks in partition refinement (`minimization.ml`),
  silent closures, terminal sets, and visited sets.

### Transitions: dense where they fill up, sparse elsewhere

- **Strong edges** are usually sparse. Store them per label as compressed
  sparse rows (CSR). While extracting, collect edges in a mutable builder;
  freeze the builder into arrays once extraction is done.
- **Silent closure** (τ*, the states reachable by silent steps) and the
  **weak relation** fill up. Store them as dense bit-matrices: one `Bits`
  row per state.
- **The memory bound** of a dense matrix is n²/8 bytes per label. For
  `Proc/Test4`'s 9,720 states that is about 11.8 MB per label: fine for τ
  and a handful of labels, too much for dozens. Choose dense or sparse per
  label, by density. Chess engines face the same tradeoff and keep a piece
  list beside their bitboards.

### Saturation as matrix algebra

- **τ\*** is the transitive closure of the τ matrix. With word-parallel
  rows, Warshall's algorithm costs n³/64 word operations. Computing it per
  silent SCC, on the condensed graph, is cheaper still.
- **Weak steps** `=a=>` are the boolean product τ\*·a·τ\*, row by row.
- **No witnesses** are built during saturation. Only reachability is
  stored.
- **On-demand saturation** (`Saturation.on_demand`, used for `Test4`)
  becomes computing a single row: the row's bitset is the cache entry.

### Witnesses and derivation trees, on demand

- **Off the edge:** the witness and the derivation trees move out of
  `Action`, into side tables keyed by `(src, label, dst)` ids.
- **Strong edges** keep their derivation trees: one set per edge, stored
  once in the side table.
- **Weak edges** get their witness rebuilt when the proof solver asks for
  it (`Saturation.silent_paths`, `Product.respond`): a breadth-first search
  over the τ rows, finding a shortest path, as today.
- **Effect:** memory for witnesses goes from one per weak edge to one per
  edge the proof actually uses.

### Determinism: the main risk

Proof step counts depend on the order in which the solver meets states,
actions and witnesses. Two examples:
- **Witness choice:** the solver takes the first of several shortest
  witnesses.
- **Table order:** `ReModel.transition` (`src/proof_solver_step.ml`)
  breaks ties by table order. The comment on `Action.hash` in
  `components.ml` keeps that order on purpose.

A new container with a new iteration order can change step counts, even
when it is correct.

**Rule:** every phase keeps the current order, or documents why a count
moved. Ids are assigned in the order states are found today. Where a
`Hashtbl` order matters, record it explicitly as a sequence instead of
relying on hashing.

## The symbolic extension (later; a new capability)

The arena makes this a change of descriptor type, not of algorithms.

- **Descriptors** become
  `Concrete of enc | Symbolic of pattern * constraint`.
  - A pattern is a term with variables.
  - A constraint restricts what those variables may stand for.
  - The ids, the bitsets and the matrices stay as they are.
- **Hash-consed terms:** for patterns, states should become terms the
  plugin itself represents, hash-consed, instead of opaque encodings. One
  translation from Rocq terms, at extraction. Equality is then a pointer
  comparison, which also takes `econstr_eq` off the model's path.
- **"Same state"** becomes "is covered by": the merge step in the arena
  asks whether one descriptor subsumes another.
- **Product cubes:** for parallel systems (`Proc`, CADP), a global state is
  a tuple of component states. Give each component its own bitboard, as
  chess keeps one per piece type. A set of global states is then a product
  of component masks, a *cube*: already a finite symbolic state, available
  before any constraint solving.
- **Out of scope here:** symbolic bisimulation in the style of
  Hennessy–Lin, and an SMT backend for constraints. They belong to the
  capability's own plan.

## Phases

Each phase ends with the same checks:
- `dune build`, `dune exec test/tests.exe`, and `make`;
- `Test.v`'s counts (`bench/testv-counts.sh`);
- the proof suite, `bench/proofs.sh -m auto,True,False` against the
  previous commit, with `bench/compare.sh`: every Solve identical, or each
  difference explained;
- for phases that change memory: peak memory and callgrind instruction
  counts on `Proc/Test1`, and peak memory on `Test4` (under the memory cap
  in `CLAUDE.md`).

### Phase 0 — measure

- **What:** a heap census at the end of saturation on `Test4`,
  `CADP/MutualExclusion` and `CCS/ABPProofs`, using `Gc.stat`, plus
  `Obj.reachable_words` on the states, edges, witnesses and the encoding
  table separately.
- **Done when:** we know which of these dominates. If edges and witnesses
  do not dominate, revise this plan before going on.

### Phase 1 — `Bits`, and ids in the arena

- **What:** a `Bits` module in `lib/utils` (pure OCaml, with unit tests in
  `test/tests.exe`), and an `Arena` from id to descriptor.
- **Also:** `State.t` becomes an id. The encoding moves into the arena.
- **Done when:** behaviour is unchanged, and all the checks above pass.

### Phase 2 — sets and partition refinement on bitsets

- **What:** `States` becomes `Bits`. `minimization.ml` and `bisimilarity.ml`
  work on bitset blocks.
- **Done when:** the same verdicts and step counts as before; time measured.

### Phase 3 — edges as CSR, with metadata in side tables

- **What:** `EdgeMap` becomes CSR per label. Derivation trees move into a
  side table. `Action` keeps the label only.
- **Done when:** iteration order is kept (see "Determinism") and step counts
  are identical.

### Phase 4 — saturation by matrices, witnesses on demand

- **What:** τ\* by closure over silent SCCs, weak rows by product, and
  witnesses rebuilt by breadth-first search with today's tie-breaking.
- **Also:** check the new saturation against the old one on every FSM in
  `tests.exe`: the same weak edge sets, and the same witness for every
  edge the solver asks about.
- **Done when:** step counts are identical, and `Test4`'s peak memory has
  been measured against today's.

### Phase 5 — clean up

- **What:** delete the old containers and the old witness-building code,
  and update `doc/limitations.md` (the quadratic memory limit) and
  `doc/future-work.md`.

## Open questions

- **Dense or sparse:** the density threshold per label. Phase 0 should
  give the label counts and densities to set it.
- **Hash-consed terms:** whether they come in before the symbolic work, as
  the fix for `econstr_eq`, or with it.
- **On-demand saturation:** whether it survives at all once a full
  saturation of `Test4` fits in memory.
- **Phase 2:** whether `Set`'s ordered iteration is relied on anywhere
  that ascending id order would not reproduce.

## Revision after Phase 0 (2026-11-05)

The measurements are in
[`doc/measurements/bitboard-phase0.md`](doc/measurements/bitboard-phase0.md).
They contradict this plan's premise in one place and confirm it in another.

### What Phase 0 found

- **Proofs: the model is not the cost.**
  - At `Sim Begin`, the model is 0.2% of the live heap on CADP
    MutualExclusion and 8% on ABP.
  - A proof file's peak (2.9GB, and over 6GB) comes later, in `Solve` and
    `Qed`, when the model is a few MB of it.
  - Phases 2 and 3 (sets and edges on bitsets) cannot move a proof's
    memory. Their time effect is unmeasured, but the solver's own cost
    is elsewhere (`doc/future-work.md`, "The solver's own per-step cost").
- **Inside a saturated FSM, witnesses are 73–75% of the edges**, as
  expected.
- **In Test4's strong FSM, derivation trees are 55%**: 2.5M words of 4.5M,
  29 words per edge.
- **Whole saturation is the one place where the representation decides
  what is possible.** Test4's 74.6M weak actions are estimated at 34–67GB
  today; as bit-rows they would be 23.6MB.
- **A correction to "Where the cost is today".** The "memory quadratic in
  the number of silent SCCs" limit is the *saturation estimate*
  (`saturation_estimate.ml`, `doc/future-work.md`). That code already uses
  dense bitsets over SCCs; the memory is in their K² bits. Bitboards do
  not fix it: only sparse rows or computing less would.

### Library choice: Zarith, not a hand-written `Bits`

- **What:** Zarith's `Z.t` is a GMP integer, with `logand`, `logor`,
  `popcount`, `testbit` and `trailing_zeros`. GMP's low-level `mpn` layer
  runs these in hand-tuned SIMD assembly.
- **Why:** Rocq already depends on Zarith, so the plugin gains no
  dependency, and the model libraries gain no Rocq dependency (Zarith is
  not Rocq).
- **Cost:** a `Z.t` is immutable, so a union allocates. For row building
  this is acceptable; measure it in Phase A.
- **Already in the code:** `saturation_estimate.ml` has a hand-written
  `Bits` over `int array`. Keep it for now, and fold it into the Zarith
  rows only if Phase A shows Zarith is no slower.
- **Considered and not chosen:**
  - **BLAS (Owl, Lacaml):** a float per entry, ~0.4–0.75GB per matrix at
    Test4's size.
  - **M4RI:** multiplies over GF(2), not the boolean semiring.
  - **SuiteSparse:GraphBLAS:** fits exactly (the boolean semiring, and
    sparse or dense chosen per matrix), but has no OCaml binding and would
    add a C dependency. Keep it in reserve for Phase A, should Zarith rows
    prove too slow.
  - **CUDD (BDDs, `mlcuddidl`):** only pays off with structured state
    encodings. It belongs to the symbolic extension, and is discussed there
    with @dcastrop, not here.

### Revised phases

Every phase keeps the checks listed under "Phases".

- **Phase A: whole saturation by bit-rows, witnesses on demand.** This is
  the old Phase 4, now first and on its own.
  - **Storage:** per FSM, ids for its states, then τ\* rows by closure
    over silent SCCs, then one weak row per (state, visible label), all as
    `Z.t`.
  - **Witnesses:** built only when the solver or a dump asks, by the
    existing breadth-first search with today's tie-breaking.
  - **Containment:** behind `Saturation`'s interface. Where code needs an
    `EdgeMap` (proof solver, dumps), it gets one built per state on
    request, through the same `fill` hook that on-demand saturation
    already uses.
  - **Done when:** step counts are identical (`satdiff` and the proof
    suite), and `Run Saturate` on Test4 fits under the 6G cap. That
    needs `Bounds Saturation` to count rows' bytes, not weak actions:
    a behaviour change to agree first.
- **Phase B (optional): derivation trees off the edges.** For large strong
  FSMs (55% on Test4). Only if Phase A leaves extraction-time memory as
  the limit.
- **Dropped:** the old Phases 1–3 as model-wide rewrites (ids everywhere,
  `States` as bitsets, CSR edges). Phase 0 shows no memory to gain from
  them in proofs, and they carry the determinism risk.
- **Unchanged:** the symbolic extension remains future work, and a new
  capability. If it comes, its first step is an engine interface (sets,
  image, closure, fixpoints) with Zarith rows as the first engine, rather
  than the arena of the old Phase 1.

### Open after the revision

- **`Bounds Saturation`:** whether it moves from counting weak actions
  to estimated bytes (Phase A). That is a behaviour change: agree it
  first.
- **ABP at its cap:** `CCS/ABPProofs.v` was OOM-killed at its 6G cap
  in `Qed` during Phase 0. It is recorded in `ASSISTED-CHANGES.md`
  (2026-11-05).
