# Bitboard-like storage for the model

A plan, written 2026-11-05. Nothing in it is implemented.

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
