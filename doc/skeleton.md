# Phase 1: from the user's rules to a vector

`SYMBOLIC_PLAN.md`, Phase 1. Built 2026-11-06: `lib/skeleton/`, and the
debugging command `Print MeBi Skeleton <term> Using <lts>`.

## Terms as containers (2026-11-10)

A **container** `S ◁ P` (Abbott, Altenkirch and Ghani, 2005) is a set of
shapes `S` and, for each shape `s`, a set of positions `P(s)`; an element
of its extension `⟦S ◁ P⟧(X) = Σ s∈S. (P(s) → X)` is a shape and a value
in `X` at each position. The symbolic route reads a state this way, in
three stages (`lib/skeleton/container.ml`, then `lib/learn`):

1. **The term language** (`Container.view`): a closed term is a
   constructor applied to parameters and fields, or something else.
   Nothing here depends on the rules.
2. **Refinement by the rules** (`Container.split`, given the structural
   constructors that `Skeleton.classify` finds in the rules): the shape is
   the term's top part made of structural constructors, its positions the
   paths (field indices from the root) to where that part stops, its
   values the subterms there. A value never starts with a structural
   constructor, so the split is canonical. If the state type is
   `μX. F_s(X) + F_v(X)`, with `F_s` the structural constructors, a term
   is an element of `⟦F_s*⟧(V)`: the free monad on `F_s` (itself a
   container: shapes are trees of structural constructors, positions
   their leaves), applied to the values `V`. The rules also classify each
   summand (`Skeleton.kind`): **local** (the shape stays), **structural**
   (the shape changes, values are moved or dropped: `0 | 0 -> 0`,
   `do_par_end`, `do_assoc`) or **dynamic** (a computed value may have
   structure, so the target's shape is known only with the value).
3. **Serialisation** (`lib/learn/learn.ml`, "Serialisation"): a shape
   becomes a number, a position a vector index, a value a number; a state
   is the vector `[shape; the value at each position]`, padded with 0, as
   Sylvan's list decision diagrams store it. **A position is a path**,
   numbered the first time a shape of the system has it (the start
   shape's in the order of its slots' use), so the same path has the
   same index in every shape: a step that changes the shape at one place
   leaves every value elsewhere where it was, and the engine's relation
   copies those positions instead of reading them.

The decision diagram is then a set of container elements, serialised:
its first level is the shape, the next levels the positions, and the
sharing of the diagram is the sharing of values among positions. A
component is a place in a shape: its keys are what the positions at or
below that place hold, and a step there may change the shape below it
(the shapes reachable at that place), never above.

## What it computes

Everything comes from the user's rules (`lib/rules`) and the start term,
with no knowledge of any particular calculus.

1. **Structural constructors:** those that some rule keeps at the same
   place in its target. CCS's `par` qualifies, and so does Proc's `cpar`,
   kept by `do_parl` although `do_par_end` dissolves it. Constructors that
   occur in sources but that no rule keeps (`pre`, `tend`) are listed as
   such.
2. **Shapes and slots.** A term splits canonically: its structural
   constructors on top form its **shape**, and the values below fill its
   **slots**. A slot's value never starts with a structural constructor,
   so a rule's structural constructors match nodes only.
3. **The state is a shape plus its slots' values;** the shape is a finite
   control component (decided 2026-11-06: it replaces D3's collapse).
4. **The summands of a shape:** every rule chain that can fire from it.
   - A premise over the same LTS on a part of the shape recurses.
   - Any other premise (another LTS, or a computed term), and any step of
     a slot's value, is a step of a closed premise, which the interpreter
     follows from that LTS's rules. It is shown as `<lts>`.
   - **Its target:** the shape that results, and where each slot's value
     comes from: **moved** unchanged from a source slot, or **computed**
     from some.
   - Label constructors prune impossible chains (a silent synchronisation
     is not a premise labelled `Some _`).
5. **Values whose structure is certain are split at once.** A computed
   value of a single-constructor structural type (a pair, a record) is
   expanded into its node.
6. **Dynamic summands:** a computed value whose type has structural
   constructors among several may start with one at run time, as when a
   `tfix` unfolds into a `tpar`. Its shape is known only then: Phase 2
   splits such values when it learns the steps.
7. **The reachable shapes:** found by following the summands' targets
   from the start shape, up to a bound (200).

## Results (2026-11-06)

| system | start shape | shapes | summands (keep / change shape) | slots written by a shape-keeping summand, on average | dynamic |
|---|---|---|---|---|---|
| CCS, `abp` | `res P0 (par (par P1 P2) (par P3 P4))` | 1 | 16 / 0 | 1.8 | 16 |
| Proc, stacked `compLTS`, Test4's `p` | `cpar (cpar (cprc P0) (cprc P1)) (cpar (cprc P2) (cprc P3))` | 9 | 39 / 31 | 1.3 | 0 |
| Proc, flat `termLTS`, Test1's `p` | `P0` (its top, `tfix`, is never kept) | 1 | 1 / 0 | 1 | 1 |
| CADP fine-grained `lts`, 2 processes | `pair (PAR (PRC ..) (PRC ..)) ..`, 10 slots | 1 | 2 / 0 | 6 | 0 |
| CADP fine-grained `lts`, 3 processes | likewise, 14 slots | 1 | 3 / 0 | 6 | 0 |
| `Test.v`'s `PrintSkeleton`, `par` with "0 \| 0 -> 0" | `par (par P0 P1) P2` | 3 | 6 / 2 | 1 | 8 |
| `Test.v`'s streams (`nats 0`) | `P0` | 1 | 1 / 0 | 1 | 0 |

**What the table shows:**
- **CCS:** the ABP's synchronisation vectors exactly: 4 local steps and
  12 synchronisations, each writing one or two components. Every summand
  is dynamic, because a component's value could become a `par` (process
  creation); the ABP's never do, which only run time shows.
- **Stacked Proc:** the 9 shapes predicted (binary trees of at most 4
  leaves). `do_par_end` and `do_assoc` move between them.
- **Flat Proc:** no structure until `tfix` unfolds: all of it is found at
  run time.
- **CADP:** each process's step writes its own term, its state's fields
  and the resource's fields, and leaves the other processes alone. Writes
  are counted as every computed field, so this over-estimates: some
  fields may be unchanged, which only run time shows.
- **Streams:** no structural constructor, so one slot. Infinite systems
  are Phase 6's.

## Superseded

The first version (same day) kept only constructors that **every** rule
keeps, and collapsed a node into one slot where a rule changed its shape
(D3's default). Proc then had no skeleton at all: one rule dissolving
`cpar` was enough. The shape was made part of the state instead (D3), which is generic for process calculi ("0 | 0 -> 0", structural
rules).
