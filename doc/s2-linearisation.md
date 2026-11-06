# S2: design

Phase S2 of [`SYMBOLIC_PLAN.md`](../SYMBOLIC_PLAN.md). Restarted
2026-11-06: the answers given on 2026-11-05 were withdrawn.

**Status (2026-11-06):** reading rules is implemented (decisions 1-5 and
7-9):
- **The library:** `lib/rules/` (`Rules.of_lts`, `Rules.pr`).
- **The command:** `Print MeBi Rules <lts>`.
- **The test:** `test/rocq/Test.v`, `PrintRules`.
- **Tried on:** CCS's `step`, `Proc`'s `compLTS`/`termLTS`, CADP's
  `lts`/`step`, and a counter.

The bit format (decision 6) is the next step.

## The task

**Today:** given a Rocq LTS (a relation `M -> option L -> M -> Prop`) and
a start term, the plugin builds the graph of reachable states and steps.

**S2:** the plugin reads each constructor of the LTS as a **rule**.
Example:

    tick : forall n, R (state0 n) (Some n) (state0 (S n))

reads "for every n, `state0 n` steps to `state0 (S n)` with label n".
Later phases work on rules, not on states:
- **S3:** deciding with BDDs.
- **S4:** proofs from a relation.

The rules are meant to replace the graph-building code
(`src/graph_builder.ml` `build`, and `check_valid_constructors` in
`lib/rocq_tools/rocq_monad_utils.ml`) eventually.

## Decisions

### 1. A rule is one constructor (agreed 2026-11-06)

A rule holds:
- **its variables:** the constructor's `forall`s;
- **its source, label and target:** Rocq terms over those variables;
- **its premises.**

Which premises are allowed is decision 2.

### 2. Which premises a rule may have (agreed 2026-11-06)

**Agreed:**
- **Steps of an LTS.**
- **Any decidable proposition,** provided it can later be resolved
  symbolically. It is kept on the rule as a condition over the rule's
  variables, not decided at reading time. (How the plugin finds a
  proposition's decision procedure is decision 2a.)

**Background:**

**Today** every premise goes through `check_updated_ctx`
(`lib/rocq_tools/rocq_monad_utils.ml`, line 1629):
- **A step of an LTS** named in `Using`: `handle_app` (line 1672)
  explores it recursively.
- **Any other proposition:** `check_unknown_app` (line 1803) decides it
  for concrete terms by a bounded proof search, `Premise_search.prove`
  (`lib/rocq_tools/premise_search.mli`, line 44).
  - **Undecided:** postponed (`resolve_deferred`, line 1131).
  - **Still undecided after that:** assumed true, with a warning.

A rule's premises mention variables, not concrete terms, so arbitrary
propositions cannot be accepted as they are.

### 2a. Decision procedures: stdlib's `Decidable` class (agreed 2026-11-06)

A premise `P` that is not a step is allowed when typeclass resolution
finds an instance of `Stdlib.Classes.DecidableClass.Decidable P`:

    (* ~/.opam/default/lib/coq/theories/Classes/DecidableClass.v:15 *)
    Class Decidable (P : Prop) := {
      Decidable_witness : bool;
      Decidable_spec : Decidable_witness = true <-> P }.

No new dependency (`rocq-stdlib`, already in `dune-project`, line 50).
Instances exist for:
- `=` on `bool` (`Bool/Bool.v`, line 161), which covers CCS's
  `allowed ns l = true`;
- `=` and `<=` on `nat` (`Arith/PeanoNat.v`, lines 152 and 170);
- `~ P` (`DecidableClass.v`, line 67);
- `=`, `<`, `<=` on `Z` (`ZArith/BinInt.v`, lines 392-412).

Users add instances for their own propositions.

What the rule keeps, and why:
- **`Decidable_witness`,** a boolean over the rule's variables: the
  symbolic side (S3, S6) works with it, outside Rocq.
- **`Decidable_spec`:** used only when a result is turned back into a Rocq
  proof (S4).


### 3. Location: `lib/rocq_tools/` (agreed 2026-11-06)

Written to replace the graph-building code eventually.

### 4. No comparison with today's graphs (agreed 2026-11-06)

**Instead:**
- **reading:** the rules printed by the debug command (decision 5), pinned
  in `test/rocq/Test.v`, check that rules are read correctly;
- **end to end:** S3's verdicts are the real check.

### 5. Visible only through a debug command (agreed 2026-11-06)

### 7. The debug command: `Print MeBi Rules <lts>` (agreed 2026-11-06)

It prints each rule:
- **its name and variables;**
- **its premises,** each step premise as a step and each decidable one
  marked `[Decidable]`;
- **its conclusion,** as `source --label--> target`.

The output for a small LTS is pinned in `test/rocq/Test.v`.

Files: `src/g_mebi.mlg`, `src/help.ml`, `test/rocq/Test.v`.

### 6. Terms as bits, and the size of the format (agreed 2026-11-06)

**The encoding:**
- **Serialised terms:** a state is its term, serialised as a tree. Each
  tree position holds which constructor of the term type sits there, or
  that nothing does.
- **Fixed format:** the BDD's variables are a fixed set of positions, so
  the format has a maximum depth. A step whose target would not fit is an
  error, never cut silently.
- **Functions in rules** (`tsubst`, `do_act`) are compiled to BDD
  operations, unrolled up to that depth.

**The depth:**
- **Given by the user:** `Set MeBi MaxDepth <n>.`, for efficiency.
- **Otherwise:** a heuristic start, enlarged and rerun whenever a target
  overflows, up to a maximum number of attempts. Past that, an error
  reports the largest depth tried.

**A limit:** a system whose terms grow without bound (`P = a.(P | P)`)
has no depth that fits. See "Infinite-state systems" below.

### 8. Code: a new library, `lib/rules/` (`rocq-mebi.rules`) (agreed 2026-11-06)

**What it holds:** the rule type, reading an LTS's constructors into
rules, and printing them.

**Dependencies:**
- **It uses** `rocq-runtime` and `rocq-mebi.rocq_tools`: `Rocq_utils`
  (`extract_args`, `lts_shaped`) and `Premise_search.is_prop`.
- **Only `src/` uses it.**

**Later libraries:** S3's bit format and BDD code (which needs CUDD) go in
a library of their own, so the rules never depend on CUDD.

No existing logic changes.

### 9. `theories/loader.v` Requires stdlib's `Decidable` class (agreed 2026-11-06)

`From Stdlib Require Classes.DecidableClass.`

**Why it is enough:**
- **A `Require`, not an `Import`:** no names enter the user's scope.
- **The stdlib's instances are `#[global]`,** so they are active once
  loaded (`Bool/Bool.v`, line 160; `Arith/PeanoNat.v`, line 151;
  `Classes/DecidableClass.v`, line 66).

## Later

- **Infinite-state systems** (`P = a.(P | P)` against `Q = a.Q`): need
  symbolic states (S6) or up-to techniques in proofs (S4), not BDDs over
  a fixed format.

Not part of S2's first step:
- **Parallel structure and structural congruence:** detecting them in the
  rules, to shrink systems such as Test4.
- **Code structure:** reusing `LTS.constructor`
  (`rocq_monad_utils.ml`, line 451), which already holds each
  constructor's context and conclusion.
