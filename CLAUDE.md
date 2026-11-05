# CLAUDE.md

Guidance for Claude Code sessions working on `mebi_plugin`.

## Working assumption

The plugin's core functionality is a finished rough draft. Remaining work is
refactoring, bug fixes, restructuring and optimization — **not** new plugin
capabilities. If a requested or seemingly-natural change would add a genuine
new capability to the plugin (something it could not previously do), say so
explicitly *before* writing it, not after. Net-new code that is purely
infrastructure (e.g. a pure-OCaml test binary) is fine, but should still be
labelled as such rather than silently blended into a refactor.

## Keep `ASSISTED-CHANGES.md` current

After any substantive piece of work, add or extend an entry in
`ASSISTED-CHANGES.md` (repo root). Tag each change **Refactor**, **Bug fix**,
**Optimization**, **New feature**, **Tooling** or **Docs**, and keep a running
per-session tally. This log is shared with collaborators (David Castro-Perez
owns the upstream repo) as a transparency record of what Claude contributed
and of what kind — be honest about mistakes and reversals there; that
candour is the point of the log.

## Keep `OUTSTANDING.md` current

`OUTSTANDING.md` (repo root) is the one *index* of what is still open:
one line per item, saying who it waits on and where its detail is. Keep
it short. Detail goes in a scoped `doc/` file, never in the index:
- `doc/decisions.md`: briefs for @dcastrop. Write them for a reader who
  has not seen the code in months: concrete file names, concise.
- `doc/limitations.md`: what the tool cannot do today.
- `doc/future-work.md`: possible work, and what was set aside.

The index should only shrink. When an item is resolved, delete its line
(and its detail), and record the resolution in `ASSISTED-CHANGES.md`.
When something new is found, add one line to the index and the detail to
the right `doc/` file. Don't grow open-item lists elsewhere (`notes/` is
local). `TODO.md` was folded into it on 2026-10-10 (`git show
cbbdd39:TODO.md`); the index replaced a 438-line version on 2026-10-13
(`git show 6b9b38f:OUTSTANDING.md`).

## Verifying changes to `lib/model` or the proof solver

`test/rocq/Test.v` and `DevTest.v` alone are **not** sufficient coverage for
changes touching the model or `src/proof_solver*`. The tests that actually
exercise the proof solver end to end are the `examples/Bisimilarity/**/
PluginProofs.v` files, which `dune build` does not build
(`examples/dune` lists only the fast examples: full proof search is slow).

The project builds with **dune only** (since 2026-11-05; the `make` build
from `_CoqProject` is gone, and `_RocqProject` is for the IDE). Run the
proof files with `bench/proofs.sh -m auto,True,False -j 1 -M <cap>` (and
`-s abp`, `-s test4` for the heavy ones), which runs each file
memory-capped against the dune build, forces the strategy without patching
`api.ml`, and writes per-Solve tables that `bench/compare.sh` diffs between
two builds; see `bench/README.md`. Keep `-j 1` and a cap well below the
machine's free memory.

1. The suites. The five proof files of the default suite are `Proc/Test1`, `Proc/Test2`, `Proc/Test3`
   (`PluginProofs.v` each), `examples/CADP/MutualExclusion.v` and
   `examples/CADP/GluedLayered.v`. `Proc/Test3` adds ~9s; the other four
   together are under a minute. (CADP's other proof files are not in it:
   `examples/README.md`, "CADP"; its `*Large.v` need a memory cap.) (`Test4` has no `PluginProofs.v`: its LTS has
   9720 states, which now *extract* in ~25s, but saturating them exhausted
   memory — backlog item B3. Since 2026-10-03 `Run Bisim` and the proof tactics
   saturate such FSMs on demand and decide on the silent-SCC quotient (~45s;
   `MeBi Config Saturation OnDemand`, notes/13); `Run Saturate` still refuses
   with `Saturation_Too_Large`. A direct proof is out of reach (proving one
   state per silent SCC is unsound inside a coinduction: notes/13, `Test.v`
   `CircularTransfer`), but `Test4` **is** proved via a normalised semantics
   with structural congruence made explicit (`examples/ProcCongruence.v`,
   `Test4/NormProofs.v`: 6,115 steps, ~30s, ~0.7GB (48,821, ~7 min, ~3.8GB
   under `Answers Default`, until 2026-10-15); `weak_bisimilar` in
   `Test4/NormBisimProofs.v` with `Answers Minimal`: 61,161 steps, ~4 min,
   ~2.8GB; neither built by default). Run
   anything on `Test4` under a memory cap,
   with `systemd-run --user --scope -p MemoryMax=6G -p MemorySwapMax=0` --
   `ulimit -v` does not work, OCaml 5 then cannot reserve its heaps. An
   uncapped run took the whole machine down.)
   Two more, `CCS/ABPProofs.v` (`abp <= spec`, 6494) and
   `CCS/ABPBisimProofs.v` (`weak_bisimilar abp spec`, 9914), the
   Alternating Bit Protocol, are also commented out: ~3.7 min and ~4.2GB
   peak each, so run them under the same memory cap, one per process (one
   file with both was OOM-killed at 6GB). They are the
   check for changes to inversion or to how dead steps are refuted
   (`Premise_search.dead`); the five above barely exercise that.
2. Compare the `(Stopped) Solved after N iterations.` lines against the
   known-good baseline, one entry per proof search (each `sim`/`bisim`;
   until 2026-11-05 each `MeBi Sim Solve`), **27 total**. Under
   the default `Auto` solver strategy and `Auto` answer policy:
   ```
   Proc/Test1   88, 82, 88, 88, 22, 10            (nested)
   Proc/Test2   112, 112, 112, 84, 112, 84        (mutual)
   Proc/Test3   408, 355, 483, 483, 555, 195, 307, 291, 307    (mutual)
   CADP/MutualExclusion   259, 202, 81, 55        (nested)
   CADP/GluedLayered      246, 277                (nested)
   ```
   (Since 2026-10-15, when `Answers Auto` became the default. Under
   `Answers Default`, until then: Test1 114, 105, 106, 109, 22, 21; Test3
   995 ... 555 (its 1st and 8th); MutualExclusion 212 and 63; Glued 268,
   355. The rest do not change: `Auto` keeps `Default` where it plans
   cheapest.
   Since 2026-10-02's hypothesis-order fix. Before it: Test3 1073, 387,
   519, 519, 603, 211, 331, 603, 331 and CADP 268, 396, 268, 396, 81, 63.
   Since 2026-10-11, `CADP/Size1/MutualExclusion` proves the layered `lts`
   against `spec_lts`, as its name says; until then it was a copy of
   `Size1/Glued`, so its entries repeated Glued's 268, 355 and 2760. Until
   2026-10-13 CADP's were three files, `CADP/Size1/MutualExclusion`,
   `Size1/Glued` and `Size1/Glued/MutualExclusion`, in that order: 259,
   212, 268, 355, 81, 63, then 1442, 2760, 185.)
   Each of those five files then ends with a `weak_bisimilar` section: one
   proof per pair, 14 total, after the 27 above (and a `mutual_sim` per
   pair, proved from its two `weak_sim`s, with no `Solve`). Under `Auto` and
   forced `MutualCofix True`, in file order:
   ```
   Proc/Test1   305, 318, 51
   Proc/Test2   425, 375, 409
   Proc/Test3   3439, 2801, 4132, 1742, 2649
   CADP/MutualExclusion   1442, 185
   CADP/GluedLayered      1658
   ```
   (Under `Answers Default`, until 2026-10-15: Test1 709, 709, 51; Test2
   702, 852, 852; Test3 13203, 6211, 9379, 4083, 6179; Glued 2760.)
   Forced `False` stops at the first of them in every file (with a loose
   bound, glued mutual exclusion's, `wbis_bigstep_1` in
   `MutualExclusion.v`, alone would finish, at 3859 under `Answers Default`;
   not re-measured). They come last in each file so
   that a forced run still reaches all 27 `weak_sim` counts first.
   `CCS/LawProofs.v` (built by default) adds 14 more, `weak_bisimilar`
   instances of CTrees' CCS laws, with least bounds, so any increase fails
   the build: under `Auto`, `36, 58, 19, 19, 56, 42, 27, 28, 15, 98, 278,
   357, 325, 2836` (until 2026-10-15, under `Answers Default`: 53 for the
   7th and 3192 for the last). Forced `True` differs only at `tau1` (29); forced
   `False` stops at `expansion` (bound 97, the nested path needs 155;
   past it, `paraC`..`parap0` take 2218, 3127, 2853 and `paraA` was
   OOM-killed at 5GB).
   A change to *classification* (Solved/Unsolved) is a regression. A change
   to iteration *count* needs an explanation (fewer is fine if it fixes a
   real miss; more needs justifying).

   **On the solver strategy.** `MeBi Config Solver MutualCofix` is
   `Auto` by default: before any proof step runs, the plugin costs both ways
   of introducing coinduction hypotheses on the model and picks (see
   `Model.Product.estimate`). It prints a `Notice` whenever it takes the
   `mutual` path — that is the deviation from what the solver historically
   did, and it changes the count a checked-in bound was measured against.
   Forcing `True` or `False` reproduces either path exactly. The
   nested-strategy figures for the first five files, should you need them,
   are `21, 22, 63, 81, 105, 106, 109, 114, 182, 194, 268, 268, 278, 299,
   396, 396, 446, 446` — that was the baseline before 2026-09-29.

   **On the answer policy.** `MeBi Config Solver Answers` is `Auto` by
   default (since 2026-10-15): before the search starts the plugin plans every answer
   under `Default`, `Greedy` and `Minimal` and keeps the cheapest. It prints
   an `Info` message ("Answers: ...") for each plan, so
   `MeBi Config Output "Info" False` hides it. Forcing `Default` reproduces
   the solver's earlier counts exactly. Measured on the default suite, all
   161 Solves under `Auto`, forced `True` and forced `False`: none takes
   more steps, none changes status, 61 take fewer
   (`doc/measurements/answers-auto.md`). On an FSM saturated on demand,
   `Auto` answers move by move unless `MaxGame` is set.

   **On bounds.** Each checked-in proof sets its bound just before its
   statement, `Set MeBi MaxSteps N.` (until 2026-11-05, `MeBi Sim Solve
   N` inside the proof; the numbers carried over unchanged). It permits up
   to `N + 1` solver steps before giving up (`solve`'s loop guard in
   `src/proof_solver.ml` is `n <= bound`, not `n < bound`), so a checked-in
   bound one below the reported "Solved after" count (`MutualExclusion` and
   `Glued` use `MaxSteps 267`/`395` and report 268/396) is expected, not a
   bug. `debug bisim <n>` (or `sim`) stops a search after `<n>` steps and
   prints its open goals, for looking at where it is.
   `Proc/Test2`'s bounds are deliberately **loose** — they are the
   nested-path figures, so the file still compiles if someone sets
   `MutualCofix False`. `Proc/Test3`'s `wsim_p3` is checked in at
   `MaxSteps 1127`, from before 2026-10-01's reflexive-pair fix; also left
   loose. A heuristic change to the proof solver should be verified with
   `MutualCofix True` and `False` forced as well as under `Auto` — `Auto`
   alone hid a CADP hang on the mutual path (see `ASSISTED-CHANGES.md`,
   2026-10-01).
   `bench/proofs.sh` gives each file its own log, so every `Solved after N`
   line is attributable; a file's line order is its proofs' order.

For pure-OCaml model changes with no Rocq/proof-solver involvement,
`dune exec test/tests.exe` (expect 111/111) is a much faster first signal, but
does not substitute for the proof-suite run above when the change touches
anything the proof solver reads.

## Working conventions and gotchas

Moved here from the local planning notes on 2026-10-10, so that a fresh
session has them.

**Conventions**
- Commit per completed step. Run `dune fmt` before committing.
- Gate a commit on the build. A `git commit` chained after a build in one
  command commits a broken tree.
- Push branches to `fork` (`thecathe/mebi_plugin`) and open PRs there.
  Never push to `origin` (`dcastrop`): it gets a single PR, when Jonah
  says. (50 commits reached `origin/main` by mistake on 2026-09-29, from
  a session whose git setup sent the push there. Before any push, read
  `git remote -v` and `git branch -vv`, and name the remote explicitly.)
- A new capability is flagged and agreed before code (see the top of this
  file).

**Rocq and the proof solver**
- **Rocq reuses freed hypothesis names** (after `inversion_clear` or
  `clear`). Never identify a "new" hypothesis by name; use context order,
  or name *and* type.
- **Solver steps run focused on the first goal** (`Declare.Proof.by`). A
  step cannot see or reorder sibling goals; reorder inside the tactic
  that creates them, as `move_premises_last` does.
- **Constructor binders are walked last to first** in extraction
  (`check_updated_ctx`); `cross_product`'s prepending turns the tree's
  children back into premise order.
- **Encodings are renumbered by every command.** Never key anything that
  outlives a command by an encoding; compare labels across dumps by term.
- **`Reset Bounds` resets more than the state and transition bounds**:
  also `MaxSaturation`, `MaxGame`, `MaxMemoryMB` (to its default),
  `Saturation OnDemand`, `PremiseDepth` and `PremiseRange` (not the premise
  tactic: that is `Reset Premise`; nor `MaxSteps`). Set
  them after it. The bounds are Rocq options (`Set MeBi ...`) since
  2026-11-05.
- **In `Proc`, a send and a receive on one channel carry the same label**
  (`Some B`). Adding a component often leaves systems bisimilar, so check
  the verdict before calling a pair "non-bisimilar".

**Builds**
- **Write `Stdlib.Option`, not `Option`,** in plugin code: inside the
  plugin `Option` can resolve to Rocq's module. (The `make` build that
  enforced this, and warning 50 on misplaced doc comments, is gone since
  2026-11-05; keep each `(** ... *)` directly before what it documents.)
- **Code that differs between Rocq 9.2 and 9.3 is under cppo's
  `#if ROCQ_VERSION >= (9, 3, 0)`.** Only the files listed in two places
  are preprocessed: the `(preprocess (per_module ...))` in
  `lib/rocq_tools/dune` or `src/dune`, and `.ocamlformat-ignore`
  (ocamlformat cannot parse `#if`). A file that gains an `#if` goes in
  both.
- **A signature shared by a module's `.ml` and `.mli` lives once, in
  `foo_intf.mli`** (an interface-only module: no `.ml`, declared in dune's
  `(modules_without_implementation ...)`), and both files say
  `module type S = Foo_intf.S`. Edit the signature there.
- **A rebuild invalidates compiled example copies** ("inconsistent
  assumptions over library MEBI.loader"), and copied `examples/` trees
  carry stale `.vo` files. `bench/` scripts copy and clean for you; by
  hand, delete `*.vo*` and `*.glob` in the copy and recompile its
  dependencies.

**Measuring**
- **The last line printed does not say where the time went.** A message
  prints *before* its phase, and the next can come much later from another
  phase. Put timestamped markers on stderr (`Printf.eprintf ... %!`)
  before blaming a phase. Per-command times: `rocq compile -time`, as
  `bench/proofs.sh` does.
- `perf` is blocked on Jonah's machine (`perf_event_paranoid` = 4).
  Time the solver from inside the plugin instead: CPU timers around each
  step, kept locally as `notes/tools/solver-profile.diff` (results in
  `doc/future-work.md`, "The solver's own per-step cost").

**Shell**
- zsh does not word-split a command held in a variable (`$RC file` fails
  as "no such file"); put such loops in a bash script.
- `pkill -f <pattern>` can kill the calling shell when the pattern is in
  its own command line; kill by PID from `ps`.

## Other repo-specific notes

- **Result dumps are off by default** (since 2026-10-04; they were on during
  active development). For debugging or analysis you may turn them on in a
  scratch copy with `MeBi Config Output "DumpResults" True.`: each command
  then writes its FSMs and results as JSON into `./_dumps/` in the compile
  directory. Keep them off for timing runs (on `Test4` they cost ~20s and
  ~1GB a command) and delete large dumps afterwards. `_dumps/` is no longer
  refreshed by every run, so don't rely on an old one being current.
- `notes/` holds local planning notes and is excluded via
  `.git/info/exclude` (not `.gitignore`) — it will not appear in a fresh
  clone's `git status` as untracked, but is not shared upstream either.
- Adding, renaming or deleting an OCaml module means updating the
  relevant dune `(modules ...)` by hand: dune silently leaves out a file no
  list claims. `python3 scripts/check_module_lists.py` checks the lists
  against the source tree (CI runs it first); run it before committing
  such a change. (Until 2026-11-05 there were three lists, with
  `_CoqProject` and `src/mebi_plugin.mlpack` for `make`.)
- In-tree build artifacts (`.vo`, `.glob`, `.aux` left in `theories/`,
  `examples/` or `src/` by the old `make` build or by deleting parts of
  `_build` by hand)
  can make `dune build` **silently skip** `test/rocq/Test.v` -- no error, no
  output (seen 2026-10-05, when it was `theories/Test.v`). If `Test.v`'s
  messages are missing, check `_build/default/test/rocq/Test.vo` exists;
  `git clean -fX theories test src examples` and `dune clean` restore it. Verification in a fresh worktree
  (the `bench/` scripts) is not affected.
- Run `dune build` (capped, `-j` limited) *before* starting a proof-suite
  run, or a failed build costs the whole run.
- `test/rocq/Test.v` contains deliberate **known-wrong** tests (a `Fail`, or
  a pin that succeeds only while a limitation lasts), each with a comment
  saying so. When behaviour improves one of them starts failing, which is
  intended: turn it into a positive test. Check every `Fail` fails for the
  intended *reason*: cut the file at that line, drop the `Fail`, and read the
  error.
- `lib/utils`, `lib/terms`, `lib/model` and `lib/showable` must never gain a
  `rocq-runtime` dependency — they need to stay linkable from
  `test/tests.exe` without a Rocq runtime. That test binary failing to build
  is the tripwire if this is accidentally violated.
