# Assisted changes log

A running record of work done on this codebase with Claude (Claude Code), kept
so that both the direction of travel and the *kind* of each change stay legible
— to Jonah, and to anyone reviewing what was contributed and why.

Entries are chronological, oldest first, so the log reads as a narrative.

| tag | meaning |
| --- | --- |
| **Refactor** | Same behaviour, different structure. |
| **Bug fix** | Behaviour was wrong; now it isn't. |
| **Optimization** | Same behaviour, measurably less work. |
| **New feature** | Capability the *plugin* did not previously have. |
| **Tooling** | Build, dev environment or editor config. No plugin behaviour change. |
| **Docs** | Documentation only. |

**Standing policy on "New feature":** the working assumption is that this
project is a finished rough draft and what remains is refactoring, bug fixes and
optimizations. Anything that would add a *plugin capability* gets raised
explicitly and up front, before it is written, rather than appearing in this log
after the fact. Those raised and agreed so far are tagged **New feature** in
their own entries (B2's `MutualCofix Auto`, H2's saturation guard, ...).

**Scope of this log.** It covers work identifiable by the
`Co-Authored-By: Claude` trailer — 26 commits as of `df4f65c`, all from
2026-08-16 onward. The
preceding 1139 commits are the project's own history; the last of them,
`ccfc606` "implemented benchmarking for building lts graphs", dates from
2026-03-31, before the several-month pause. If any earlier assisted work exists
it left no trailer and is not identifiable here.

---

## 2026-08-16 — Rocq 9.2 port and reproducible toolchain

Branch `nix-setup`, seven commits, merged to `main` as `7d36087`.
Net: **18 files changed, +468 / −35**.

Context: the project had been untouched since March. Rocq had moved to 9.2 in
the meantime and the plugin no longer built — the break went unnoticed for
months because nothing in the repo recorded which Rocq the source required.
The goal of this session was to get it building again and make that state
reproducible.

- `e8007c0` — **Bug fix.** Ported the plugin to Rocq 9.2. `Option.IsNone` was removed from `rocq-runtime`'s clib, so `annotation.ml` raises its own `AnnotationIsNone`; `next_evar_name` lost its sigma argument and now returns `(Id.t * bool) option`; `extern_constr` requires `~flags`, passed as `PrintingFlags.current ()` to preserve behaviour; `Declare.Proof.by` takes an env first; `Tactics.cofix` moved to `FixTactics`.
- `b8df503` — **Bug fix** + **Tooling.** `COQBIN` probed for `coqtop`, which Rocq 9.x no longer ships, so `$(dir …)` collapsed to `/` and every invocation became `//rocq`. `rocq` now comes from `PATH` via the local opam switch, with `COQBIN` kept as an override. Added a `make dune` target, since the makefile build writes `g_mebi.ml`/`.vo`/`.glob` into the source tree and dune refuses to build over files it also generates.
- `57a19f4` — **Bug fix.** `test/saturation.ml{,i}` predates the model refactor and refers to modules that no longer exist. Nothing links it, so `dune build` never noticed — but `dune build @check` and merlin did, which is why it showed as errors in the editor.
- `7da83b6` — **Tooling.** Pinned the toolchain: `dune-project` gained real bounds (`rocq-core >= 9.2 < 9.3`, `yojson >= 3.0`) and declares `rocq-stdlib`, which `theories/` always required but nothing asked for; `rocq-mebi.opam.locked` pins ~100 packages exactly. Stopped ignoring `*.opam`, since `rocq-mebi.opam` is the input to `opam switch create .` and a fresh clone could not bootstrap without it. Added a nix flake covering three systems, with `OPAMNODEPEXTS=1` because opam otherwise probes the system package manager, finds no gmp or pkg-config on NixOS, and offers to run `nix-build`, aborting the bootstrap. Verified by bootstrapping an empty switch from the lockfile and building both paths in it.
- `ff54d5c` — **Bug fix** + **Tooling.** A local opam switch is identified by its full path, so the VS Code sandbox switch must be `${workspaceFolder}`; `${workspaceFolderBasename}` resolves to a global switch name that does not exist and the OCaml LSP never starts. Shared the setting rather than leaving it to be rediscovered.
- `5176ae1` — **Docs.** The README build section still described Coq 8.20 and `coq_makefile`. Replaced with the Rocq 9.2 toolchain, the two build paths and why switching needs `make dune`, the faster inner loops, and the from-clone bootstrap.
- `5566c6a` — **Docs.** Recorded a structural cleanup backlog in `TODO.md`: repo size dominated by `paper/`, no CI, no LICENSE, overlapping module lists, assorted stale files.

**Session tally:** Bug fix 4 · Tooling 3 · Docs 2 · Refactor 0 · Optimization 0 ·
**New feature 0.**

---

## 2026-08-17 — Functor-layer refactor

Branch `refactor/functor-layer`, three commits off `main` (`7d36087`).
Net: **148 files changed, +2603 / −3430** — the codebase got smaller.

Context: `lib/model/state.ml`'s `Make(Log)(Base)` was representative of a
pattern across ~60 files where every module took a `Log : Logger.S` functor
parameter, which blocked running the model from plain OCaml tests. Full analysis
in `~/.claude/plans/i-d-like-some-advice-snappy-bentley.md`.

### `99b0501` — Logger and Rocq_context become values, not functor parameters

*145 files, +2353 / −3168*

- **Refactor** — `Logger.S`/`Make`/`ReMake` replaced by a mutable sink installed once at plugin load (`src/rocq_output.ml`, the new home of `Pp`/`Feedback`). The `Log` parameter is gone from all ~60 functors. The signature had no abstract type — every member returned `unit` — so the functor imposed a parameter everywhere and bought nothing at the type level.
- **Refactor** — `Rocq_context.S` (a module) became `Rocq_context.source = unit -> t` (a value), removing the parameter from six functors and letting the 14 per-invocation `Wrapper.make ()` calls in `g_mebi.mlg` collapse to one shared instance.
- **Refactor** — `Info.Make` now takes its constructor-bindings parameter as a `Json.S` instead of a `Constructor_bindings.S`. It is only stored and serialised there, and that one parameter was what forced `lib/model` to depend on `rocq_tools`. **`lib/utils`, `lib/terms` and `lib/model` now declare no Rocq dependency at all.**
- **Refactor** — `lib/model`'s uses of Rocq's clib `Option` (`cata`/`default`/`has_some`) made explicit as `Stdlib.Option`. The two builds disagree about which `Option` is in scope: dune gives `lib/model` the stdlib one, `make` puts Rocq's in scope via `_CoqProject`'s `-I` flags.
- **Bug fix** — `src/mebi_plugin.mlpack` listed `Benchmarking` twice (already noted in `TODO.md`).
- **Bug fix** — deleted `Rocq_context.update`, which could never have worked: `Make.get` allocated a fresh `ref` per call, so it wrote into a value discarded immediately. Nothing called it.

*Verified:* `theories/` output byte-identical to `main` apart from
non-deterministic benchmark timings; proof suites unchanged (baseline below).

### `c078308` — pure-OCaml model tests

*3 files, +227 / −258*

- **New — test infrastructure, not a plugin feature.** `test/tests.exe` links `rocq-mebi.model` and nothing Rocq-related. Nine checks over `FSM.of_lts`, saturation, minimization, bisimilarity and JSON. This is the payoff of the Rocq-freedom work above, and doubles as a tripwire: if a Rocq dependency creeps back into those three libraries, the target stops building. Flagged as *new* because it is net-new code, but it adds no capability to the plugin itself.
- **Bug fix** — dropped `(public_name rocq-mebi.tests)`, which made `opam install .` place a do-nothing binary in the switch's `bin/` (noted in `TODO.md`). The previous `tests.ml` was commented out end to end and referenced a pre-refactor API, so it was replaced rather than revived.

### `6f94748` — proof completion detected in the iteration that closes it

*1 file, +23 / −4*

- **Bug fix** — `Proof_solver.solve` left completion detection to the next call to `step`, spending a whole iteration noticing an already-closed proof. With `bound` one below the true requirement the proof still closed and `Qed` succeeded, but the loop exited on the bound rather than on `NothingToDo`, so `statem` was never set to `Done` and the run reported "Unsolved". This is why every `MeBi Sim Solve N` in `examples/` needed `N` one greater than the work required. Reported counts were always correct; only the classification and the minimum bound were wrong, and existing bounds all still hold.

### Corrections to the analysis, made during the work

Recorded because they changed conclusions, not just wording:

- The claim that `lib/model` and `lib/terms` contained *zero* Rocq references was wrong — they used Rocq's `Option`, which shadows the stdlib module of the same name and so slipped past a grep for `EConstr|Names|Pp|Feedback|…`.
- `lib/utils` had a fourth Rocq coupling beyond `Feedback`/`Pp`: `Utils.FileWriter.get_loc` called `Loc.get_current_command_loc`. Now an installable hook with the pre-existing `"Unknown Location"` fallback.
- The sharing-constraint burden in `model.mli` was attributed to `base` being abstract. Re-measured: of 80 constraint occurrences, 71 are inter-component sharing and only 9 involve `base`/`tree`/`trees`. The cause is one-functor-per-file, not the abstract element type.

**Session tally:** Refactor 4 · Bug fix 4 · Optimization 0 ·
**New feature 0** (one new *test* binary, no new plugin capability).

Net public API surface shrank: `Logger.S` (as a functor parameter),
`Output.Mode`, `Output.Config`, `Api.make_logger` and
`Rocq_context.S`/`Make`/`Default`/`MakeFromGoal` were removed;
`Logger.set_sink`/`quiet`/`Scoped`, `Rocq_context.source`/`global`/`of_goal`,
`Wrapper.get` and `Utils.FileWriter.set_loc_provider` replace them.

---

## 2026-08-18 — Encoding tables unshared, contexts fixed per instance

Branch `refactor/functor-layer`, one commit (`328a26f`) on top of `6f94748`.
Net: **7 files changed, +86 / −56**.

Context: backing out the encoding-table sharing from `99b0501`, the first item
under "Outstanding" below. Working note in `notes/1-revert-shared-encoding-table.md`.
Tracing it before implementing turned one item into three: the note's fix as
written would have left the hazard it was aimed at, and reintroduced a worse one
that predates the branch. All three are in the one commit because they are the
same design tension — one table with a moving context, versus two tables sharing
one counter — and (B) is only reachable because of (C).

- **Refactor** — (C) `Proof_solver_wrapper.Make` drops its `M` parameter and builds its own `Rocq_monad_utils` again. The sharing was justified on the grounds that a fresh `Bi_encoding` per proof step meant nothing from a previous step could be found; that is not where the lookups that matter go. `ReModel.state`/`label` resolve against `W.M`, the command-time table, before and after. The per-step table only ever backed `Iter`'s own `encode`/`econstr_compare`/`EConstrSet`, which are per-step by construction. Measured effect of the sharing: none.
- **Bug fix** — (A) `Rocq_monad.run` loses `?ctx` and reads its own instance's context, as it did pre-`99b0501` via `Ctx.get ()`. `Bi_encoding.set_ctx` becomes install-once, called by `Proof_solver_wrapper.Make` with the goal. Overriding a `run` default was never sufficient: `encode`, `fstring` and `Rocq_monad_utils.get_encoding` call `run` themselves and cannot pass a `~ctx`, so they defaulted to `Rocq_context.global` — including via `econstr_compare`, hence `EConstrSet`. A table can hash an entry under one sigma and look it up under another, and that was reachable both before and after (C) alone. The `run` override in `proof_solver_wrapper.ml` was `?ctx`'s only caller, so it disappears with it.
- **Bug fix** — (B) `Bi_encoding.initialize` allocates the maps without calling `Enc.reset`. **Pre-existing, not introduced by `99b0501`** — that commit removed the reachable path by accident, and a literal revert would have restored it. `Enc` is one counter shared by every `Bi_encoding` instance, and a per-step table is a fresh instance each step, so its first `run` put the counter back to `0` while the command-time table already held encodings `0..N-1`. A later `M.encode` of a term not already in that table — reachable from `M.exists_eq` in `Proof_solver_theory` — is then handed a live encoding, and `B.add` shadows the model's binding for it: false positives in `M.econstr_eq`, wrong terms out of `Decode`. Only an explicit `~reset_encoding:true` resets the counter now, which is what every command call site passes.

*Verified:* per file, in emission order, against the commit's parent. Each
`PluginProofs.v` built as its own `make -j1` target — under `make -j$(nproc)`
the concurrent `rocq` processes interleave line by line and no count can be tied
to a file, which an aggregate comparison hides.

| file | before | after |
| --- | --- | --- |
| `Proc/Test1` | 114 105 106 109 22 21 | 114 105 106 109 22 21 |
| `Proc/Test2` | 446 278 299 194 446 182 | 446 278 299 194 446 182 |
| `CADP/Size1/MutualExclusion` | 268 396 | 268 396 |
| `CADP/Size1/Glued` | 268 396 | 268 396 |
| `CADP/Size1/Glued/MutualExclusion` | *(none — fails at `Example`)* | *(none)* |

Exit codes match; full per-file logs identical once build lines are stripped.
`dune build @check`, `dune build`, `make` and `dune exec test/tests.exe` (9/9)
all clean.

**Session tally:** Bug fix 2 · Refactor 1 · Optimization 0 · Docs 0 ·
**New feature 0.**

Public API surface: `Rocq_monad.S.run` loses its `?ctx` argument;
`Bi_encoding.S` gains `current_ctx` and re-specifies `set_ctx` as install-once;
`Proof_solver_wrapper.Make` loses its `M` parameter.

Note that (B) is reasoned from the code, not observed. The mechanism is
concrete, but none of the five suites trips it — which is why the counts do not
move. It is cheap insurance, not a fix with a reproducer behind it.

---

## 2026-09-27 — Model component cluster collapsed, renamed, unified with Showable

Branch `refactor/model-components`, four commits off `main` (`7d36087`).
Net: **82 files changed, +2205 / −2998** — the codebase got smaller again.

Context: Jonah returned to the project after another multi-month pause,
wanting the codebase cleaned up and restructured rather than extended.
Working assumption for this and future sessions, now recorded in
`CLAUDE.md`: the plugin's core functionality is complete; what remains is
refactoring, bug fixes, restructuring and optimization. Before any of that,
all uncommitted and unpushed work (this branch, 9 commits, plus an
in-progress uncommitted experiment) was pushed to Jonah's personal fork
(`github.com/thecathe/mebi_plugin`) so he could keep working on it without
pressure ahead of an eventual PR back to `dcastrop/mebi_plugin`; `origin`
stays pointed at `dcastrop/mebi_plugin` for that PR.

The uncommitted experiment — a partial attempt to rebuild `State` on top of
membranes-style (`~/Documents/git/thecathe/membranes`) generic `Set`/`Map`
abstractions — was syntactically broken (an incomplete signature in
`state/set_.ml`) and would have produced a second, colliding `State` module
alongside the existing one. It was stashed aside rather than committed or
deleted, then superseded entirely by the work below, which targets the
actual diagnosed bottleneck (`notes/3-collapse-model-component-cluster.md`)
rather than a wholesale port of the other project's abstractions.

- `a6eec52` — **Docs.** Added `CLAUDE.md`, making the "refactoring only"
  working assumption, this log's practice, and the `PluginProofs.v`
  verification procedure discoverable by anyone, not just prior-session
  memory.
- `16bbe37` — **Refactor.** Collapsed all 17 model components (`State`,
  `Label`, `Action`, `Edge`, ...) from 17 separate files, each its own
  functor, into nested modules inside one `Components.Make` functor
  (`lib/model/components.ml`) — exactly the change `notes/3` had already
  sized and designed. Nested modules see each other directly, so the
  sharing constraints that used to relate one component's functor
  parameters to another's output are gone: `model.mli` needed on the order
  of ten, not eighty. Every algorithm functor (`LTS`, `FSM`, `Saturation`,
  `Minimization`, `Bisimilarity`, plus `Saturation`'s private `WIP`/`Trace`/
  `Traces` helpers) now takes a single `Components.S` argument (plus each
  other where needed) instead of 5–13 individually-constrained ones. No
  `src/` changes — every module path is preserved.
- `6e436dd` — **Refactor.** Renamed each component's Set/Map/Pair to a
  submodule of its element type — `States` → `State.Set`, `Labels` →
  `Label.Set`, `Actions`/`ActionMap`/`ActionPair`/`ActionPairs` →
  `Action.Set`/`Action.Map`/`Action.Pair`/`Action.Pair.Set`, and so on —
  so `Model.Action.Map.update` reads as what it is instead of requiring the
  reader to already know `Actions` and `ActionMap` are related. `EdgeMap`
  and `Partition` deliberately stay standalone: `EdgeMap` and `Action.Map`
  are mutually dependent (each stores the other's value type as data), which
  can't be expressed if either is nested inside its own key type's module —
  doing so would need `State`'s declaration to come both before and after
  several other components, a cycle ordinary module signatures can't
  express. Mechanical but wide: every `src/` call site referencing an old
  flat name needed updating (`proof_solver_step.ml`, `decoder.ml`,
  `wrapper.ml`/`.mli`, `results.ml`/`.mli`, `graph_extract_lts.ml`,
  `proof_solver_theory.ml`, `_examples.ml`, `test/`). `graph.ml`/
  `graph_builder.ml`/`graph_type.ml` were deliberately left alone — their
  own `States`/`Actions`/`Transitions` are a separate, unrelated module
  hierarchy that happens to share these names.
- `e037c18` — **Refactor.** Unified the `lib/showable` port of Jonah's
  membranes-style `Ordered`/`Set`/`Map` abstractions (committed in
  `7249d40`, previously unused) with the existing JSON-dump mechanism
  (`lib/utils/json.ml`) into one `Thing.Make` (new `lib/showable/thing.ml`):
  a component supplies `{name; json; equal; compare}` once and gets
  `pp`/`show`/`equal`/`compare` (from `Showable`) and `json`/`to_string`/
  `log`/`write` (from the existing dump mechanism) together, instead of a
  separate hand-written `equal`/`compare` plus a `Json.Thing.Make` call.
  `pp`/`show` are derived from the existing `json` function, not
  independently written, so nothing gains a second, divergent notion of
  "show". Applied to every component with a natural `Ordered` shape —
  `State`, `Label`, `Note`, `Annotation`, `Transition`, `Action`, `Edge`,
  `ActionPair`, their `.Set` companions, and `Partition` — leaving
  `ActionMap`/`EdgeMap` untouched (Hashtbl-based; `lib/showable` has no
  Hashtbl equivalent). **Flagged before writing, per standing policy:**
  `ActionPair` gains an `equal` it never had (`Thing.Make` requires one);
  defined to agree with the existing `compare`, since nothing else can rely
  on it — a plumbing detail, not a plugin capability. Surfaced pre-existing,
  unrelated breakage: `lib/showable`/`lib/json` were never added to
  `_CoqProject` when introduced the day before, so `make` had silently
  never compiled either (only `dune build` had); fixing that then surfaced
  a second latent issue, `lib/showable/type_.ml`'s `[@@deriving show, eq]`
  never actually running under `make` either, since `_CoqProject` has no
  equivalent of dune's per-library `(preprocess (pps ...))` — replaced with
  hand-written `pp`/`show`/`equal` for the five small presets affected,
  removing the `ppx_deriving` dependency from `lib/showable` entirely.

**Verification**, per-file, run twice from a clean slate (after the collapse,
and again after the rename+unification):

| file | before | after |
| --- | --- | --- |
| `Proc/Test1` | 114 105 106 109 22 21 | 114 105 106 109 22 21 |
| `Proc/Test2` | 446 278 299 194 446 182 | 446 278 299 194 446 182 |
| `CADP/Size1/MutualExclusion` | 268 396 | 268 396 |
| `CADP/Size1/Glued` | 268 396 | 268 396 |
| `CADP/Size1/Glued/MutualExclusion` | *(none — fails at `Example`)* | *(none)* |

`dune exec test/tests.exe` (9/9) after each of the four commits. Additionally,
for the `Thing.Make` unification specifically (the step most likely to touch
JSON dump *format*): a byte-for-byte diff of `State`/`Label`/`Transition`/
`Action`/`Edge`/`FSM`/`Info` JSON output between this commit and the previous
one, built in a throwaway `git worktree` — identical.

**Session tally:** Refactor 3 · Docs 1 · Bug fix 0 · Optimization 0 ·
**New feature 0.**

---

## 2026-09-27 — Review; dead-file cleanup and packaging docs

Two commits off `main`. Net: 14 files removed, `_CoqProject`/`TODO.md`/
`dune-project`/`rocq-mebi.opam`/`README.md` updated.

Context: asked for a full review of the codebase, focused on what stands
between it and being a usable Rocq plugin, and on finishing remaining work.
Three parallel research passes (build/packaging/vernacular interface;
`lib/model` and friends post-refactor; proof solver and test coverage)
turned up a prioritized list of findings; this session actioned the two
cheapest/highest-value tiers (dead-file cleanup, packaging/docs) and left
the rest — proof-solver correctness gaps, build-list consistency beyond
what was hit here, `ocamlformat` drift on `components.ml`, deeper test
coverage — as backlog, recorded below.

- **Tooling.** Removed 13 dead/orphaned files confirmed unreferenced by any
  build description (`_CoqProject`, every `dune`'s `(modules ...)`,
  `src/mebi_plugin.mlpack`): `src/_command.{ml,mli}`,
  `src/_examples.{ml,mli}`, `src/_mebi_help.{ml,mli}` (the underscore-file
  convention `TODO.md` already flagged as needing a decision),
  `lib/model/algorithms/similarity.{ml,mli}` (0-byte placeholders),
  `lib/utils/writer.ml`/`.mli`, `test/saturation.{ml,mli}` (already
  excluded from `test/dune`, per `57a19f4`), `test/coqplugin/Proc.v` (uses
  command syntax that predates the current `g_mebi.mlg` grammar), and
  `examples/CADP_v2.v` (an unreferenced fork of `examples/CADP.v`). Also
  cleared stray `.cmi`/`.cmx`/etc. build artifacts left in-tree for
  `wrapper_results` (no source file at all), `writer`, and old
  `minimize`/`saturate` filenames, plus three empty untracked directories
  under `lib/term/` (rename debris from `term` → `terms`). Deleting
  `similarity.{ml,mli}` broke `make dune` — `_CoqProject` still listed
  them on lines 172-173 even though `lib/model/algorithms/dune`'s
  `(modules ...)` had already dropped them — a live instance of exactly
  the "three independently-maintained module lists can drift silently"
  risk `TODO.md` already tracked in the abstract. Fixed by removing those
  two `_CoqProject` lines too. Verified with `dune build`,
  `dune exec test/tests.exe` (9/9), and a full `make dune` round-trip.
- **Docs.** `dune-project`'s package metadata was unedited dune-init
  boilerplate (`synopsis "A short synopsis"`, `description "A longer
  description"`, `tags (topics "to describe" your project)`), which flowed
  straight into the generated `rocq-mebi.opam`. Filled in real values and
  regenerated the opam file. Left `(license LICENSE)` commented out —
  that's a decision for @dcastrop, who owns the upstream repo, not
  something to pick unilaterally; noted in `TODO.md`.
- **Docs.** `README.md`'s only usage example was `MeBi Run LTS <ident>.`,
  which omits the mandatory `Using <reference>` clause every real
  `MeBi Run *` command requires, and never mentioned `Bisim`/`Merge`/
  `Minimize`/`Saturate`/`Benchmark`, `MeBi Config *`, or `MeBi Sim *` at
  all. Replaced the "Scratchpad" section with a full `Usage` section
  covering the whole command surface (grammar drawn from `src/g_mebi.mlg`,
  examples drawn from `theories/Test.v` and
  `examples/Bisimilarity/Proc/Test1/PluginProofs.v`). Also replaced the
  README's own `TODO` section, which described core LTS-reading/
  bisimilarity functionality as unbuilt — stale relative to what's
  actually implemented — with an accurate one-paragraph status pointing at
  `TODO.md`; and corrected the "Running tests" section, which still
  described `test/tests.ml` as commented out end-to-end (it's a real,
  passing 9-assertion suite as of `c078308`, 2026-08-17) and had no
  mention of the `PluginProofs.v` suite being the only end-to-end
  proof-solver coverage.

**Verification:** `dune build` (clean, both before and after the
`_CoqProject` fix), `dune exec test/tests.exe` (9/9), `make dune` full
round-trip (`rm -f Makefile.rocq Makefile.rocq.conf && make -j$(nproc)`,
confirmed error-free, then `make dune` to restore the dune-buildable
state). The `examples/Bisimilarity/**/PluginProofs.v` proof-solver suite
was not re-run this session — nothing touched `lib/model` or
`src/proof_solver*` behaviour, only build-list entries and docs.

**Session tally:** Tooling 1 · Docs 2 · Refactor 0 · Bug fix 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — Fix: CADP/Glued/MutualExclusion compose/create rename fallout

Context: earlier the same day, this session's review flagged
`examples/Bisimilarity/CADP/Size1/Glued/MutualExclusion/PluginProofs.v`'s
"The reference compose was not found" failure as likely stale example code
rather than a Rocq 9.2 regression, but left it unfixed as out of scope for
that pass. Jonah recalled defining `compose`/`create` for the CADP terms
and suspected a rename during the 9.2 port; asked for it to be traced back
through history before trusting either the fix or the old baseline.

- **Bug fix.** `compose (create N b)` was folded into `composition_create N
  b` in `f850375` (2026-03-24, "discovered bug in CADP write_next, memory
  out of bounds"), touching `examples/CADP.v`/`examples/CADP_Glued.v` — but
  the last edit to this specific file (`87eec6f`, 2026-03-19) predates that
  commit by five days, so it was never updated and has been broken ever
  since. The rename also shifted the counting convention: old `create N b`
  produced exactly `N` processes; new `sys_create N b` (which
  `composition_create` wraps) recurses down to `0` inclusive, producing
  `N + 1`. A second, independent shift did the same thing to
  `make_spec_pid` in `93dda66` (2026-03-26, "debugging CADP size 2") — its
  base case changed from `Nil` (0 pids) to `Pid 0 Nil` (1 pid), so
  `make_spec N` also went from `N` pids to `N + 1`. Reconstructing the
  historically-validated (82/64-iteration) 1-process test in the current
  codebase's conventions needed *both* arguments dropped by one:
  `compose (create 1 Protocol.P)` → `composition_create 0 Protocol.P`
  (matching `examples/Bisimilarity/CADP/Size1/Terms.v`'s own `c1`), and
  `make_spec 1` → `make_spec 0`. Verified with a targeted `make -j1` build:
  `wsim_bigstep` solves in 81 iterations (bound 82, matching the file's own
  "Iteration History" comment almost exactly), `wsim_spec_lts` in 63
  (bound 64).
- Blind alley, recorded for whoever next touches this file: renaming only
  `compose`/`create` → `composition_create` without the index shift (i.e.
  `composition_create 1 Protocol.P`, keeping `make_spec 1`) still
  type-checks and is internally self-consistent with the *current*
  codebase's conventions (both sides use "N" to mean "N+1
  processes/pids") — so it isn't a compile error — but it's a 2-process
  mutual-exclusion instance, not the 1-process one this file has always
  tested, and its proof search ran 84+ minutes of CPU time without
  converging before being stopped. Not confirmed whether it would
  eventually solve or is a genuine second proof-explosion case; not
  investigated further since the 1-process version is the intended test.
- Also resolved, as a byproduct of debugging this with a clean `-j1`
  rebuild: the "baseline discrepancy" flagged in this morning's review
  entry (checked-in bounds of `267`/`395` for `CADP/Size1/MutualExclusion`
  and `CADP/Size1/Glued` vs. a documented baseline of `268`/`396`) is not a
  real discrepancy. `Proof_solver.solve`'s loop guard
  (`src/proof_solver.ml:179`, stepping again on `Int.compare n bound = 0`
  and only stopping once `n > bound`) permits one solver step beyond the
  nominal bound before giving up, so `Solve 267` can genuinely report
  "Solved after 268 iterations" and still succeed. Confirmed directly: a
  clean `make -j1` rebuild of both files reproduces 268/396 exactly,
  matching the documented baseline.

**Verification:** `make -j1` targeted rebuilds (not `-j$(nproc)`, whose
interleaved output cannot be reliably attributed to one file/proof —
confirmed the hard way mid-session, after initially misreading an
interleaved run as showing this file's proof exploring for 84+ minutes,
which was actually a different, wrongly-indexed instance of the problem)
of `CADP/Size1/Glued/MutualExclusion/PluginProofs.v` (fixed: 81/63, bound
82/64), `CADP/Size1/MutualExclusion/PluginProofs.v` and
`CADP/Size1/Glued/PluginProofs.v` (268/396 each, confirming the existing
baseline is current and correct). `_CoqProject` restored to its original
commented-out state and `make dune` run afterward. `CLAUDE.md`'s baseline
table updated to the full 18-value set (previously 15, missing this file's
two values plus a stray duplicate omission) and its "known unrelated
failure" note removed, now that it's fixed.

**Session tally:** Bug fix 1 · Docs 1 (folded into the same commit) ·
Refactor 0 · Tooling 0 · Optimization 0 · **New feature 0.**

---

## 2026-09-27 — Two more Tier 2 review items: Test3 duplicate names, Test4's missing file

Continuing the same day's backlog after the CADP/Glued/MutualExclusion fix.

- **Bug fix.** `examples/Bisimilarity/Proc/Test3/PluginProofs.v` declared
  `wsim_rp`/`wsim_pr` twice each — the `r`/`s` and `s`/`r` pairs (dividers
  `ProofTest.rs`/`ProofTest.sr`) were copy-pasted from the `r`/`p` and
  `p`/`r` pairs above them without updating the `Example` name, a real
  Rocq identifier collision that would reject the file the moment it's
  compiled. Renamed to `wsim_rs`/`wsim_sr`, matching every other pair's
  `wsim_<first>_<second>` convention already used in the file. **Not**
  verified with a full `make` build: this file needs `MeBi Sim Solve
  100000` per example, and a `-j1` run with the expensive `Solve` calls
  swapped for `admit` (to check elaboration/naming only, skipping the
  actual proof search) still hadn't gotten past the *first* live example
  after 180 seconds — the `Layered` term elaboration inside `MeBi Sim
  Begin` is itself expensive here, independent of proof search, matching
  the file's own "proof explosion" tag. The fix is a straightforward Rocq
  identifier-uniqueness correction (two declarations can't legally share a
  name in the same scope regardless of what they prove), so it was applied
  without a build-verified round-trip; flagging that explicitly rather
  than silently skipping the usual verification step.
- **Docs.** `_CoqProject:53` commented out
  `examples/Bisimilarity/Proc/Test4/PluginProofs.v` tagged `### TODO: proof
  explosion`, but `git log --all` shows no commit ever created this file —
  unlike Test1–3, a `PluginProofs.v` for Test4 was never written, so the
  tag was actively misleading (it implies a file that exists and is known
  slow, not one that was never authored). Per Jonah: it was deliberately
  skipped, not forgotten — `Test3`'s own `PluginProofs.v` was already
  hitting proof explosion (`Solve 100000`, some examples unfinished even at
  500000–1000000), so a `Test4` version was expected to be worse still and
  not worth writing. Writing a real `PluginProofs.v` for Test4 would mean
  originating new example/proof content from scratch, which is out of
  scope for a quick fix and was not attempted here — updated the
  `_CoqProject` comment to record the actual reason instead.

**Verification:** `dune build`, `dune exec test/tests.exe` (9/9), `make
dune` round-trip. The Test3 fix specifically was not proof-suite-verified,
per the note above — its correctness rests on it being a mechanical Rocq
identifier rename, not on a completed build.

**Session tally:** Bug fix 1 · Docs 1 · Refactor 0 · Tooling 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — ocamlformat the functor-collapse drift

- **Tooling.** `lib/model/components.ml`, `model.mli`,
  `wip/wip_annotation.ml`/`.mli` and `algorithms/saturation.ml`/`.mli` had
  never been run through `ocamlformat` since the three-commit
  functor-collapse/rename/`Thing`-unification refactor landed on this
  branch — `dune build @lib/model/fmt` reported a diff for all six,
  `components.ml`'s alone touching ~1800 of its 1565 lines (everything
  below `State` inside the `Impl` submodule sat one indent level too
  shallow). Ran `dune build @lib/model/fmt --auto-promote`; purely
  whitespace/line-wrapping, no AST change.
- While scoping this, found unrelated pre-existing `@fmt` drift outside
  `lib/model` — `lib/rocq_tools/rocq_monad.mli`, `rocq_monad_utils.ml`,
  `theories.ml`; `lib/showable/thing.ml`; `src/proof_solver_wrapper.ml`,
  `proof_solver_step.ml`, `proof_solver.ml`, `graph_extract_lts.ml`,
  `graph_type.ml`; `test/tests.ml`. Not part of the functor-collapse
  refactor this branch is otherwise about, and not touched here — left
  as a separate backlog item (see Outstanding).

**Verification:** `dune build @lib/model/fmt` clean afterward. `dune build`
and `dune exec test/tests.exe` (9/9) both pass. Since this touches files
the proof solver reads, also ran the full five-file proof-solver baseline
(`-j1` per file, per `CLAUDE.md`'s procedure) rather than relying on
`tests.exe` alone: `Proc/Test1` 114/105/106/109/22/21, `Proc/Test2`
446/278/299/194/446/182, `CADP/Size1/MutualExclusion` 268/396,
`CADP/Size1/Glued` 268/396, `CADP/Size1/Glued/MutualExclusion` 81/63 — all
18 counts match the current baseline exactly, confirming the reformat is
behaviour-preserving. `_CoqProject` restored and `make dune` run
afterward.

**Session tally:** Tooling 1 · Bug fix 0 · Docs 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — Proof solver's two self-flagged open design questions

Two spots in `src/proof_solver*` carried their own doc comments flagging
open, unresolved questions (`proof_solver_wrapper.ml:88`,
`proof_solver_step.ml:205`). Traced both to a conclusion rather than
leaving them open indefinitely.

- **Docs.** `proof_solver_wrapper.ml`'s `EConstrSet` comment flagged
  that, since each proof step gets a new `env`/`sigma`, comparing
  `EConstr.t` values across steps might not be meaningful — "this needs
  to be investigated." Audited every call site: `Proof_solver.step`
  (`proof_solver.ml:94-107`) creates a brand-new `PStep` module (and with
  it a fresh `Iter`/`EConstrSet`) via `(val make gl)` on *every* call, and
  discards the whole module when it returns; no persistent state type
  (`Proof_solver_statem.S`, `Proof_solver.t`) ever stores an
  `EConstrSet.t`; and the only two actual uses
  (`Proof_solver_tactics.collect_component_econstrs`/`try_unfold_any`)
  build, consume and discard one within a single function call. The
  invariant holds structurally, not by convention — there's no code path
  that could compare across steps even by accident. Rewrote the comment
  to record this as a settled, audited invariant instead of an open
  question, with a note for future maintainers on what would need
  re-checking if a new cross-step-persisting use were ever added.
- **Bug fix.** `proof_solver_step.ml`'s `transition` function looks up the
  specific transition `from --label--> goto`; when more than one distinct
  `Action.t` (differing in `annotation`/`trees` — different weak-transition
  witnesses reaching the same destination under the same label) matched,
  it gave up (`raise CouldNotFind_Transition`) rather than picking one.
  `lib/model/components.ml`'s `ActionPairs.shortest_annotation`/
  `ActionPair.shorter_annotation` already exist for exactly this
  "pick the best of several candidates" reduction, and are already used
  for the structurally identical situation two hundred lines later in the
  same file (`try_get_visible_transition`, `proof_solver_step.ml:589`),
  with the rationale spelled out inline there: "get the pair with the
  shortest annotation (less steps to do)." Applied the same fold here
  instead of raising, and merged what used to be a separate
  single-candidate branch into the general case, since folding over an
  empty tail is a no-op — the fix is also a small simplification.

**Verification:** `dune build`, `dune exec test/tests.exe` (9/9). Since the
`transition` fix changes proof-solver behaviour, also ran the full
five-file baseline (`-j1` per file): all 18 counts match exactly, unchanged
— but worth being explicit that this means **none of the five cheap tests
actually exercise the "multiple actionpairs" branch this fix touches**; the
baseline confirms no regression, not that the new code path has been
positively exercised. That would need either a hand-built minimal example
that genuinely produces saturation-derived transition ambiguity, or finding
one already present in the more expensive `Test3`/`Test4`/`Size2` examples
— not attempted this session. `_CoqProject` restored and `make dune` run
afterward. Tracked in `TODO.md`'s "Optimizations & Fixes" section so it
doesn't get lost.

**Session tally:** Bug fix 1 · Docs 1 · Refactor 0 · Tooling 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — CI, and closing out the rest of backlog item C6

Continuing down `notes/4-post-review-backlog.md`'s suggested order (C1, then
C6) after Jonah returned to the project wanting it back to a usable state.

- **Tooling.** Added `.github/workflows/ci.yml`: `nix develop` to provide
  the system layer, then `opam switch create . --locked --deps-only` (cached
  on `rocq-mebi.opam.locked`'s hash, since building OCaml/Rocq from source
  takes a while), `dune build`, `dune exec test/tests.exe`, and `make
  -j$(nproc)` — exactly the bootstrap path `TODO.md` proposed and the one
  `ASSISTED-CHANGES.md`'s 2026-08-16 entry verified by hand, now automated.
  Deliberately does not enable the `PluginProofs.v` proof-search suite (see
  CLAUDE.md); `_CoqProject`'s default subset is what `make` builds.
- **Tooling.** Dropped a git stash entry, `WIP: broken membranes-style state
  experiment (superseded by cluster-collapse plan)` — confirmed superseded
  by the now-completed `16bbe37` cluster collapse, per the 2026-09-27
  cluster-collapse entry above.
- **Tooling.** Removed `.gitignore`'s `src/commandOLDunify.ml` line —
  confirmed via `git log --all` that the file is gone from the working tree
  (last existed before this log's Rocq-9.2-port-era history) and the entry
  was dead weight, matching what `TODO.md` already flagged.
- Checked the rest of `TODO.md`'s C6 "stale detritus" item and found it
  already resolved or not actually a problem, so left alone: the leftover
  `CoqMakeFile`/`CoqMakeFile.conf`/`.CoqMakeFile.d` files it described are
  not present in this working tree (already gitignored, and apparently
  cleared by a later `make dune` run); `doc/index.html` redirecting into the
  gitignored `_build/default/_doc/_html/` is dune's standard `@doc`/odoc
  local-viewer pattern, not stale detritus — no change made.

**Verification:** `dune build`, `dune exec test/tests.exe` (9/9), and
`make -j$(nproc)` all run clean locally (the same commands the new CI job
runs), followed by `make dune` to restore the dune-buildable state.
`git status` clean afterward apart from the intended `.gitignore` edit and
new `.github/` directory. The workflow itself has not yet been exercised by
GitHub Actions — that only happens once it's pushed.

**Session tally:** Tooling 3 · Bug fix 0 · Docs 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — C7: checked the three underscore-prefixed `.v` examples

Backlog item C7 explicitly withheld a verdict on
`examples/**/_mutual_exclusion.v`, `_no_starvation.v`, `_nat_streams.v` —
they resemble the underscore-prefixed OCaml dead files removed earlier this
week (`5743b46`), but each needed its own check rather than a batch delete.
Traced each file's full rename/edit history with `git log --all --follow`.

- **Tooling.** Deleted `examples/Bisimilarity/CADP/Properties/_mutual_exclusion.v`
  (and its stale, already-gitignored `.mutual_exclusion.aux`). Confirmed
  dead: the same commit that underscore-prefixed it, `97e668c` "reorganizing
  and preparing to rework mutual exclusion", is immediately followed by
  `3cc6f10`/`37d997a`/`93dda66`, which wrote the current, actively-built
  `MutualExclusion.v` from scratch as that rework — the commit's own message
  documents the supersession, and `MutualExclusion.v` is what every CADP
  mutual-exclusion `PluginProofs.v` (including this session's earlier
  `Glued/MutualExclusion` fix) actually exercises.
- **Docs.** Left `_no_starvation.v` and `_nat_streams.v` alone — neither is
  dead. `_no_starvation.v` was underscore-prefixed by the same reorg commit,
  but nothing ever replaced it: `TODO.md` still lists "no starvation" as
  open, and this is the only extant attempt at it. `_nat_streams.v` has
  eleven of its own commits ("finished nat streams examples", "finished ltac
  for first case", "parity plus odd trans lemma", ...) predating and
  unrelated to the mutual-exclusion rework that happened to sweep it up in
  the same renaming commit — a real, fairly developed piece of Jonah's own
  work, not draft filler, just currently unwired from any build target.
- **Docs.** `_CoqProject`'s commented-out lines for these files pointed at
  stale paths from before the March 2026 reorg
  (`examples/properties/mutual_exclusion.v`,
  `examples/properties/no_starvation.v`,
  `examples/Bisimilarity/nat_streams.v` — none of which exist).
  Removed the now-meaningless `mutual_exclusion.v` line entirely and
  corrected the other two to their real current paths, each with a note on
  why it's commented out (matches the verdicts above).

**Verification:** `dune build`, `make -j$(nproc)` (neither ever referenced
the deleted file — the stale `_CoqProject` comment line pointed elsewhere
even before this fix), `make dune` round-trip. `dune exec test/tests.exe`
not affected (`examples/` is outside its scope).

**Session tally:** Tooling 1 · Docs 2 · Bug fix 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — Formatting debt outside `lib/model`, non-`proof_solver*` half

Backlog item D. The `proof_solver*` half is deliberately left alone here —
formatting those files would need the full five-file `PluginProofs.v`
baseline re-run, budgeted separately.

- **Tooling.** `dune build @lib/rocq_tools/fmt @lib/showable/fmt
  @test/fmt --auto-promote`: `lib/rocq_tools/rocq_monad.mli`,
  `lib/showable/thing.ml`, `test/tests.ml`. Purely whitespace/line-wrapping,
  no AST change. `rocq_monad_utils.ml` and `theories.ml`, both listed as
  drifted in the 2026-09-27 review that produced this backlog, turned out
  already clean on re-check — nothing here changed them since.

**Verification:** `dune build @lib/rocq_tools/fmt @lib/showable/fmt
@test/fmt` clean afterward. `dune build` and `dune exec test/tests.exe`
(9/9) both pass. None of these three files are read by `src/proof_solver*`
or anything `lib/model` depends on, so the proof-solver baseline wasn't
re-run.

**Session tally:** Tooling 1 · Bug fix 0 · Docs 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — Formatting debt outside `lib/model`, `proof_solver*` half

Closes the rest of backlog item D, deliberately deferred in the previous
entry because it touches files the proof solver reads and needs the full
baseline re-run to verify, not just `dune build`/`tests.exe`.

- **Tooling.** `dune build @src/fmt --auto-promote`:
  `src/graph_extract_lts.ml`, `src/proof_solver_wrapper.ml`,
  `src/proof_solver_step.ml`. Purely whitespace/line-wrapping, no AST
  change — same character as every other formatting pass in this log.
  `src/proof_solver.ml` and `src/graph_type.ml`, also listed as drifted in
  the original review, turned out already clean on re-check, same as
  `rocq_monad_utils.ml`/`theories.ml` in the previous entry.
- Found, not fixed: `dune build @src/fmt` also surfaces a pre-existing
  invalid odoc comment, `src/proof_solver_step.ml:65` — `{i {e.g., ...}}`
  triggers odoc's `{e ...}` emphasis-tag syntax by accident (`{e` needs to
  be followed by whitespace). Unrelated to this formatting pass (odoc
  syntax, not whitespace) and left alone as a small, low-risk item for a
  future docs pass rather than folded in here.

**Verification:** full five-file `PluginProofs.v` baseline, each file
rebuilt individually with `make -j1 <path>.vo` (after clearing its `.vo`/
`.glob`) for a trustworthy per-file count, per CLAUDE.md's procedure:

| file | counts |
| --- | --- |
| `Proc/Test1` | 114 105 106 109 22 21 |
| `Proc/Test2` | 446 278 299 194 446 182 |
| `CADP/Size1/MutualExclusion` | 268 396 |
| `CADP/Size1/Glued` | 268 396 |
| `CADP/Size1/Glued/MutualExclusion` | 81 63 |

All 18 counts match the baseline exactly — no regression, expected for a
pure-whitespace change. `_CoqProject` restored and `make dune` run
afterward; `dune build` and `dune exec test/tests.exe` (9/9) both pass.

This closes backlog item D in full.

**Session tally:** Tooling 1 · Bug fix 0 · Docs 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — A2 investigation: two saturation findings, no working positive test yet

Attempted backlog item A2: build a minimal hand-written LTS that positively
exercises `Proof_solver_step.ReModel.transition`'s multiple-actionpairs
fold (`6124eeb`'s fix — picks the shortest-annotation candidate instead of
raising when more than one `Action.t` matches the same `(from, label,
goto)`). Three hand-built terms and one check against a real example
(`CADP/Size1/MutualExclusion`, Trace-enabled) all failed to trigger it.
Digging into why turned up two separate findings in
`lib/model/algorithms/saturation.ml` / `lib/model/components.ml`, neither
fixed here — this entry exists to record them precisely enough that a
future session doesn't have to re-derive this.

- **Found, not fixed — real correctness bug.**
  `Saturation.Make.edge_action_destinations`
  (`lib/model/algorithms/saturation.ml:376`):
  ```ocaml
  let edge_action_destinations (d : data) (from : State.t) (ys : States.t)
    : ActionPairs.t
    =
    States.fold
      (fun (y : State.t) (acc : ActionPairs.t) -> check_from d y ActionPairs.empty)
      ys
      ActionPairs.empty
  ```
  The fold's `acc` is never read in the body — every `y` in `ys` is
  explored with a fresh `ActionPairs.empty`, so only the *last*-visited
  destination's results survive; every other destination silently vanishes.
  This only matters when a single action genuinely has more than one
  destination (real LTS nondeterminism under one label) — confirmed by
  building a minimal `tChoice`-based term (`p` offering the same label via
  two different intermediate states) and dumping the saturated FSM as JSON
  (`MeBi Config Output "DumpResults" True`, `MeBi Run Saturate p Using
  termLTS.`): one of the two reachable intermediate states was completely
  absent from `p`'s saturated action list, not merely deprioritized. The
  five-file baseline never exercises this because Proc.v's structural
  congruence rules (`do_fix`, `do_comm`, `do_seq_end`, `do_par_end`) are all
  deterministic — one destination per action — so `ys` is always a
  singleton there and the bug is inert. It would need to be exercised by a
  label with genuine multi-state branching, which doesn't happen in any
  example built so far. Left unfixed: out of scope for A2, and a fix needs
  its own baseline-reverification pass. The likely correct fix is threading
  `acc` through the fold (or explicitly unioning each `y`'s result into it)
  instead of discarding it — analogous to `check_destinations` three
  functions above (`lib/model/algorithms/saturation.ml:362`), which does
  this correctly (`States.fold (check_from d) xs`, letting `check_from`'s
  curried `acc` argument thread through) and is the pattern
  `edge_action_destinations` looks like it was meant to follow.

- **Found, not fixed — a structural reason A2 is hard.** Separately from
  the bug above, `ActionPair.try_update`
  (`lib/model/components.ml:971`, used by `ActionPair.merge_lists`, called
  from `Saturation.edge_actions`) merges two same-label candidates whenever
  `Action.wk_equal xaction yaction && States.equal xdestinations
  ydestinations` — `wk_equal` (`components.ml:888`) compares only `label`,
  ignoring `annotation` entirely. So *any* two same-label actions with
  exactly equal destination sets get collapsed to the shorter-annotation
  one immediately during saturation, before the model is even stored.
  Every actionpair `update_acc` (`saturation.ml:219`) ever produces starts
  as a `States.singleton`, and two singletons are "equal" as sets exactly
  when they hold the same one element — so two different-annotation
  witnesses for the *same* single `goto` are, by construction, always
  merged away at this point; verified by hand-tracing three deliberately
  different constructions (two-branch choice reaching a shared destination;
  a post-visible silent self-loop revisiting the same state via
  `Annotations.extrapolate`'s prefix generation, `components.ml:749`) and
  confirming each one collapses to a single surviving action for exactly
  this reason. For `ReModel.transition`'s fold to ever see more than one
  candidate, the competing actions' *full* destination sets have to be
  unequal-but-overlapping on the queried `goto` — which, given every
  actionpair is built as a singleton and singleton-vs-singleton always
  either matches-and-merges or doesn't-match-and-stays-separate-on-a-
  different-goto, seems to require a multi-element destination set to
  survive from a base-level branching action essentially unchanged — which
  is exactly the case the bug above corrupts. Whether the two findings are
  connected (i.e. whether fixing the first bug is a *precondition* for A2
  ever being constructible, or whether some other construction not yet
  tried — e.g. via `MeBi Run Merge`'s cross-FSM action combination, not
  investigated here — can produce it independently) is not established.
- **Tooling.** Kept one small, low-risk piece of instrumentation from the
  investigation: `src/proof_solver_step.ml`'s `transition` function now
  logs (`Logger.trace`, so silent unless `MeBi Config Output "Trace" True`)
  when it actually receives more than one candidate, with the count. Costs
  nothing when the branch isn't hit (confirmed: the CADP/Glued baseline
  file produces over 5 million trace lines with `Trace` enabled and zero
  hits). This is exactly the check A2's original note proposed as one way
  to confirm reachability — useful for whoever next investigates whether
  any of the expensive `Test3`/`Test4`/`Size2` examples hit this branch,
  without having to re-add it.

**Not fixed, not committed as example code:** the three hand-built example
attempts were discarded (not real, correct positive tests — one design was
never even bisimilarity-true as written). A2 remains open.

**Verification:** `dune build`, `dune exec test/tests.exe` (9/9). No
proof-solver behaviour changed (the one surviving code change is a
trace-only log line), so the full baseline wasn't re-run; `_CoqProject` and
all example files were restored to their pre-investigation state.

**Session tally:** Tooling 1 · Docs 1 · Bug fix 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-27 — Fix: saturation dropped destinations when one action had more than one (A5)

Fixes the correctness bug found during the A2 investigation above.

- **Bug fix.** `Saturation.Make.edge_action_destinations`
  (`lib/model/algorithms/saturation.ml:376`) explored a multi-destination
  action's `States.fold` with a *fresh* `ActionPairs.empty` on every
  iteration instead of threading the fold's own accumulator, so only the
  last-visited destination's results ever survived saturation. Fixed by
  matching `check_destinations` three functions above (`saturation.ml:362`,
  `States.fold (check_from d) xs`) — the sibling function this one looks
  like it was meant to mirror, and which already threads the accumulator
  correctly: `States.fold (check_from d) ys ActionPairs.empty`. Two-line
  net change.
- **Tooling.** Regression test added:
  `test_saturate_multi_destination_action` in `test/tests.ml` — a single
  silent action from state 0 reaching two destinations (1 and 2), which
  then diverge under different visible labels; both resulting weak
  transitions must survive saturation. Confirmed to be a real (not
  vacuous) regression test by temporarily reverting the fix and rerunning:
  exactly one of the two checks failed, matching the bug's exact mechanism
  (last-visited-survives). Adding this test surfaced a second, pre-existing
  issue: `test_saturate_with_tau` never actually exercised saturation at
  all — `FSM.saturate`'s default `only_if_weak:true` gates on
  `Info.weak_labels`, which the shared `info`/`lts`/`fsm` test helpers
  never set, so `saturate` silently returned its input unchanged and the
  test's "state count unchanged" assertion passed vacuously regardless.
  Fixed by adding an optional `~weak_labels` parameter to `info`/`lts`/`fsm`
  (defaulting to empty, so every other existing test is unaffected) and
  passing it through on both saturation tests.

**Verification:** full five-file `PluginProofs.v` baseline, each file
rebuilt individually via `make -j1 <path>.vo` for a trustworthy per-file
count:

| file | counts |
| --- | --- |
| `Proc/Test1` | 114 105 106 109 22 21 |
| `Proc/Test2` | 446 278 299 194 446 182 |
| `CADP/Size1/MutualExclusion` | 268 396 |
| `CADP/Size1/Glued` | 268 396 |
| `CADP/Size1/Glued/MutualExclusion` | 81 63 |

All 18 counts match the baseline exactly — expected, since (per the A2
investigation's analysis) none of the existing examples have a single
action with genuinely more than one destination, so the bug was inert for
all of them. `_CoqProject` restored and `make dune` run afterward. `dune
build` and `dune exec test/tests.exe` (11/11, up from 9/9) both pass.

**Session tally:** Bug fix 1 · Tooling 1 · Docs 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-28 — A2 revisited: a fresh, untested lead (cross-FSM merge)

No code changed — a follow-up read of `lib/model/components.ml` at
Jonah's request, to leave a documented lead for a fresh session rather
than continue building throwaway examples in this one.

- **Docs.** The 2026-09-27 A2 analysis covered deduplication *within* one
  FSM's own saturation (`Saturation.edge_actions`'s
  `ActionPair.merge_lists`/`try_update` fold), and concluded it makes
  same-`goto` ambiguity collapse automatically. That analysis doesn't cover
  how the *two* FSMs in a `weak_sim` proof combine: `MeBi Sim Begin` builds
  and saturates an FSM for each side separately, then merges them
  (`FSM.merge` → `EdgeMap.merge` → `ActionMap.merge` for any shared state).
  `ActionMap.merge` (`lib/model/components.ml:1162`,
  `ActionPairs.union (to_actionpairs a) (to_actionpairs b) |> of_actionpairs`)
  is a plain set union on full structural equality — it does not run
  `try_update`'s weaker collapse. So a state genuinely shared between both
  FSMs, with each FSM's own independent saturation deriving a
  different-annotation weak transition from it to the same `goto`, would
  survive the merge as two separate entries. Plausible root cause of the
  needed asymmetry: `MeBi Config`'s state-count bound (settable via `MeBi
  Config Bounds As Num States <n>`, `src/g_mebi.mlg:214`) truncating one
  FSM's BFS before it fully explores a shared state's descendants while
  the other's doesn't. Full write-up, including a concrete 4-step plan for
  a fresh session to try, in `notes/4-post-review-backlog.md`'s A2 section.

Entirely unverified — a hypothesis from reading `ActionMap.merge`, not a
confirmed mechanism.

**Session tally:** Docs 1 · Bug fix 0 · Tooling 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-28 — A1 sized before implementing: the `ReModel` miss path never fires

Branch `refactor/model-components`, merged into `main` on the `fork` remote
first (`da32f6b`, a `--no-ff` merge of the 30-commit branch) at Jonah's
request, so upstream integration can later be a single PR from `fork/main`
to `origin/main` that references these milestones. `origin/main` is
deliberately untouched.

`notes/2-unify-instead-of-lookup.md` (backlog item **A1**) proposes
replacing `ReModel.state`/`ReModel.label`'s syntactic hashtable lookup with
real unification, because the lookup can miss on evars, universe instances
or local-context differences. The note ends by suggesting the work be sized
first: instrument the miss path, count misses across the five cheap
`PluginProofs.v` suites, and if the count is zero treat the failure mode as
latent rather than active. That sizing pass was done before writing any of
the refactor.

- **Freshness check.** The note survives `16bbe37` intact. `ReModel` is now
  at `src/proof_solver_step.ml:92-170` rather than the note's `~92-150`, and
  `Model.States`/`Model.Labels` are now `Model.State.Set`/`Model.Label.Set`
  after the `6e436dd` nested-submodule rename. Nothing else in the analysis
  has drifted — `get_encoding`, the `Hashtbl.Make` key, and the `None`/`Some`
  theory fallbacks are all exactly as described.

- **Measurement.** Every miss path in `ReModel.state`/`ReModel.label` was
  temporarily raised to `Logger.warning` (which prints by default), and all
  five `### Success` files were rebuilt individually with `make -j1`.
  **Result: zero misses, in all five files.** All 18 iteration counts match
  the baseline exactly (`Test1` 114 105 106 109 22 21; `Test2` 446 278 299
  194 446 182; `MutualExclusion` 268 396; `Glued` 268 396;
  `Glued/MutualExclusion` 81 63).

  So A1 is **latent, not active**: on every example that currently works,
  the syntactic lookup always hits, and the unification rewrite would fix
  nothing presently observable while costing what the note itself warns is
  a much more expensive operation per lookup. A1 is accordingly *not*
  implemented, and drops in priority — per the note's own stated criterion.

- **Tooling.** The temporary `Logger.warning` probes were demoted to
  `Logger.debug` and kept (`src/proof_solver_step.ml`, +12 lines, no
  behaviour change). `Debug` is off by default and, unlike `Trace`, is not
  emitted on every function entry, so `MeBi Config Output "Debug" True`
  now gives a targeted read of the miss paths without the 5M-line `Trace`
  flood the A2 investigation ran into. Two of the five paths (`state`'s
  `Not_found`, and `label`'s two fallback outcomes) previously had no
  logging at all. Same rationale as the permanent silent trace left at
  `transition`'s fold site for A2.

What this measurement does **not** cover, and the natural follow-up: the
five files probed are exactly the ones that already succeed. The examples
that *fail* — `Proc/Test3`'s `wsim_p3` (unfinished at 500000, crashed at
1000000, backlog item B2) and `CADP/Size2/Glued` (marked `### FAIL`) —
were not probed, and a lookup miss there is a live candidate cause. Running
the same probe against a failing example is a cheaper and better-targeted
next step than implementing A1 blind.

**Session tally:** Tooling 1 · Docs 1 · Bug fix 0 · Refactor 0 ·
Optimization 0 · **New feature 0.**

---

## 2026-09-28 — A1 settled by measurement: the lookup keys structurally cannot miss

Branch `investigate/a1-sizing`. Follow-up to the sizing pass above, at
Jonah's request: "is there a new example or test we could derive to check
whether the `ReModel` unification is necessary?"

The answer turned out to be better served by a *classifier* than by a new
example. "Zero misses observed" is weak evidence — it says the failure
never happened, not that it could not. So instead of guessing at an example
that might trigger a miss, `Bi_encoding.classify_key` now counts, in any
term, the three things the A1 note says can make a syntactic key miss:
undefined evars, non-empty universe instances, and local-context variables.
`ReModel.state`/`label` call it on every lookup, at `Debug`.

- **Tooling.** `classify_key` in `lib/rocq_tools/bi_encoding.ml`, declared in
  both the `.ml`'s own `module type S` and `bi_encoding.mli`, so it reaches
  `src/proof_solver_step.ml` as `M.classify_key` via
  `Rocq_monad.S`'s `include Bi_encoding.S` with no intermediate signature
  churn. It walks the term with `Constr.iter` over the same
  `EConstr.to_constr ~abort_on_undefined_evars:false (sigma ())` form the
  hashtable hashes. Guarded by `Logger.is_enabled Output.Kind.Debug`, so it
  costs nothing when Debug is off.

  One caveat is documented at the definition: it inspects the term as given,
  before the `nf_all` that `Rocq_monad_utils.get_encoding` applies. Since
  normalisation can only *remove* these (instantiating defined evars,
  unfolding), never introduce them, a reported zero is conclusive for the
  real key while a non-zero count would need re-checking after normalisation.

- **Result.** All five `### Success` suites rebuilt with
  `MeBi Config Output "Debug" True` and `make -j1`:

  | file | lookups | classification | misses |
  | --- | --- | --- | --- |
  | `Proc/Test1` | 403 | all `evar=0 univ=0 var=0` | 0 |
  | `Proc/Test2` | 1614 | all `evar=0 univ=0 var=0` | 0 |
  | `CADP/Size1/MutualExclusion` | 305 | all `evar=0 univ=0 var=0` | 0 |
  | `CADP/Size1/Glued` | 305 | all `evar=0 univ=0 var=0` | 0 |
  | `CADP/Size1/Glued/MutualExclusion` | 93 | all `evar=0 univ=0 var=0` | 0 |

  **2720 lookups, not one carrying any of the three hazards.** All 18
  iteration counts unchanged. Debug output is also tractable — 7k-69k lines
  per file, against the 5M+ that `Trace` produced during the A2 work.

  This upgrades the earlier conclusion from "A1 never fires" to "A1 *cannot*
  fire here": the goal terms the solver resolves are closed, ground,
  evar-free and universe-free, so a syntactic key and a unifier would agree
  by construction on every one of them.

- **Structural corroboration.** `term` and `label` are parameterless `Set`
  inductives (`examples/Proc.v:3`, `:26`), so their universe instances are
  necessarily empty. The note's cause (2), universe instances, is not merely
  unobserved but unreachable for state and label terms in this codebase.

**What this means for A1, and it is a reframing.** For the plugin's current
usage there is no example that would exercise the unification, because
producing a state term carrying an evar means writing a goal where the state
is *unknown* — `Example ... : exists q, weak_sim p q. Proof. eexists. MeBi
Sim Begin ...` — i.e. asking the solver to discover `q` rather than check a
given `q`. The plugin cannot do that today. So implementing A1 would not be
fixing a latent bug; it would be building the enabling mechanism for a
capability the plugin does not have. Per `CLAUDE.md`'s working assumption
that is flagged here, before anything is written, and not started
unilaterally.

Two constructions considered and rejected as tests: a goal over a local
variable (`Example wsim (r : term) (H : r = p) : weak_sim r q`) does produce
`var=1`, but unification alone would not resolve it either without
rewriting by `H`, so it does not discriminate between lookup and
unification; and a convertible-but-not-syntactically-equal term is already
handled, since `nf_all` runs before both encode and lookup — the note says
as much.

Also formats `test/tests.ml`, which had drifted since the A5 regression test
landed in `a7bd17c` without a `@fmt` pass.

**Session tally:** Tooling 1 · Docs 1 · Bug fix 0 · Refactor 0 ·
Optimization 0 · **New feature 0** (one capability *flagged*, not built).

---

## 2026-09-28 — Logging: a level fix and a wasted-work fix

Branch `optimize/logger-formatting`, off `main` (`da32f6b`) rather than off
the A1 branch, since neither change has anything to do with A1. Both were
found while trying to use the A1 probe on a large example, not by looking
for them.

- **Tooling.** `lib/rocq_tools/bindings.ml:204` and `:216` logged at `Debug`
  on every call to `find_name` — one of them formatting an `EConstr` through
  `Strfy.econstr`, a Rocq pretty-printer. Lines 208 and 211, inside the same
  function, already used `Trace`, so the two `Debug` calls were the outliers.
  Moved both to `Trace`. This is what made `Debug` unusable as a diagnostic
  level: a `Debug`-enabled run of `Proc/Test3` spent 50 minutes inside
  `find_name` and never reached proof search.

  Confirmed on `Proc/Test1` with `Debug True`: `find_name` lines drop from
  ~1300 to **0**. Note the honest limit of that demonstration — Test1's total
  Debug output only falls 22113 → 20822 lines, because `find_name` was never
  the bulk *there*. The flood is a large-example problem, and whether this
  makes `Proc/Test3` tractable is **not yet verified**.

- **Optimization.** `lib/utils/logger.ml`'s `thing` (and `things`) called
  `out ... (f x)`. As a function argument `f x` is evaluated *before* `emit`
  ever consults `is_enabled`, so every `Logger.thing`/`things`/`option`/
  `options` call in the plugin formatted its value even with that output kind
  switched off. Both now guard on `is_enabled k` first.

  Behaviour-identical: `is_enabled` is the very predicate `emit` filters on,
  and it already accounts for both the global `quiet`/`disable` switch and
  the per-kind config.

  Measured, warm dependencies, default output configuration:

  | example | eager | lazy | delta |
  | --- | --- | --- | --- |
  | `Proc/Test1` | 1.67 / 1.67 / 1.68 s | 1.44 / 1.45 / 1.46 s | −13.3% |
  | `Proc/Test2` | 469.99 / 467.98 / 469.28 s | 461.25 / 463.06 s | −1.5% |

  Deliberately not overstated: the absolute saving grows (0.22s → 7s) but
  `Test2` is dominated by proof-search compute, so the proportional gain
  falls to near noise. **This is not an explanation for the plugin being
  slow on large examples**, and `TODO.md`'s A3 saturation item is untouched
  by it. It is a free, behaviour-preserving removal of wasted work, worth
  roughly 1–2% on realistic workloads.

**A correction worth recording.** On finding the eager formatting, the first
hypothesis was that it explained the `Debug` probe stall. It does not, and
the two are independent: laziness only helps when a kind is *disabled*,
whereas the stall happened with `Debug` *enabled*, where the formatting is
genuinely wanted and the cost is the sheer volume. The real fix for the
stall is the `bindings.ml` level change above. The measurement is what
separated them.

Verification: full five-file `PluginProofs.v` run, `make -j1` per file, all
18 counts identical to baseline (`Test1` 114 105 106 109 22 21; `Test2` 446
278 299 194 446 182; `MutualExclusion` 268 396; `Glued` 268 396;
`Glued/MutualExclusion` 81 63), zero `Unsolved`. `dune exec test/tests.exe`
11/11.

**Session tally:** Optimization 1 · Tooling 1 · Docs 1 · Bug fix 0 ·
Refactor 0 · **New feature 0.**
## 2026-09-28 — B2 reframed: saturation enumerates paths, not states

Branch `investigate/saturation-path-explosion`, off `main` (`da32f6b`).
`TODO.md`'s A3 ("optimize saturation -- takes a long time on larger/
multi-layered examples") has been an unquantified hunch since it was
written. It now has a mechanism, a location and a number.

**How B2 was framed, and why that was wrong.** The backlog said
`Proc/Test3`'s trouble is "specifically `MeBi Sim`'s proof *search*", with
extraction already succeeding. Two phase-isolation runs say otherwise:

- `CADP/Size2/Glued` fails in *extraction*, not search — `LTS_Incomplete`
  from `src/wrapper.ml:243`, raised when the state bound is hit. It never
  reaches the proof solver at all (zero `ReModel` lookups logged). Its
  `### FAIL: ^` tag, inherited rather than verified, turns out to be
  accurate.
- `Proc/Test3`'s `wsim_pq` was rebuilt with **no `Solve` at all** — just
  `MeBi Sim Begin`, so the wall time is extraction + saturation + merge with
  zero proof search in it. It ran **1 hour 13 minutes without completing**
  and was killed. The earlier 50-minute and 25-minute timeouts died in the
  same phase; the `Solve 300` cap tried in between was never going to help,
  because `Begin` runs unconditionally.

So for `Test3` the cost is not proof search. B2 as written is misdiagnosed.

**A hypothesis that was disproved on the way.** The first guess was that
multi-layer extraction (`Using compLTS termLTS`, two LTSs) was to blame.
It is not: `CADP/Size1/Glued/MutualExclusion` also uses two LTSs
(`Using lts step`) and is the *fastest* of the five baseline files at 81/63.

**The actual mechanism.** `Saturation.check_from`
(`lib/model/algorithms/saturation.ml:278`) prunes only against `d.visited`.
But `update_visited` returns a *copy* (`{ d with visited = ... }`), and
`check_destinations` is `States.fold (check_from d) xs` — every sibling
destination receives the same `d`. So `visited` accumulates down a path and
never carries across branches: the traversal enumerates **simple paths**,
not states. The `Traces` memo meant to curb this is properly global (one
`ref` created in `edges`, shared across source states), but is switched off
for whole subtrees by `collect_from_traces`'s `None, None` branch, which
recurses with `{ d with can_collect_traces = ref false }` — a *fresh* ref,
so nothing below re-enables it.

**Quantified.** `test/satscale.ml` (new, see below) saturates a k x k grid
of silent transitions — exactly the shape parallel interleaving produces,
since `Layered.compLTS`'s `do_parl`/`do_parr` let either side of a `cpar`
move — against a silent *chain* of identical state count as a control:

| k | states | simple paths | grid (s) | chain (s) | ratio |
| --- | --- | --- | --- | --- | --- |
| 6 | 50 | 924 | 0.10 | 0.0008 | 129x |
| 7 | 65 | 3432 | 0.85 | 0.0015 | 565x |
| 8 | 82 | 12870 | 13.44 | 0.0030 | 4481x |
| 9 | 101 | 48620 | **436.72** | 0.0050 | 87344x |

The chain is linear in state count. The grid, at the *same* state count,
takes 437 seconds for 101 states. Per-step growth is 8x, 16x, 32x while the
path count grows only 3.8x per step, so the cost is worse than path
enumeration alone — there is super-linear work per path as well.

**Why the fix is well-defined rather than open-ended.**
`ActionPair.try_update` (`lib/model/components.ml:971`) merges any two
actionpairs whose actions are `wk_equal` and whose destination sets are
*equal* by keeping `Annotation.shorter`. So of the exponentially many paths
enumerated, all but the **shortest annotation** in each equivalence class
are discarded. The exploration is computing, at great expense, something a
shortest-path search would produce directly. (Note the equivalence is on
*exactly equal* destination sets, so not everything collapses to a single
survivor — but within a class the work beyond the shortest is waste.)

- **Tooling.** `test/satscale.ml` plus its `test/dune` stanza: a pure-OCaml
  scaling harness linking `rocq-mebi.model` only, same constraint as
  `tests.ml`. Labelled explicitly as infrastructure per `CLAUDE.md` — it is
  a measurement binary, not plugin capability. It partially covers
  `TODO.md`'s unchecked "Benchmarking -> Algorithms -> Saturation" item,
  though it was written to answer this question rather than to be that
  feature. Its practical value going forward is that saturation changes can
  now be iterated in **seconds** against a known-bad shape, instead of
  hour-long Rocq builds, with the 18-count proof baseline as the
  correctness gate.

No change to the algorithm itself in this entry — this is the diagnosis.

**Session tally:** Tooling 1 · Docs 1 · Optimization 0 · Bug fix 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-28 — Differential harness for the saturation rewrite

Branch `investigate/saturation-path-explosion`. Step 1 of the plan in
`notes/5-saturation-rewrite.md`, agreed with Jonah: build the safety net
before touching the algorithm.

- **Tooling.** `test/satdiff.ml` plus `test/satdiff.expected` and a
  `test/dune` stanza. Infrastructure only, linking `rocq-mebi.model` — same
  constraint as `tests.ml` and `satscale.ml`.

  It generates deterministic pseudo-random LTSs, saturates each, and prints a
  **canonically sorted** rendering of the resulting `EdgeMap` — source states
  ordered, actions ordered, destination sets ordered — because `EdgeMap` is a
  `Hashtbl` and its iteration order is not a contract. 200 seeds produce 1332
  weak-transition rows, each showing label, full annotation and destination
  set.

  Why this and not the proof suite: `CLAUDE.md`'s 18-count baseline says the
  proofs still close in the same number of steps; it does *not* say the
  saturated FSM holds the same weak transitions. Since `ActionPair.try_update`
  merges on *exactly equal* destination sets and keeps `Annotation.shorter`, a
  rewrite can change which annotation survives and still pass the proof gate.
  That is the failure mode this harness exists to catch.

Two things learned building it, both worth recording:

- The first attempt generated 3-8 state graphs with out-degree up to 3 and
  **failed to clear a single seed in ten minutes** — with the current
  implementation. That is the blow-up being fixed, reproduced accidentally on
  graphs small enough to draw by hand. Sizes are now 3-5 states, out-degree
  1-2, which complete instantly; the harness is only useful while the *old*
  implementation can still finish.
- The first version printed per-seed timings into the dump, which made the
  output differ between runs and defeated the entire purpose. Timings now go
  to stderr; stdout is the artifact being diffed and may contain nothing that
  varies run to run. Verified deterministic across repeated runs.

`test/satdiff.expected` is the golden capture of the **current**
implementation, confirmed to match on a fresh run. The rewrite is green when
`dune exec test/satdiff.exe -- 200 2>/dev/null | diff test/satdiff.expected -`
is empty.

**Session tally:** Tooling 1 · Docs 1 · Optimization 0 · Bug fix 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-28 — Saturation rewritten: closure instead of path enumeration

Branch `investigate/saturation-path-explosion`. Step 2 of the plan in
`notes/5-saturation-rewrite.md`. Closes `TODO.md`'s long-standing A3
("optimize saturation -- takes a long time on larger/multi-layered
examples").

- **Optimization.** `Saturation.edges` now routes through `edge_closure`
  rather than `edge`. Instead of a depth-first enumeration of every simple
  path, it takes the reflexive-transitive silent closure of each state
  breadth-first (recording a shortest silent path to each member), then for
  every visible edge `s -a-> t` emits `(a, {goto})` for each `s` in the
  closure of the source and each `goto` in the closure of `t`, annotated
  with the concatenation. Results still go through
  `ActionPair.merge_lists` so everything downstream is untouched.

  Breadth-first is what makes this equivalent rather than merely similar:
  `ActionPair.try_update` merges `wk_equal` actions with equal destination
  sets by keeping `Annotation.shorter`, so of the exponentially many paths
  the old traversal explored, only the shortest per destination ever
  survived. The closure produces exactly those survivors directly.

  | k | states | simple paths | before | after |
  | --- | --- | --- | --- | --- |
  | 9 | 101 | 48620 | 436.72 s | **0.0009 s** |
  | 12 | 170 | 2704156 | infeasible | **0.0025 s** |

- **Bug fix.** The rewrite is *not* behaviour-preserving, and the
  differential harness caught exactly why. Over 200 generated LTSs:
  **0 weak transitions lost, 73 gained, 2 annotations strictly shorter, 0
  longer**, and 45 equal-length tie-swaps (`Annotation.shorter` returns its
  second argument on ties, so emission order picks among equally short
  witnesses).

  The 73 are a genuine under-approximation in the old algorithm. Its
  `visited` set prunes any witness that revisits a state — necessary to make
  a depth-first search terminate on a cyclic graph, but it also silently
  discards valid weak transitions, since `s =a=> t` holds whenever *some*
  walk `tau* a tau*` exists and walks may revisit states. The closure has no
  such restriction. This is the same character of defect as A5: a quiet
  under-approximation in saturation, inert on the examples that happen to
  work. A missing weak transition is a soundness concern for bisimilarity —
  a distinguishing branch that was never derived cannot separate two
  processes.

  Adopted with Jonah's explicit agreement, since it changes what the plugin
  computes rather than only how fast.

Verification:

- All 1405 emitted annotations checked structurally — every one a
  well-formed walk whose notes chain (`goto` = next `from`), starting at its
  source, ending at its declared destination, containing exactly one visible
  action matching its label.
- Full five-file `PluginProofs.v` run, `make -j1` per file: **all 18 counts
  identical to baseline, zero `Unsolved`**. That the counts are unchanged
  despite 73 extra weak transitions is the reassuring part — the additions
  are options the solver never needed.
- `dune exec test/tests.exe` 11/11.
- `test/satdiff.expected` regenerated against the new implementation
  (1332 -> 1405 weak rows).

**Session tally:** Optimization 1 · Bug fix 1 · Docs 1 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-28 — Delete the path-enumeration machinery

Branch `investigate/saturation-path-explosion`. Step 4 of the plan: the
closure implementation landed in the previous entry, so everything that
existed only to service the depth-first traversal is now dead.

- **Refactor.** Removed from `lib/model/algorithms/saturation.ml` and its
  `.mli`: the `data` record (`named`/`current`/`visited`/`traces`/
  `can_collect_traces`/`old_edges`) and its updaters, `check_from`,
  `check_actions`, `collect_from_traces`, `continue_check_destinations`,
  `check_destinations`, `edge_action_destinations`, `edge_actions`, `edge`,
  `stop`, `update_acc`, `finish_with_trace`, `finish_with_trace_upto`,
  `skip_action`, `already_visited` and `get_old_actions`. Deleted
  `lib/model/wip/` entirely — `wip_annotation`, `wip_trace`, `wip_traces`
  and their `dune` — with the `rocq-mebi.model.wip` dependency dropped from
  `lib/model/dune` and `lib/model/algorithms/dune`, and the `-I` plus six
  module lines dropped from `_CoqProject`.

  The public signature shrinks from 24 values and three submodules to a
  single `val edges`. Every consumer (`FSM.ml`, `FSM.mli`, `model.ml`,
  `model.mli`) already constrained only `state`, `states`, `labels` and
  `edgemap` and called only `edges`, so none of them needed touching.

  This is what made backlog item **A** moot rather than solved, as predicted
  in `notes/5-saturation-rewrite.md`: the trace memo whose soundness was
  going to be investigated no longer exists.

Verification: `test/satdiff.exe` output **byte-identical** to the golden
file before and after the deletion, which is the point — this commit must
change nothing observable. `dune exec test/tests.exe` 11/11; `satscale`
unchanged; full `make -j$(nproc)`.

Worth recording: `make` caught five warnings that `dune build` accepted —
unused module `Annotations`, unused module `Label`, and unused types
`label`/`annotation`/`trees`/`action` left behind by the strip. This is the
second time this session that `make`'s stricter settings caught something
`dune build` waved through, as `ASSISTED-CHANGES.md`'s verification-baseline
note warns. Always finish with a `make` run.

**Session tally:** Refactor 1 · Docs 1 · Optimization 0 · Bug fix 0 ·
Tooling 0 · **New feature 0.**

---

## 2026-09-28 — `Proc/Test3` after the rewrite: extraction 1h13m+ → 0.65s

Branch `investigate/saturation-path-explosion`. The end-to-end check on the
example that started this whole line of work. No code change.

`Proc/Test3`'s `PluginProofs.v`, unmodified, with its checked-in
`MeBi Sim Solve 100000`:

| phase | before | after |
| --- | --- | --- |
| `MeBi Sim Begin` (extraction + saturation + merge) | **1h13m, killed without completing** | **0.65 s** |
| whole file | never reached proof search | 166 s |

The lower bound on the speedup for that phase is about 6700x, and it is only
a lower bound because the old run never finished.

**What this does and does not fix.** The file still fails: proof search runs
its full budget and reports `(Stopped) Unsolved after 100001 iterations`
(the documented `N + 1`), so `Qed` fails and `make` exits 2. Of the 166
seconds, 0.65 is extraction and the remaining ~165 is proof search.

So B2 is now, for the first time, genuinely what the backlog always claimed
it was. The original entry said the trouble was "specifically `MeBi Sim`'s
proof *search*" with extraction already succeeding. That was **wrong when
written** — extraction never completed, so proof search was not even being
reached, which is what the 2026-09-28 phase isolation established. After the
saturation fix the description becomes accurate: extraction is now trivial
and the remaining problem really is search. The backlog entry has been
corrected to record both the error and the fact that its conclusion now
holds for a different reason.

Worth noting that the saturation fix *added* 73 weak transitions in testing,
which enlarges the search space rather than shrinking it — so `Test3`
remaining unsolved at 100000 iterations is not evidence against the rewrite.
Whether it closes at a higher bound is untested, and `Proc/Test3`'s
own comment already records `wsim_p3` as "unfinished after 500000, crashed
on 1000000".

**Session tally:** Docs 1 · Optimization 0 · Bug fix 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-28 — Wire the saturation differential into CI

Branch `main` (on the `fork` remote). Closes a gap noticed while
summarising: `satdiff` existed but nothing ran it automatically.

- **Tooling.** `.github/workflows/ci.yml` gains a step running
  `dune exec test/satdiff.exe -- 200` and diffing against
  `test/satdiff.expected`, between the `tests.exe` and `make` steps.

  The rationale is recorded in the workflow itself: this is the only check
  that catches a change to `Saturation` silently altering which weak
  transitions or annotations survive. The `PluginProofs.v` suite
  demonstrably cannot see that — all 18 of its counts stayed identical
  through a change that added 73 weak transitions. It is also already
  proven to catch this class of change, having flagged 167 lines when the
  closure rewrite landed.

  Safe to run unattended: the dump is canonically sorted and timings go to
  stderr, so it is deterministic and any diff is a real behaviour change.
  An intended one means regenerating the golden file deliberately and
  saying why.

**Session tally:** Tooling 1 · Docs 1 · Optimization 0 · Bug fix 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-28 — B2 at a larger bound: memory, not time, is the ceiling

Branch `main` (on `fork`). No code change — an experiment, at Jonah's
request, to learn whether `Proc/Test3` merely needs a bigger budget now
that the saturation fix lets it reach proof search at all.

`wsim_pq` isolated (the other three live proofs omitted so the measurement
is of one proof) and raised from `MeBi Sim Solve 100000` to `1000000`.

**It was killed after roughly four minutes**, `exit 137` (SIGKILL). The
cause is not the kernel OOM killer — `journalctl -k` records zero OOM kills
this boot — but **`systemd-oomd`**, the userspace one Ubuntu runs by
default:

```
Killed /user.slice/.../run-r25ac...scope due to memory pressure for
/user.slice/user-1001.slice/user@1001.service being 55.71% > 50.00%
for > 20s with reclaim activity
... systemd-oomd killed 19 process(es) in this unit.
```

Timing it from the logs: extraction finished at 16:07:44 (the last line
written), the kill landed at 16:11:24, so proof search ran about 220
seconds. At the ~1.64ms per iteration measured from the `Solve 100000` run
(100001 iterations in ~164s), that is roughly **130,000-145,000 iterations**
before exhausting memory on a 15GB machine.

**What this establishes.** The binding constraint on `Test3` is *memory per
iteration*, not time. At 100000 iterations the proof completes the budget in
165s and reports `Unsolved`; the ceiling sits only a little above that, so
raising the bound cannot help — the solver accumulates state per iteration
and runs out long before any plausible budget. This is a sharper statement
than "proof explosion", and it points the future investigation at the
solver's per-iteration allocation rather than at search strategy or bounds.

It also retires the question this experiment was set to answer: `Test3`
cannot be resolved by a larger bound. Whether it is *solvable* at all
remains unknown, since no run has ever exhausted the search.

The file's own comment already said `wsim_p3` "crashed on 1000000". That is
now explained rather than merely recorded, and it was a *different* proof —
this is `wsim_pq`, so the memory ceiling is not specific to one example.

Two practical notes:

- My pre-run estimate of "about 27 minutes" extrapolated the *time* per
  iteration and was right about the rate but wrong about which resource
  would run out first. Memory bound it at four minutes.
- `systemd-oomd` killed 19 processes in the scope, not just `rocq`. Runs
  like this should be capped — e.g. `systemd-run --scope -p MemoryMax=8G` —
  so an experiment cannot take unrelated processes with it.

**Session tally:** Docs 1 · Optimization 0 · Bug fix 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — The solve loop retained every intermediate proof state

Branch `fix/solve-loop-retains-proof-states`, off `main` (`b9e5594`).
Prompted by Jonah asking whether `Test3` might be solvable by splitting one
`Solve N` into several smaller `Solve` commands, and whether the same effect
could be had from the OCaml side — he recalled relying on per-command
batching and finding no OCaml equivalent.

- **Optimization.** `Proof_solver.solve`'s loop read

  ```ocaml
  | _ -> (try step p |> f (n + 1) with NothingToDo -> n, p)
  ```

  The recursive call sits inside the `try`, so it is not a tail call; worse,
  the handler body mentions `p`, so **every frame kept its own
  `Declare.Proof.t` reachable for the whole command**. A single
  `Solve 1000000` therefore held up to a million intermediate proof terms and
  evar maps alive at once, none of them collectable. Per-command batching
  worked precisely because each command returned, unwound the recursion and
  dropped the lot — there genuinely was no way to get that from OCaml, which
  answers the second question: nothing was being missed.

  Now catches around `step p` alone, so `f (n + 1) p'` is a real tail call and
  only the current `p` stays live.

- **Measured, and smaller than predicted.** Both runs are `wsim_pq` alone at
  `Solve 1000000` under an identical `systemd-run --scope -p MemoryMax=8G`,
  killed by the cgroup OOM killer at the same RSS (8368088 kB vs 8366780 kB):

  | | CPU time to exhaust 8G | RSS growth |
  | --- | --- | --- |
  | tail-call fix | **4min 40.8s** | 30.0 MB/s |
  | old loop | 3min 58.0s | 33.8 MB/s |

  **+18% more work within the same memory budget.** Real, worth keeping, and
  nowhere near enough for `Test3`, which needs orders of magnitude. The
  mechanism was right; the magnitude prediction was wrong — this was expected
  to be the dominant sink and is about a fifth of it.

- **What this implies for batching, not yet measured.** The remaining ~82% is
  most plausibly the proof term and evar map under construction — roughly
  50KB per iteration at the observed rate — which persists across command
  boundaries just as it does within a command. If so, batching cannot help
  `Test3` either. That is inference from the rate, *not* a measurement: the
  direct test (twenty sequential `Solve 50000` against one `Solve 1000000`,
  same cap, compare peak RSS) has not been run.

  A plausible reconciliation with Jonah's recollection: on shorter proofs the
  retained-frame cost is proportionally much larger, so batching would have
  helped visibly there, while on something the size of `Test3` the proof term
  dominates.

Verification: full five-file `PluginProofs.v` run, `make -j1` per file, all 18
counts identical to baseline, zero `Unsolved`; `tests.exe` 11/11.

Note also that capping the run (`systemd-run --scope -p MemoryMax=8G`) worked
as intended — the cgroup OOM killer took only `rocqworker`, where the
uncapped run on 2026-09-28 had `systemd-oomd` kill 19 processes in the scope.

**Session tally:** Optimization 1 · Docs 1 · Bug fix 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — CI was broken from the day it was added

Branch `main` (on `fork`). Jonah reported the GitHub run failing; the CI
workflow added 2026-09-27 (backlog item C1) had never actually passed.

```
mebi: no local opam switch in ./_opam (or it is incomplete).
[ERROR] Opam has not been initialised, please run `opam init'
Error: Process completed with exit code 50.
```

The `mebi:` lines are a red herring — that is `flake.nix`'s own shellHook
reporting the absent switch, which on CI has no tty and so just prints and
continues. The real failure is the step after it.

- **Tooling.** Two bugs in `.github/workflows/ci.yml`, both mine:

  1. **No `opam init`.** `opam switch create .` needs an initialised opam
     *root* (`~/.opam`), and a fresh runner has never had one. Added an
     `Initialise opam root` step running
     `opam init --bare --no-setup --disable-sandboxing --yes`, guarded on
     `~/.opam` not existing. `--bare` because the switch we want is the
     local one created by the next step; sandboxing off because opam's
     bubblewrap sandbox is unreliable inside the nix shell and the runner
     is already isolated.
  2. **The cache saved only half the state.** `path: _opam` captured the
     local switch but not the opam root, so even a cache hit would have
     left every later `opam env` failing the same way. Now caches both
     `_opam` and `~/.opam` under the same lock-file key.

  Verified as far as is possible locally: `opam --version` is 2.5.2 in the
  flake's shell, all three flags exist there (`-n, --no-setup` is spelled
  with the short flag, which an earlier grep missed), and the exact command
  was run against a scratch `OPAMROOT`, reporting `[default] Initialised`.
  The workflow YAML parses and the step order is right. It cannot be fully
  verified without a push, since the failure is specific to a runner that
  has never seen opam.

Worth recording plainly: C1 was marked done on 2026-09-27 on the strength
of the workflow being written, not of a green run. A CI job that has never
passed is not CI. The same caution applies to anything else closed this
session on the strength of local verification alone.

**Session tally:** Bug fix 1 · Docs 1 · Tooling 0 · Optimization 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — Batching settled by measurement: it does not release memory

Branch `main` (on `fork`). No code change. Runs the experiment left open by
the previous entry, rather than leaving it as a note for a fresh session.

**Question.** Would splitting one `MeBi Sim Solve 1000000` into several
smaller `Solve` commands let `Proc/Test3` get further, because memory is
released between commands?

**Method.** `wsim_pq` alone, twenty sequential `MeBi Sim Solve 50000`
commands (1,000,000 iterations in total, matching the single-command run),
under an identical `systemd-run --scope -p MemoryMax=8G`, with RSS sampled
every 20 seconds so command boundaries are visible in the curve.

**Result: no release at any boundary.** RSS climbed straight through them:

| elapsed | RSS | commands finished |
| --- | --- | --- |
| 01:57 | 3888 MB | 2 |
| 02:37 | 5110 MB | 2 |
| 02:57 | 5713 MB | 3 |
| 03:37 | 6954 MB | 3 |
| 03:57 | 7503 MB | 4 |
| 04:17 | 8123 MB | 4 |

Killed by the cgroup OOM killer at 8370260 kB after **4 completed commands
= 200,004 iterations**, 4min 31.2s CPU. The single-command run reached
4min 40.8s CPU at the same 8G ceiling; at the 1.36 ms/iteration this run
measures, that is about **207,000 iterations**. The two are within ~3% of
each other.

So batching and a single command are equivalent, and the earlier inference
(recorded in the previous entry as *not* measured) is now confirmed: the
memory is the proof term and evar map under construction, which persist
across command boundaries exactly as within a command. **No batching scheme
can resolve `Test3`.** The ceiling is roughly 200,000 iterations per 8GB,
about 40KB per iteration.

**On the recollection this tested.** Jonah recalled relying on per-command
batching and finding no OCaml-side equivalent. The second half was exactly
right and is now fixed (`87403dc`): the solve loop retained every
intermediate proof state, which only a command boundary could release. That
effect is real but about a fifth of the growth, so batching would have
helped visibly on shorter proofs where the retained frames are
proportionally large, and cannot help on anything `Test3`-sized where the
proof term dominates. Both halves of the recollection are accounted for.

**What this leaves for B2.** Not bounds, not batching, not the loop. The
remaining lever is the ~40KB per iteration itself — what `step` adds to the
proof term each time, and whether the search can be made to close in far
fewer steps. That is a proof-solver design question, and the first genuinely
open one since this line of work began.

**Session tally:** Docs 1 · Optimization 0 · Bug fix 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — B2 diagnosed: the proof search enumerates paths, not pairs

Branch `main` (on `fork`). No code change; instrumentation only, reverted.
Takes up the question the previous entry left as "the first genuinely open
one" — what the ~40KB per iteration is spent on, and whether the search can
close in far fewer steps.

**Method.** A throwaway `B2Probe.v` next to each example, `Debug` on, plus a
one-line temporary addition to `handle_state` so the *cofix* hypotheses were
logged alongside the non-cofix ones (`Hyps.log ~cofix_only:(Some true)`).
That makes every iteration self-describing: proof state, conclusion, and the
exact set of coinduction hypotheses in scope. A small Python pass over the
log assigns each `weak_sim` conclusion a pair id and classifies each visit as
*first*, *closed against an ancestor cofix*, or *re-explored*.

**Result for `Proc/Test3`'s `wsim_pq`, at `Solve 20000`:**

| | |
| --- | --- |
| iterations | 20,001 |
| `weak_sim` goal visits | 2,446 |
| **distinct pairs** | **17** |
| first visits | 17 |
| closed immediately against an ancestor cofix | 1,623 |
| **re-explorations** | **806** |
| maximum cofix stack depth | 16 |

All 17 pairs are discovered inside the first ~300 iterations. The remaining
19,700 iterations discover nothing: they re-derive pairs that were already
proved. The rate is flat — about 80 re-explorations per 2000 iterations in
every window of the run, with no sign of converging.

**Mechanism.** `handle_weaksim` closes a `weak_sim` goal only when
`Hyps.can_solve_concl_cofix ()` finds a syntactically equal *cofix
hypothesis in the current context*, and otherwise calls `handle_new_cofix`,
which runs `FixTactics.cofix` on a fresh name. Cofix hypotheses are
introduced inside a branch, so they are visible only to that branch and its
descendants. A pair therefore closes when it repeats an **ancestor** on the
current branch, and starts a whole fresh subtree when it repeats a
**sibling** already proved elsewhere. The search is consequently enumerating
**simple paths through the product graph**, not its states — the maximum
cofix depth capping at 16 for a 17-pair relation is exactly that signature.
This is the same pathology A3 found in saturation, one layer up: the fix
there replaced path enumeration with a closure over states.

**Why the passing examples pass.** The same instrumentation on the cheap
suites shows the difference is in the product graph's shape, not in the
proof strategy:

| proof | iterations | pairs | closed on ancestor | re-explored |
| --- | --- | --- | --- | --- |
| `Test1/wsim_pq` | 114 | 13 | 6 | **0** |
| `Test2/wsim_pq` | 446 | 10 | 29 | 32 |
| `Test2/wsim_qp` | 278 | 10 | 22 | 18 |
| `Test2/wsim_qr` | 299 | 10 | 22 | 18 |
| `Test2/wsim_rq` | 194 | 8 | 13 | 10 |
| `Test2/wsim_pr` | 446 | 10 | 29 | 32 |
| `Test2/wsim_rp` | 182 | 8 | 13 | 10 |

`Test1`'s product graph is acyclic enough that ancestors always suffice, so
it costs nothing. `Test2` already pays — roughly half its `weak_sim` visits
are redundant — but its simple-path tree is small enough to terminate.
`Test3`'s is not. Nothing about `Test3` is special except size; the waste is
present throughout and has simply been affordable until now.

**What this means for the "~40KB per iteration" framing.** It is not the
wrong question, but it is the second question. Each redundant subtree is
also a redundant chunk of proof term, so the memory ceiling and the
iteration count have the same cause. Shrinking per-iteration allocation
would buy a constant factor against a combinatorial blow-up.

**Candidate fix, flagged not started.** `FixTactics.mutual_cofix : Id.t ->
(Id.t * constr) list -> unit Proofview.tactic` exists in the Rocq 9.2
runtime. Because `check_bisimilarity` has already computed the full relation
before proof search begins, the solver could issue **one mutual cofix at the
start**, naming every pair in the relation, instead of a fresh nested
`cofix` per newly-seen pair. Every cofix hypothesis would then be in scope in
every branch, `can_solve_concl_cofix` would close every repeat at its first
encounter, and the search would be linear in the size of the relation rather
than in its number of simple paths. Per `CLAUDE.md`'s working assumption this
is raised **before** writing it: it is a different proof-construction
strategy and a substantial piece of net-new machinery in `proof_solver*`,
even though it proves the same statements the plugin already states. Two
consequences to weigh first: `mutual_cofix` yields one goal per cofixpoint,
and the step machinery currently assumes a single focused goal with a global
mutable `ProofState`; and the checked-in iteration counts for `Test2` and
probably the CADP suites would **drop**, so the 18-number baseline in
`CLAUDE.md` would need regenerating rather than matching.

**Verification.** All instrumentation reverted; `git status` clean,
`make dune` clean, `dune exec test/tests.exe` 11/11. No baseline run was
needed — no source changed.

**Session tally:** Docs 1 · Optimization 0 · Bug fix 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — Feasibility of the mutual-cofix fix for B2

Branch `main` (on `fork`). No code change; instrumentation and a throwaway
Rocq probe, both reverted. Answers the question the previous entry flagged
before writing anything: is `FixTactics.mutual_cofix` implementable against
the step machinery, and is it the right fix?

**Q1. Does the step machinery cope with many goals?** Yes, and it already
does. `Declare.Proof.by` is `Proof.solve env (Goal_select.select_nth 1)`
(`vernac/declare.ml:2173`), so the `Proofview.Goal.enter` in
`Proof_solver.step` always sees exactly one focused goal — goal 1 — no matter
how many are open. Logging `List.length (Proof.data ...).goals` at the top of
`step` shows the proof routinely carrying **15 to 33 open goals**, peaking at
33, across `Test1` and `Test3`:

| open goals | 1–11 | 12–16 | 17–23 | 24–33 |
| --- | --- | --- | --- | --- |
| steps | 195 | 811 | 3,130 | 942 |

Rocq's goal list is the solver's work stack and the global mutable
`ProofState.StateM` is the continuation for whichever goal is currently
first. N sibling goals from a mutual cofix is therefore not a new regime.

*(An earlier reading that some iterations ran `handle_state` twice for two
goals was wrong: those 2,444 double-logs are `step ()`'s own `ExitWeakSim`
tail call re-entering `handle_state` on the same goal.)*

**Q2. Is there a multi-goal hazard?** Yes — one, and it is sharp. After
`mutual_cofix`, every goal in the block is *syntactically identical to its
own hypothesis*. `handle_weaksim` consults `Hyps.can_solve_concl_cofix ()`
before anything else, so on each freshly-opened block goal it would find
`Cofix_i` and close the goal with `exact Cofix_i` — unguarded. Nested
`cofix` never exposes this because `handle_new_cofix` applies
`In_sim`/`Pack_sim`/`intros` in the same chained tactic, so the bare goal is
never observed. Both halves were checked in Rocq directly
(`theories/MutualCofixProbe.v`, a 2-state cycle — the smallest product graph
needing a *sibling* rather than an *ancestor* hypothesis):

- a mutual cofix in which each branch closes with the **other** branch's
  hypothesis compiles and **`Qed` succeeds** — the guard checker accepts
  exactly the proof shape this fix would build;
- the same proof with one branch closed by bare `exact CB` needs
  `Fail Qed.` to compile — the hazard is real, and it fails loudly at `Qed`
  rather than silently.

The fix is confined and does not require making `StateM` per-goal: emit
`mutual_cofix` and the `In_sim`/`Pack_sim`/`intros` for every block goal as
**one** tactic, with `Proofview.tclALLGOALS` for the second part. `select_nth
1` focuses before the tactic runs and does not constrain the goals the
tactic itself creates.

**Q3. Can the cofix types be built?** Yes. `FixTactics.mutual_cofix : Id.t ->
(Id.t * constr) list -> unit Proofview.tactic` needs a `weak_sim ltsA ltsB a
b` per pair; `Decoder.state : state -> EConstr.t` goes through
`Bi_encoding`'s `bck` map, which stores the original `EConstr.t` that was
encoded, so it returns the very term the goal will contain. A1's measurement
de-risks the cross-`sigma` question for free: all 2,720 terms `ReModel`
resolves are `evar=0 univ=0 var=0`, i.e. closed and ground, so a term decoded
under the command-time `sigma` is safe to inject into a proof goal.

**Q4. Which pairs?** This is where the design is actually decided, and the
obvious choice is the wrong one. Two candidates:

- *all bisimilar pairs* (`a ∈ A`, `b ∈ B`, same partition block) — needs no
  replay of the solver's choice function, so nothing can drift out of sync;
- *pairs reachable in the product from the root* — needs a BFS mirroring
  `try_get_visible_transition`.

Dumping the FSMs and partition settles it. `Proc/Test3`'s `wsim_pq`: A has 8
states, B has 18, and the partition is a **single block containing all 26**,
so all-bisimilar is 144 pairs against the 17 the search actually visits.
`CADP/Size1/MutualExclusion`'s `wsim_bigstep_lts`: A has 10 states, B has 33,
two blocks (29 and 4), about **240** bisimilar pairs — against a proof that
currently closes in **268 iterations total**. The superset would make that
example dramatically *worse*. So the pair set must be the reachable product,
computed by a BFS in `lib/model` over the saturated FSMs.

**Q5. What would it do to the examples that already pass?** Re-measured with
the cofix logging, and this is the most important result of the session:

| proof | iterations | pairs | closed on ancestor | re-explored |
| --- | --- | --- | --- | --- |
| `Test1/wsim_pq` | 114 | 13 | 6 | 0 |
| `Test2/wsim_pq` | 446 | 10 | 29 | 32 |
| `Test2/wsim_rp` | 182 | 8 | 13 | 10 |
| `CADP/Size1/ME` `wsim_bigstep_lts` | 268 | 19 | 1 | **0** |
| `CADP/Size1/ME` `wsim_lts_bigstep` | 396 | 50 | 1 | **0** |
| `Proc/Test3/wsim_pq` | >100,000 | 17 | 1,623 | 806 and rising |

The CADP proofs re-explore **nothing** — their reachable product is a tree,
and they close exactly one goal by coinduction in several hundred
iterations. Only `Test2` and `Test3` re-explore at all. So the fix is not a
general speed-up: it is targeted at product graphs that are not trees, and on
the tree-shaped ones its only effect is to split one goal into N and add one
cheap closure per product edge. Those counts would move a little (up, not
down, on CADP), `Test2`'s would fall, and `Test3` would go from
non-terminating to roughly the low hundreds. The 18-number baseline needs
regenerating either way.

**Recommendation.** The fix is feasible and correctly targeted, and the
multi-goal question is not a blocker. The remaining work is genuinely in
`lib/model` — a reachable-product BFS mirroring the solver's own choice
function — plus a single new tactic in `proof_solver_tactics`. It is still
net-new machinery and still wants a decision before it is written.

**Incidental, found while dumping and not fixed:** `MeBi Config Output
"DumpResults" True` aborts the whole `.v` file with a `System error` unless
`_dumps/` already exists, because `Utils.FileWriter.create_parent_dir fn`
creates `Filename.dirname fn` and is called with the directory itself
(`"./_dumps/"`, whose dirname is `"."`). And every dump filename's month is
one low (`2026 08 29` on 2026-09-29): `get_local_timestamp` prints `Unix`'s
0-based `tm_mon` without adding 1. Both are two-line fixes.

**Verification.** All instrumentation and probes reverted; `git status`
clean, `make dune` clean, `dune exec test/tests.exe` 11/11. No source
changed, so no baseline run was needed.

**Session tally:** Docs 1 · Optimization 0 · Bug fix 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — Two bugs in the JSON dump path

Branch `main` (on `fork`). Both were found while using
`MeBi Config Output "DumpResults" True` to size the pair set for B2's
candidate fix (previous entry), and both are independent of that work.

**1. `DumpResults True` aborted the whole `.v` file unless `_dumps/` already
existed.** *(Bug fix.)* `Utils.FileWriter.create_parent_dir fn` creates
`Filename.dirname fn` — the directory that will *hold* `fn`. Its only caller,
`Json.write`, was passing it the output directory itself, and
`Filename.dirname "./_dumps/"` is `"."`, which always exists. So nothing was
created, and the `open_out` two lines later failed with

```
System error: "./_dumps/2026 08 29 - 11:09:54 | ... | FSM a (original).json:
No such file or directory"
```

which is raised out of the command and kills the compilation. Fixed by
moving the call below the `filepath` binding and passing `filepath`, so the
helper gets the file path its contract asks for. The `(* TODO: *)` that sat
directly above the misuse is gone with it. The helper itself is unchanged and
still correct; only the call site was wrong.

**2. Every dump filename's month was one low.** *(Bug fix.)* `Unix.tm_mon` is
0-based and `get_local_timestamp` printed it raw, so a dump written on
2026-09-29 was named `2026 08 29`. December would have read `00`. Fixed with
`tm_mon + 1`. The per-field zero padding was hand-rolled as
`(if tm_mon < 10 then "0" else "")` immediately before printing `tm_mon`, and
that coupling is precisely what hid the bug — a `+ 1` applied to the printed
value alone would have left the guard testing the wrong number. Replaced the
six hand-rolled guards with `%02d` so the padding cannot drift from the value
again. `tm_year + 1900` was already right.

**Verification.** `MeBi Config Output "DumpResults" True` on a throwaway
`Proc/Test3` probe with no `_dumps/` present: the directory is created, all
five dumps are written, the file compiles, and the names now read
`2026 09 29 - 11:25:32`, matching `date`. Before the first fix the same probe
failed to compile. `make -j$(nproc)` on the unmodified `_CoqProject` is
clean, `make dune` is clean, `dune exec test/tests.exe` is 11/11, and
`dune build @fmt` is clean. No proof-suite run: the change is in `lib/utils`
and touches neither `lib/model` nor `src/proof_solver*`.

**Noticed, deliberately not changed.** `get_local_timestamp` is a `string`,
not a `unit -> string`, so it is evaluated once when the module is
initialised and every dump in a session shares the plugin-load time rather
than its own. That reads as intentional — it groups a session's dumps
together in a directory listing, and the filename already carries the source
line — so it is recorded here rather than "fixed" on the way past.

**Session tally:** Bug fix 2 · Docs 1 · Optimization 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — Which examples the B2 fix serves, and a correction

Branch `main` (on `fork`). No code change; instrumentation reverted. Jonah
pushed back on the previous entry's closing line — "worth it if `Test3`/
`Test4` matter; not a general win" — and asked whether there is any
indication that the `Test3`/`Test4` shape is *not* relevant to the plugin.
There is not. That framing was wrong and is retracted here.

**What actually triggers the blow-up.** The first guess — that the
re-exploration follows from a calculus having interleaving and structural
congruence — was tested and is wrong. `Flat.Simple` and `Flat.Complex` share
the same `term` type, so `Test1`'s own `p`/`q` can be proved under each in
turn with nothing else changed. `Complex` adds `do_parl`, `do_parr`,
`do_assocl` and `do_assocr`; the result was **13 pairs and 0 re-explorations
under both**, 114 iterations against 130. So the rules alone do not do it.

The reason is that **`Flat` has no atomic action rules at all** — no
`do_send`, no `do_recv`, only `do_handshake`, which consumes a whole `tpar`
in one step. `do_parl`/`do_parr` need a sub-transition to lift and so can
never fire. `Layered` is the only module in the corpus where a component can
move on its own (`termLTS`'s `do_send`/`do_recv` lifted by `compLTS`'s
`do_parl`/`do_parr`). The blow-up needs **independently-evolving recursive
components**, and the corpus bears that out exactly:

| example | shape | pairs | re-explored |
| --- | --- | --- | --- |
| `Test1` (`Flat.Simple`) | `tfix (tseq (tpar ...) trec)` — parallelism *inside* a sequential loop, one way round | 13 | 0 |
| `Test1` terms under `Flat.Complex` | same terms, interleaving rules present but unfireable | 13 | 0 |
| `CADP/Size1/ME` | `weak_sim bigstep lts c1 c1` — two semantics of *one* term, not a comparison of two systems | 19 / 50 | 0 |
| `Test2` (`Flat.Complex`) | two *independently recursive* processes in parallel; their `do_fix` unfoldings commute | 10 | 32 |
| `Test3` (`Layered`) | the same, over two LTS layers, with `do_comm`/`do_assoc*` on top | 17 | 806 and rising |

So the proofs the fix does nothing for are the degenerate ones: `Test1`'s
concurrency cannot interleave, and CADP is not a concurrency comparison at
all. The proofs it serves are concurrent recursive processes compared up to
weak bisimilarity — which is what the plugin is for (`README.md`: "automating
bisimilarity proofs ... from *Advanced Topics in Bisimulation and
Coinduction*, Section 3.2.2"). **`Layered` is the most realistic calculus in
the repository, not an outlier**, and the correct statement is that the fix
is a large win on the central case and a modest cost on the peripheral ones.

**Predicted cost, from the recorded trajectories.** Measuring local work per
pair and per closure directly out of the instrumented logs gives, for a
mutual-cofix proof of `pairs x local + edges x close`:

| proof | now | predicted after the fix |
| --- | --- | --- |
| `Test3/wsim_pq` | >100,000, never closes | **~620** |
| `Test2/wsim_pq` | 446 | ~159 |
| `Test2/wsim_rp` | 182 | ~118 |
| `Test1/wsim_pq` | 114 | ~151 |
| `CADP/Size1/ME` bigstep | 268 | ~399 |
| `CADP/Size1/ME` lts | 396 | ~549 |

`Test3` goes from unsolvable to a few hundred iterations (and, at ~40KB
each, from an 8GB ceiling to tens of megabytes); `Test2` improves about
2.8x; `Test1` and CADP get roughly 40% more expensive, staying in the same
order. The CADP figures are the least trustworthy — those proofs contain
only one coinductive closure each, so the estimator has almost no sample for
the closure cost and falls back to half the local cost. Worth measuring
rather than extrapolating before the numbers are quoted anywhere.

**`Test4` is blocked earlier, and should not be used to justify this fix.**
Run for the first time: at the default 100-state bound it fails **extraction**
with `LTS_Incomplete` and never reaches proof search; raised to 5000 states,
extraction was still running after 14 minutes at ~490MB with zero solver
iterations. `Test4` is `cpar (cpar a1 a2) (cpar b1 b2)` against permutations
and re-associations of itself, so its state space is the permutation lattice
of four processes under full structural congruence. That is a state-space
problem, not a proof-search one, and the mutual cofix would not by itself
unlock it.

**Evidence the fix would actually close `Test3`.** Two things from the 20,001
iteration trajectory. First, the pair set is **closed under the successor
relation**: 17 pairs, 65 distinct pair-to-pair steps, every target inside the
set, and no new pair after iteration ~300. That is precisely the coinduction
invariant a mutual cofix needs — every obligation lands on a hypothesis.
Second, and a caveat on the estimate above, the **edge** set is not saturated
nearly as fast: 38 edges by iteration 500, 55 by 4,000, 65 by 20,000, with
the last new edge at **18,403**. The depth-first search takes tens of
thousands of iterations to get round to obligations a breadth-first
enumeration would list immediately — which is an argument *for* precomputing
the product rather than discovering it, and also means ~620 is a floor.

**Also closed since the previous entry.** The sharpest remaining risk — that
a plugin-built cofix type might not match the goal the solver later sees —
is settled by existing measurement. `Concl.eq` goes through `econstr_eq`,
whose default `~enc:true` path compares by encoding, and `Bi_encoding`'s
table is keyed on `EConstr.eq_constr` under its own sigma. A1 already
measured 2,720 `ReModel` lookups with zero misses and every key `evar=0
univ=0 var=0`, so the goal terms are closed and ground, `eq_constr` on them
is sigma-independent, and `Decoder.state` returns the very key that matched.

**Still open before implementing.** (1) Whether the product BFS should
replay the solver's exact choice function or over-approximate it with every
bisimilar response — the over-approximation removes a lockstep-fragility but
its size has not been measured. (2) The CADP regression should be measured,
not extrapolated. (3) `Test4`'s extraction cost deserves its own backlog item.

**Verification.** Instrumentation reverted; `make dune` clean,
`dune exec test/tests.exe` 11/11. No source changed.

**Session tally:** Docs 1 · Bug fix 0 · Optimization 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — B2 explored further: a second, independent defect and a hard design constraint

Branch `main` (on `fork`). No code change; instrumentation reverted. Three
explorations that the previous entry left open, plus one that was not
planned and turned out to matter more than the ones that were.

### 1. A second defect, independent of B2 and general to every proof

`Hyps.try_invert_any` re-inverts hypotheses it has already inverted.
`inversion H` does not clear `H`, `Hyp.invertibility` grades on the shape of
the hypothesis rather than on whether inverting it would yield anything new,
and nothing records what has been inverted — the function's own docstring
still names a `inverted_hyps` parameter that no longer exists, and the
logging line for it is still there, commented out
(`src/proof_solver_step.ml:502,506`).

Measured from the instrumented trajectories, counting a step as *sterile*
when the goal is unchanged and the only new hypothesis is an exact duplicate
of one already in context:

| proof | steps | create a duplicate | provably sterile |
| --- | --- | --- | --- |
| `Test1/wsim_pq` | 114 | 18 (15.8%) | 3 (2.6%) |
| `Test2/wsim_pq` | 446 | 70 (15.7%) | 12 (2.7%) |
| `Test2/wsim_qp` | 278 | 49 (17.6%) | 12 (4.3%) |
| `CADP/Size1/ME` bigstep | 268 | 13 (4.9%) | 13 (4.9%) |
| `CADP/Size1/ME` lts | 396 | 140 (35.4%) | 56 (**14.1%**) |
| `Test3/wsim_pq` | 20,001 | 4,836 (24.2%) | 2,391 (**12.0%**) |

A worked instance, `Test3` iterations 5-6: the context holds
`H1 : termLTS (tfix (tact (send A) trec)) a t'` and
`H4 : compLTS (cprc (tfix (tact (send A) trec))) a (cprc t')`, both graded 3
along with `H`. `try_invert_any`'s fold breaks ties toward the *last*
candidate, so it picks `H4`; inverting `H4` yields `termLTS (tfix ...) a t'`,
which is `H1` again, and the goal does not move. The next iteration makes
progress only because the duplicate lands at the end of the list and is
picked instead.

This is **not** the cause of B2's blow-up — it is a constant factor — but it
is worth more on CADP (14.1%) than on `Test3` (12.0%), so it is a general
win that would *partly offset* the regression the mutual-cofix fix is
predicted to cause on the tree-shaped proofs. It is small, self-contained,
needs no new machinery, and can be done and verified entirely on its own. It
would lower the 18-number baseline.

### 2. The over-approximating pair set is dead — measured, not argued

The previous entry offered the choice between replaying the solver's exact
choice function and over-approximating it with *every* bisimilar response,
noting the second removes a lockstep fragility. Computing both offline from
the dumped FSMs and partition kills the second outright:

| | `Test3/wsim_pq` | `CADP/Size1/ME` bigstep |
| --- | --- | --- |
| pairs the solver actually visits | 17 | 19 |
| over-approximating reachable product | **144 pairs, 7,680 edges** | **100 pairs, 448 edges** |
| all bisimilar pairs, no reachability | 144 | 240 |

For `Test3` the over-approximation degenerates to the full all-bisimilar
set — everything is in one partition block and everything is reachable —
with a mean out-degree of 53. It is not a slightly-larger set, it is a
different order of magnitude, and it would make every proof far worse.

### 3. A re-implementation of the choice function drifts immediately

This was the unplanned finding. An exact replay was written offline — ~40
lines mirroring `try_get_visible_transition` plus `handle_wk_concl`'s silent
stay-put rule — and run against the pairs the solver was observed to visit.
It produces **16 pairs and 48 edges**, and the pair count is right: the
solver's 17th is the pre-unfolding goal written with the definitions `s1`,
`r1` rather than their bodies. (The "65 edges" in the previous entry was an
overcount — consecutive `weak_sim` *visits* in a depth-first walk include
backtracking steps, which are not product edges. 48 is the trustworthy
number.) But of the 16 pairs, **8 have the wrong B-side**. All 8 A-sides are
right.

The reason is in `lib/model/components.ml`. `shortest_annotation` is
`List.fold_left ActionPair.shorter_annotation` over `to_list`, and
`shorter_annotation` swaps only on a strict `1` — so ties keep whichever
element comes first in `ActionPair.Set`'s own ordering, which runs through
`Action.compare`, which compares annotations and constructor trees
structurally. Then `min_elt` is taken over *that one pair's* destinations,
not the union across ties. Reproducing the choice therefore means
reproducing the whole model comparison stack.

**So the lockstep fragility is not hypothetical, and the design constraint
is firm:** the product BFS must *call the same OCaml function the solver
calls*, not mirror it. Fortunately that is a clean lift.
`try_get_visible_transition` is Rocq-dependent only in its first two lines
(`ReModel.state tys.(3)`, `ReModel.label tys.(5)`); everything from
`Model.Action.Map.reduce_by_label` onward, and `handle_wk_concl`'s silent
rule, is pure model code over `(fsm_a, fsm_b, partition, (a, b))`.

### What this suggests the root cause is

Not the mutual cofix's absence. **The solver decides the bisimulation
relation incrementally, inside the proof, one Rocq tactic at a time, when
the relation is pure model data that could be computed once before the proof
starts.** Every symptom follows from that: the product can only be
discovered depth-first, so closure can only be against ancestors, so the
search enumerates paths; and the decision procedure cannot be tested at all
without a Rocq runtime, which is why none of this surfaced earlier.

### Proposed decomposition, none of it started

- **Step 0** (independent, any order) — the sterile-inversion fix above.
  *Verification:* the 18 counts drop; regenerate the baseline.
- **Step 1, pure refactor** — lift the pure-model core of the response choice
  out of `src/proof_solver_step.ml` into `lib/model`, called by the solver
  exactly as now. *Verification:* the 18 counts must be **identical**. No new
  capability, and valuable on its own.
- **Step 2, new model code, no proof changes** — the product BFS on top of
  Step 1's function, plus a `test/` harness asserting its pair set against
  what the solver actually visits on the five cheap suites. This is where
  drift gets caught, and — because Step 1 makes the decision procedure
  Rocq-free — it runs under `test/tests.exe` with no Rocq runtime.
- **Step 3, the actual fix** — `mutual_cofix` over Step 2's pair set, with
  `tclALLGOALS` for the `In_sim`/`Pack_sim`/`intros`. By this point
  everything it depends on has been measured.

Steps 0, 1 and 2 are individually safe and verifiable; Step 3 is the only
one carrying real risk, and it is the only one that needs a decision about
new machinery.

**Revised prediction for `Test3`** on the corrected edge count: 16 pairs x
~10.4 local + 48 closures x ~6.8 ≈ **~490 iterations**, against >100,000 and
never closing.

**Verification.** Instrumentation reverted; `make dune` clean,
`dune exec test/tests.exe` 11/11. No source changed.

**Session tally:** Docs 1 · Bug fix 0 · Optimization 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — Step 0 attempted and REVERTED: the inversion tie-break is load bearing

Branch `main` (on `fork`). Net change: a comment in
`src/proof_solver_step.ml` recording the experiment. The code is as it was.

**What was tried.** The sterile re-inversions found in the previous entry
come from `Hyps.try_invert_any`'s fold breaking grade ties toward the *last*
candidate. `Hyp.invertibility` grades on shape, so a whole chain of
transitions grades identically — `H : compLTS (cpar (cprc X) R) a (...)`,
`H1 : termLTS X a Y` and `H4 : compLTS (cprc X) a (cprc Y)` all score 3 — and
taking the last picks `H4`, whose inversion reproduces `H1`. The fix tried
was to break ties toward the **smaller** hypothesis: the innermost
transition is the one whose inversion determines the label and destination.
Implemented as a `type_size` on `Hyp` plus a two-key comparison, with `<=`
so an exact tie on both keys still kept the later hypothesis.

**Result: 16 of 18 byte-identical, 2 catastrophic.** All five cheap suites
were rebuilt. The iteration counts came back as the exact baseline —
`21 22 63 81 105 106 109 114 182 194 268 268 278 299 396 396 446 446` — but
**two of them were now `Unsolved`**: `wsim_lts` in
`CADP/Size1/Glued/PluginProofs.v` and `wsim_lts_bigstep` in
`CADP/Size1/MutualExclusion/PluginProofs.v`, both `MeBi Sim Solve 395`,
both reporting `Unsolved after 396` where they had reported `Solved after
396`. Raising the bound to 5000 to find the real cost: `wsim_lts_bigstep`
had **not closed after 10 minutes and 1.4GB** and was killed. A 396-iteration
proof became one that does not finish.

**Reverted**, and the baseline re-verified after the revert: 18 of 18
`Solved`, counts identical.

**What this says, and it is not a small thing.** Which hypothesis gets
inverted steers the entire downstream path, and the search has no plan to
fall back on when a local heuristic sends it somewhere else. A change that
is locally strictly better — it provably removes work that produces nothing
— flips two proofs from converging to diverging. The 12-14% of sterile steps
is real and still worth recovering, but it cannot be recovered by making the
local choice smarter while the search remains an unplanned depth-first walk.

No conservative variant exists either. Keeping the choice and merely dropping
the duplicate afterwards does not work: the next step would re-make the same
choice on the same context, so the solver loops forever. Any fix must change
which hypothesis is inverted, and that is exactly what proved unsafe.

**So Step 0 is parked behind Steps 1 and 2**, not abandoned. Once the product
relation is computed up front, the solver knows which response it is looking
for, and the inversion order stops being able to decide whether a proof
converges. That is the point at which this becomes safe to revisit — and it
is one more argument for the root cause identified in the previous entry.

The reasoning, the measurements and the warning are now a comment on the
tie-break itself, so the next person to look at that fold finds out before
trying it rather than after.

**Verification.** `make -j$(nproc)` with all five cheap suites enabled: 18 of
18 `Solved`, counts matching the baseline exactly. `make dune` clean,
`dune exec test/tests.exe` 11/11, `dune build @fmt` clean.

**Session tally:** Docs 1 · Bug fix 0 · Optimization 0 · Tooling 0 ·
Refactor 0 · **New feature 0.**

---

## 2026-09-29 — Steps 1 and 2: the response choice lifted, and the product computed up front

Branch `main` (on `fork`). Two commits. Step 0 was attempted first and
reverted — see the previous entry.

### Step 1 — `Product.respond` (Refactor)

`try_get_visible_transition` decided, at every proof step, which state the
right-hand FSM moves to in reply to a labelled move by the left-hand one.
Only its first two lines were Rocq-dependent; everything after read the FSM,
the state, the label and the bisimilar set and nothing else. It now lives in
`lib/model/algorithms/product.ml` as `Product.respond`, exposed through
`Model.Product`, with the solver calling it.

Deliberately shared rather than specified: the tie-breaks run through
`Action.Pair.Set`'s ordering via `Action.compare`, and the offline
re-implementation from the previous entry got 8 of 16 responses wrong.

*Verification:* all five cheap suites, **18 of 18 `Solved`, counts
byte-identical** — `21 22 63 81 105 106 109 114 182 194 268 268 278 299 396
396 446 446`. A pure refactor has to be exactly that, and it is.

*Incidental:* the new module had to be registered in **three** places —
`lib/model/algorithms/dune`, `_CoqProject` and `src/mebi_plugin.mlpack`. The
dune build passed while the make build failed twice, once per list missed.
That is backlog item C3 happening, not a hypothetical.

### Step 2 — `Product.successors` / `Product.reachable` (New feature, infrastructure)

`successors a b pi (x, y)` is every game state one move away, mirroring what
the solver does with one `weak_sim` goal: obligations come from the
**unsaturated** left FSM (that is what inversion of the hypothesis yields), a
silent move to somewhere already bisimilar to the right-hand state is
answered by standing still, and everything else goes through `respond`.
`reachable` is the breadth-first closure. This computes, before any proof
step runs, the relation the solver currently discovers depth-first.

Labelled **new feature** per `CLAUDE.md` even though no plugin behaviour
changes: it is net-new machinery, not a rearrangement. Nothing calls it yet.

Eight assertions added to `test/tests.exe` (11 → 19), all pure OCaml with no
Rocq runtime — which is the point of Step 1. They cover `respond` landing on
a bisimilar state and raising otherwise, the silent stand-still rule,
termination on a cyclic product, and a **diamond**: four game states with
four moves, two of which land on the same meeting point, so a walk that can
only close against its own ancestors must prove that state twice. That is
`Proc/Test3`'s failure in miniature.

### The cross-check, and what it found

`reachable` was run inside the plugin against the pairs the solver actually
visits, on all five cheap suites plus `Proc/Test3`:

| suite | proofs | predicted vs observed |
| --- | --- | --- |
| `Proc/Test1` | 4 | **exact** |
| `Proc/Test2` | 6 | **exact** |
| `Proc/Test3` | 1 | 16 vs 17 — the extra is the root goal before unfolding |
| `CADP/Size1` ME + Glued | 4 | predicted **6 fewer** each |
| `CADP/Size1/Glued/ME` | 2 | predicted **4 more** (8 vs 4, 7 vs 3) |

Ten of the eighteen match exactly or to the known root-spelling. The other
eight are three separate discrepancies, and finding them is what the harness
is for:

1. **The root goal before unfolding.** `Proc/Test3`'s 17th is
   `weak_sim ... (cpar (cprc s1) (cprc r1)) ...` — the same pair written with
   the definitions rather than their bodies.
2. **Intermediate spellings (the CADP +6).** Dumping the goals as terms shows
   the extras are dominated by partially-unfolded forms —
   `(weak_sim (PRC (worker_create 0 (REC_DEF Protocol.PMainLoopDef ...`,
   `(weak_sim (composition_create 0 Protocol.P) (composition_create 0
   Protocol.P))` — where `Decoder.state` returns the fully normalised term.
   Same game state, different spelling. Two predicted pairs were not matched
   under any spelling and are not yet explained.
3. **Over-prediction (the Glued/ME −4).** `reachable` explores past where the
   proof actually closes on `wsim_bigstep` and `wsim_spec_lts`. Not yet
   diagnosed. Harmless for correctness — surplus pairs are surplus goals —
   but it is surplus work and it is not understood.

**This matters for Step 3 and is why it was worth building Step 2 first.**
`can_solve_concl_cofix` compares syntactically, so a mutual cofix whose
hypothesis types come from `Decoder.state` will not close a goal still in an
intermediate spelling until it has been unfolded. The solver does unfold, via
`Concl.try_unfold_any`, but Step 3 has to order that against the closure test
rather than assume it. Had the mutual cofix been built first, this would have
surfaced as proofs mysteriously failing to close.

**Verification.** Both steps: all five cheap suites, 18 of 18 `Solved`, counts
byte-identical to the baseline. `dune exec test/tests.exe` 19/19. `make dune`
clean, `dune build @fmt` clean. All probes removed and the example files
restored to their committed state before the final run.

**Session tally:** Refactor 1 · New feature 1 · Docs 1 · Bug fix 0 ·
Optimization 0 · **Tooling 0.**

---

## 2026-09-29 — B2 fixed: one mutual cofix over the precomputed product

Branch `main` (on `fork`). Three commits: the two preliminaries, an
ocamlformat pass, and the fix. This closes backlog item **B2**.

### Falsification first

The plan's own cheapest kill-shot was run before any plugin code: does Rocq's
guard checker accept a large mutual cofix where branches close with *each
other's* hypotheses? A generated cycle over the real
`weak_sim`/`In_sim`/`Pack_sim` compiles and `Qed`s at **10, 50 and 200**
branches — 1s at 50, 24s at 200. Superlinear, irrelevant at the sizes in
play (16 to 44).

### P1 — the cross-check, redone on encodings

Step 2's cross-check compared *printed terms*, which is why CADP appeared to
be 6 pairs short. Printed terms carry the goal's spelling; the plugin's
canonical identity is the encoding. Redone as
`(ReModel.state tys.(5), ReModel.state tys.(6))` against
`Product.reachable`'s `Pair.Set`:

- **zero** goals failed to resolve, on all 18 proofs;
- **zero misses** — every pair the solver visits is predicted;
- 16 of 18 match exactly; 2 over-predict by 4.

The gate was zero misses, because a miss means a goal with no hypothesis.
It held. The earlier +6 was an artifact of my own diff, not of the model.

### P2 and P3 — landed separately, each at 18 of 18 with identical counts

`can_solve_concl_cofix` now returns the matching hypothesis and the caller
closes with a new `Tacs.exact_hyp`, instead of letting `trivial` find it
again by hint search — fine for one hypothesis per branch, neither cheap nor
predictable with the whole relation in scope. And `handle_weaksim` now runs
`Concl.try_unfold_any` to exhaustion *before* consulting the cofixes: the
unfolding used to sit inside `handle_new_cofix`, harmless while every
hypothesis was minted from the goal itself, but not once the hypotheses are
built ahead of time from decoded model states and `Concl.eq` is syntactic.

### The fix

`MeBi Config Solver MutualCofix True` (new; off by default) makes
`handle_new_proof` enter a new `OpenBlock` state, which normalises the
conclusion, resolves the goal's own pair, computes
`Model.Product.reachable`, and emits

```
mutual_cofix Cofix0 [(Cofix1, ty1); ...]
  <*> all_goals (In_sim; Pack_sim; intros)
```

as **one** tactic. The second half is not optional: straight after
`mutual_cofix` every block goal is syntactically its own hypothesis, so a
`handle_weaksim` running in between would close each with an unguarded
`exact` and `Qed` would reject the proof — the hazard confirmed on the
2-state probe. With the block open, `handle_new_cofix` is unreachable and a
pair outside the product raises `PairNotInProduct` naming it, rather than
leaving a stuck goal.

### Result

**`examples/Bisimilarity/Proc/Test3/PluginProofs.v` compiles**, in 17.7s:

| proof | pairs | iterations | before |
| --- | --- | --- | --- |
| `wsim_p3` | 42 | **1127** | unfinished after 500000, crashed on 1000000 |
| `wsim_pq` | 16 | **387** | `Unsolved after 100001` |
| `wsim_qp` / `wsim_qr` | 18 | 519 | never run |
| `wsim_rq` / `wsim_rs` | 24 | 603 | never run |
| `wsim_pr` | 8 | 211 | never run |
| `wsim_rp` / `wsim_sr` | 12 | 331 | never run |

`_CoqProject`'s `### TODO: proof explosion` is now `### Success`, and
`wsim_p3` — commented out since it was written, with the note "unfinished
after 500000, crashed on 1000000" — is uncommented and `Qed`s.

Across the five cheap suites, aligned per proof:

| suite | nested | mutual |
| --- | --- | --- |
| `Proc/Test1` ×4 | 114 105 106 109 | unchanged |
| `Proc/Test1` ×2 | 22, 21 | **69, 63** |
| `Proc/Test2` ×6 | 446 278 299 194 446 182 | **112 112 112 84 112 84** |
| `CADP/Size1` ×6 | 268 396 268 396 81 63 | unchanged |

**3794 → 2654 iterations, −30% overall.**

### The one regression, and it is understood

The two `Proc/Test1` proofs that got worse are exactly the two where
`Product.reachable` over-predicts — 8 pairs against the 4 the solver visits,
and 7 against 3. A surplus pair is a surplus goal that still has to be
proved, and on proofs of 22 and 21 iterations four extra goals cost 47 and
42. Diagnosed, not fixed. It is bounded (surplus is cost, never failure, and
P1's gate keeps misses at zero) and it is the last open thread on B2.

### What is still open

- The over-prediction above. `successors` enumerates obligations from the
  unsaturated FSM; the solver evidently discharges fewer on those two
  proofs. Worth a look, but it is a constant-factor tidy-up, not a blocker.
- **Step 0 is now safe to retry.** The sterile re-inversions (2.6% of steps
  on `Test1`, 12.0% on `Test3`, 14.1% on CADP) could not be recovered while
  the search had no plan to fall back on — breaking the inversion tie-break
  turned two 396-iteration proofs into non-terminating ones. With the
  relation known up front, inversion order can no longer decide whether a
  proof converges. Every number above is measured *with* the sterile steps
  still in, so they are all understated.
- Whether the flag should become the default. That is a decision about the
  checked-in bounds, and belongs with @dcastrop.
- `Test4` remains blocked on **extraction** (B3), untouched by this.

**Verification.** Flag off: all five cheap suites, 18 of 18 `Solved`, counts
byte-identical to the baseline, zero mutual-cofix activations. Flag on: 18 of
18 `Solved` with the counts above, plus `Proc/Test3`'s nine. `dune exec
test/tests.exe` 19/19, `make dune` clean, `dune build @fmt` clean.

**Session tally:** New feature 1 · Refactor 1 · Docs 1 · Bug fix 0 ·
Optimization 0 · **Tooling 0.**

---

## 2026-09-29 — The tool can tell when the mutual cofix is needed, and a latent partition bug

Branch `main` (on `fork`). Two commits. Answers Jonah's question: if this
shipped, could the plugin work out for itself whether
`MeBi Config Solver MutualCofix True` is required?

**It can, and cheaply, because the product relation is known before any proof
step runs.** Both strategies can simply be costed on the model. A mutual
cofix visits each game state once and each move once — `pairs + moves`. A
nested cofix walks the tree of simple paths, because it can only close a
repeat that is an *ancestor*. `Model.Product.estimate` computes both, capping
the nested walk at a small multiple of the mutual cost since only the
comparison matters; `prefer_mutual` is the decision.
`MeBi Config Solver MutualCofix` now also takes **`Auto`**.

**The ratio separates the corpus cleanly.** Measured on all 27 proofs:

| ratio nested / (pairs+moves) | proofs | what actually happens |
| --- | --- | --- |
| **0.5–0.6×** | `Proc/Test1` ×6, `CADP/Size1` ×6 | nested is cheaper — and these are exactly the two proofs a mutual cofix made *worse* (22→69, 21→63) plus the ten it left unchanged |
| **1.4–2.4×** | `Proc/Test2` ×6 | mutual wins: 446→112, 278→112, 194→84 |
| **10× to 664×, five over the cap** | `Proc/Test3` ×9 | only mutual finishes |

`Auto` picks correctly on **all 27**. `Test1` and CADP keep their baseline
counts exactly; `Test2` and `Test3` take the mutual path. Over the 18 cheap
proofs that is **2565 iterations, against 3794 nested and 2654
always-mutual** — better than either fixed strategy, with no regression
anywhere. The default stays `False`, because `Nested` costs nothing to
compute and is what every checked-in bound was measured against; `Auto` is
the setting to recommend if this ships.

The estimate is also the honest explanation of the two regressions recorded
in the previous entry: on a single diamond the nested walk costs 5 goals
against a mutual block's 8, so small products genuinely favour the nested
strategy. It takes about three diamonds in series before the doubling
overtakes the constant. That is now a test.

### The bug this turned up

Writing that test exposed a real defect in `lib/model`.
`Partition.get_bisimilar x p` was
`find_first (fun ys -> States.mem x ys)`. `Set.S`'s `find_first` returns the
least element satisfying a predicate and **requires that predicate to be
monotonically increasing** over the set's ordering. "This block contains
[x]" is not, and with a non-monotonic predicate the binary search is
unspecified. It really does miss: on a ten-block partition of twenty states
it failed to find the block holding the second state, which is visibly there.

Both callers — `Results.get_bisimilar_states`, which is on the solver's hot
path via `handle_visible_transition`, and `Product.successors` — turn
`Not_found` into the empty set. So a miss surfaced not as an error but as a
state with nothing bisimilar to it, and hence as a transition the solver
could not respond to. Replaced with a filter-and-choose.

**Latent, not active, on the current corpus:** all 27 checked-in counts are
unchanged, so the binary search happened to land correctly on every partition
these examples produce. It would not have stayed that way.

**Verification.** Six proof suites, 27 of 27 `Solved`, every count identical
— the 18 baseline counts and `Proc/Test3`'s nine. `test/satdiff.exe -- 200`
matches its golden file. `dune exec test/tests.exe` 30/30 (19 → 30).
`make dune` and `dune build @fmt` clean.

**Session tally:** Bug fix 1 · New feature 1 · Docs 1 · Refactor 0 ·
Optimization 0 · **Tooling 0.**

---

## 2026-09-29 — `Auto` becomes the default solver strategy

Branch `main` (on `fork`). Jonah's call: make `Auto` the default, announce
only when it deviates, and put the question to @dcastrop.

**Default flipped.** `Api.the_solver_strategy` is `Auto`, and so is what
`MeBi Config Reset` restores. `Nested` and `Mutual` still force either path
exactly.

**It reports only the deviation.** The `Notice` now fires **only** when
`Auto` takes the mutual path — that is the departure from what the solver has
always done, it changes the iteration count a checked-in `MeBi Sim Solve`
bound was measured against, and where it matters it is the difference between
finishing and not. Staying on the nested path is the status quo and says
nothing (it logs at `Debug`). Across the six suites the notice fires exactly
six times, once per `Proc/Test2` proof; `Proc/Test3` sets `MutualCofix True`
explicitly so it is not `Auto` and stays quiet.

**All 27 proofs still pass**, and the counts are now:

| suite | strategy chosen | counts |
| --- | --- | --- |
| `Proc/Test1` | nested | 114 105 106 109 22 21 |
| `Proc/Test2` | **mutual** | 112 112 112 84 112 84 |
| `Proc/Test3` | mutual (explicit) | 1127 387 519 519 603 211 331 603 331 |
| `CADP/Size1` | nested | 268 396 268 396 81 63 |

`CLAUDE.md`'s verification baseline is updated to these 27 numbers, with the
old 18 nested-path figures kept alongside for forcing `MutualCofix False`.
The estimate costs nothing measurable: a full six-suite `make -j1` is 3m09s
against 3m13s before the flip.

**`Proc/Test2`'s bounds are deliberately left loose.** They are the
nested-path figures (446, 278, 299, 194, 446, 182) while the proofs now close
in 112/84. `MeBi Sim Solve N` only caps, so the file compiles either way —
tightening them would break it under `MutualCofix False`. Recorded in
`CLAUDE.md` so the mismatch does not read as staleness.

**`Proc/Test3` keeps its explicit `MutualCofix True`.** `Auto` would choose
the same thing, but the setting is left as a record of what the file depends
on, and so it still works if the default changes. Its comment says so.

**Raised with @dcastrop**, in `TODO.md` alongside the `paper/` and LICENSE
items, and as **C8** in the backlog note. Three sub-decisions, none of them
Claude's to make: whether `Auto` is the right default for a released tool
given it can silently change a proof's iteration count; whether `Test2`'s
loose bounds should be tightened; and whether `Test3` should keep its
explicit setting.

**On the bug from the previous entry** — yes, fixed and shipped in
`b493e1d`, not merely reported. `Partition.get_bisimilar` no longer uses
`find_first` with a non-monotonic predicate.

**Verification.** Six proof suites, 27 of 27 `Solved` with the counts above.
`dune exec test/tests.exe` 30/30, `test/satdiff.exe -- 200` matches its
golden file, `make dune` and `dune build @fmt` clean.

**Session tally:** Docs 1 · New feature 0 · Bug fix 0 · Refactor 0 ·
Optimization 0 · **Tooling 0.**

---

## 2026-10-01 — Step 0, gated to the mutual path

Branch `main` (on `fork`). Optimization. The sterile re-inversions recorded
on 2026-09-29, recovered where it is safe to do so.

**Why gating, and a correction.** The previous entry said Step 0 was "now
safe to retry" because the search finally had a plan to fall back on. That
was too broad. The two proofs the original attempt broke — `wsim_lts` and
`wsim_lts_bigstep`, both 396 iterations, neither closing after 5000 — are
**CADP** proofs, and `Auto` keeps CADP on the *nested* path. Nothing about
them changed, so they would have broken again.

What is true is narrower and sufficient: **with the mutual block open, every
reachable pair already has a hypothesis in scope**, so sending the search
down a different route cannot lose a closure, only reorder what gets proved.
On the nested path it demonstrably can. So the smaller-first tie-break in
`Hyps.try_invert_any` now applies when `Api.the_mutual_cofix` is set, and
nowhere else — the nested path is byte-identical by construction, not by
measurement.

**Result.** All 27 proofs `Solved`:

| suite | path | before | after |
| --- | --- | --- | --- |
| `Proc/Test1` | nested | 114 105 106 109 22 21 | identical |
| `Proc/Test2` | mutual | 112 112 112 84 112 84 | identical |
| `Proc/Test3` | mutual | 1127 387 519 519 603 211 331 603 331 | **1043 355 483 483 555 195 307 555 307** |
| `CADP/Size1` | nested | 268 396 268 396 81 63 | identical |

**7-8% off every `Proc/Test3` proof**, and nothing else moves. `Test2` is
unchanged because its hypotheses are flat `termLTS` chains where the tie
rarely has a size to break — consistent with its sterile-step rate having
been the lowest measured (2.7-4.3%, against `Test3`'s 12.0%).

**A mistake worth recording.** The first attempt at the gate failed
immediately: `Test1`'s first proof went from `Solved after 114` to
`Unsolved after 115`, on the *nested* path, which the gate was supposed to
leave untouched. The original fold is

```
match Int.compare grade n with -1 -> keep old | _ -> take new
```

so a **tie takes the new (later) hypothesis**, and my rewritten condition
kept the earlier one. The gate was right; the predicate around it was not.
Caught by the suite on the first run, which is the argument for running all
27 rather than reasoning about which ones could be affected.

**Bounds left loose.** `Proc/Test3`'s checked-in `MeBi Sim Solve` bounds are
now the pre-2026-10-01 figures while the proofs close in 7-8% fewer steps.
`Solve N` only caps, so the file still compiles; leaving them loose keeps it
working across heuristic changes, which is the same decision already taken
for `Test2`. `CLAUDE.md` carries the real counts and says so.

**What is left of Step 0.** The nested path still spends 2.6% (`Test1`) to
14.1% (`CADP/Size1`'s `wsim_lts_bigstep`) of its steps re-inverting. That is
not recoverable by a better local heuristic — it is the same unplanned walk
that broke on 2026-09-29. Recovering it needs either a product-aware nested
path or a fallback that retries with the mutual block when the nested one
stalls. Neither is designed.

**Verification.** Six proof suites, 27 of 27 `Solved` with the counts above.
`dune exec test/tests.exe` 30/30, `test/satdiff.exe -- 200` matches its
golden file, `make dune` and `dune build @fmt` clean.

**Session tally:** Optimization 1 · Docs 1 · Bug fix 0 · New feature 0 ·
Refactor 0 · **Tooling 0.**

---

## 2026-10-01 — `Product.reachable` stops at reflexive pairs; Step 0 found to break forced-mutual CADP

Branch `main` (on `fork`). Backlog item 12, plus a regression found while
verifying it.

### The over-prediction, diagnosed and fixed — Bug fix

`Product.reachable` predicted 8 and 7 pairs on `Proc/Test1`'s `wsim_pr` and
`wsim_rp` against the 4 and 3 the solver visits. The cause is a short-cut the
model did not mirror: `Proof_solver_step.handle_weaksim` tests
`Concl.is_weak_refl` before anything else, and closes `weak_sim x x` by
`weak_sim_refl` outright when both sides use the same LTS. `r` is one step of
`q`'s unfolding, so the game soon reaches pairs of *equal* states — and
`successors` went on to enumerate the whole of `p`'s loop behind them.

`successors`/`reachable`/`estimate` now take `~refl:bool` (both sides share an
LTS) and treat a pair of equal states as a leaf. The open block passes the
same `econstr_eq tys.(3) tys.(4)` test `is_weak_refl` uses; `Auto`, which runs
before the goal exists, compares the two LTS qualids — two names for one LTS
only lose the short-cut, which over-predicts (cost, never a missing pair).
New regression test in `test/tests.exe` (30 → 34).

**A mistake caught on the first run.** Making a reflexive pair a leaf but
still minting it a cofixpoint broke `wsim_pr` under a forced mutual cofix
with `PairNotInProduct`: the block's `all_goals (In_sim; Pack_sim; intros)`
puts the leaf's own goal past the point where `weak_sim_refl` can apply, and
its successors were no longer in the block. Reflexive leaves now get no
cofixpoint at all; every goal reaching one is still a bare `weak_sim x x` and
closes by reflexivity. The `(Mutual cofix over N pairs.)` notice now counts
cofixpoints actually minted.

**Result.** Under `Auto`, 27 of 27 `Solved`; 26 counts byte-identical, and
`Proc/Test3`'s `wsim_p3` **1043 → 995** (42 → 39 cofixpoints — it had three
reflexive leaves too). `CLAUDE.md` updated. With `MutualCofix True` forced,
`wsim_pr`/`wsim_rp` go **69/63 → 22/21**, i.e. exactly their nested counts:
the "two `Test1` slowdowns" recorded on 2026-09-29 as the cost of a small
product were this over-prediction, not something inherent to the mutual
path. `Test2` and `Glued/MutualExclusion` unchanged under forced mutual.

**Correcting the record.** The 2026-09-28 Step 2 table attributes the
"8 vs 4, 7 vs 3" over-prediction to `CADP/Size1/Glued/MutualExclusion`. It
was `Proc/Test1`'s `wsim_pr`/`wsim_rp`; `Glued/MutualExclusion` shows no
over-prediction (81 and 63, mutual or nested).

### Step 0 (`ac98c3c`) breaks `MutualCofix True` on CADP — found, not fixed

Running the forced-mutual pass over all six suites, CADP
`MutualExclusion`'s `wsim_lts_bigstep` (396 iterations) did not finish: 29
minutes at 1.7GB before it was stopped, and `Unsolved after 1001` with a
capped bound. Bisected:

| commit | forced-mutual `wsim_lts_bigstep` |
| --- | --- |
| `acfd55c` (before Step 0) | **Solved after 396** |
| `ac98c3c` (Step 0) | Unsolved after 1001 |
| this change | Unsolved after 1001 (unaffected) |

So the previous entry's central claim — that with the mutual block open "a
different route cannot lose a closure, only reorder what gets proved" — is
**false**. The smaller-first tie-break reproduces the original 2026-09-29
non-termination on the very proof it broke then, merely moved to the
mutual path. It went unnoticed because Step 0 was verified only under
`Auto`, which keeps CADP nested; the default path is genuinely unaffected,
and all 27 default counts hold. It does mean a user-facing configuration
that worked (`MeBi Config Solver MutualCofix True` on CADP) now hangs, and
it undercuts the backlog's "fall back to the mutual block when the nested
one stalls" idea for Step 0's remaining half. Left in place pending a
decision (revert, or narrow the gate); see the backlog.

**Verification.** Six proof suites under `Auto`, 27 of 27 `Solved` with the
counts above. Forced mutual: `Test1`, `Test2`, `Test3`,
`Glued/MutualExclusion` all `Solved`; CADP `MutualExclusion` as tabled
(`Glued` not run — same pre-existing failure expected). `dune exec
test/tests.exe` 34/34, `test/satdiff.exe -- 200` matches its golden file,
`make dune` and `dune build @fmt` clean.

**Session tally:** Bug fix 1 · Docs 1 · Optimization 0 · New feature 0 ·
Refactor 0 · **Tooling 0.**

---

## 2026-10-01 — Step 0 reverted (code only), parked as a separate optimization

Branch `main` (on `fork`). Reverts the `src/proof_solver_step.ml` half of
`ac98c3c` at the user's direction; its log entry above is kept as the
record. The tie-break comment now says the gated retry was tried and
reverted, and that heuristic changes need the strategy forced both ways.
The idea is written up for a future session in the local note
`notes/7-inversion-tie-break.md` (history, the bisect, an unconfirmed
hypothesis for why it fails, candidate directions).

**Result.** Under `Auto`, 27 of 27 `Solved`:

| suite | counts |
| --- | --- |
| `Proc/Test1` | 114 105 106 109 22 21 (unchanged) |
| `Proc/Test2` | 112 112 112 84 112 84 (unchanged) |
| `Proc/Test3` | **1073** 387 519 519 603 211 331 603 331 |
| `CADP/Size1` | 268 396 268 396 81 63 (unchanged) |

`Test3` returns to its pre-Step-0 figures except `wsim_p3`, 1127 → 1073 —
the reflexive-pair fix's own contribution, now measured without the
tie-break on top. Forced `MutualCofix True` (bounds capped at 1000):
`Test1`, `Test2`, all three CADP files `Solved` with the same counts,
CADP `wsim_lts_bigstep` back to **396**. `CLAUDE.md` updated (baseline,
loose `wsim_p3` bound, and verify-both-ways guidance; `tests.exe` expectation
30 → 34).

**A slip in the previous entry.** It reported `dune build @fmt` clean for
`6f06ed9`; one line in `proof_solver_step.ml` was not. Formatted here.

**Session tally (2026-10-01, this session):** Bug fix 1 · Refactor 0 ·
Optimization 0 (one reverted) · Docs 2 · New feature 0 · **Tooling 0.**

---

## 2026-10-01 — B3: extraction was quadratic in evar names; `Test4` now extracts in 25s

Branch `main` (on `fork`). Optimization. Backlog item B3 (`Proc/Test4`).

### Sizing first

`Test4/TermTests.v`'s `### Success` tag was **stale**: it fails at its first
command (`MeBi Run FSM p`) with `LTS_Incomplete` at the default 100-state
bound, in half a second. Enumerating `Proc.Layered`'s rules directly (a
throwaway Python model of `termLTS`/`compLTS`, rule for rule) gives the exact
reachable space: **9720 states, 87,480 transitions, 85% silent**, the same
set from `p`, `q` and `r`. That is 3⁴ local configurations × 120 tree shapes
(4! orderings × Catalan(3) bracketings, all silently interconvertible via
`do_comm`/`do_assocl`/`do_assocr`); in general 3ᵏ·(2k−2)!/(k−1)! for k
components — 18, 324, 9720, 408,240.

9720 states is not large. But extraction time per bound was 250 → 3.5s, 500
→ 35s, 1000 → 295s: ~9× per doubling, extrapolating to days for the whole
LTS. So B3 was *not* purely a state-space limit.

### Finding the cost — one wrong hypothesis on the way

- Plugin-level trace counts (`Logger.trace`) grow linearly with the bound, so
  the extra cost was in per-call duration, not call count.
- **Wrong hypothesis: the evar map.** `fresh_evar` writes into the monad's
  main `sigma` and nothing ever removes them — 41,616 evars after 500 states.
  Running each state's exploration under `M.sandbox` kept `sigma` at zero
  evars with a byte-identical FSM, **and no speed-up** (34.3s → 33.7s at
  600 states). Real, but irrelevant.
- **The cause: `Rocq_utils`' evar-name cache.** It kept the set of every
  evar name ever issued and asked `Namegen.next_ident_away` for one not in
  it. That function restarts its search from the base name (`UnifEvar0`)
  whenever the candidate is taken — which it always was — so each new name
  probed *every name before it*, at ~80 evars per state. Resetting the cache
  per state: 34.3s → 0.76s, byte-identical output.

### The fix

The cache is replaced by a counter. The old code's sequence was always
`UnifEvar0, 1, 2, …` (it scans to the first gap, and there never was one),
so the counter produces **the same names**: same freshness guarantee, O(1).
`the_cache`/`the_prev`/`the_default_next` removed from `rocq_utils.mli`;
nothing else used them.

**Result.**

- `Test4` 600-state extraction **34.3s → 0.75s**, byte-identical FSM output
  (1.46MB compared).
- **The complete 9720-state `Test4` LTS extracts in ~25-27s** (1.6GB peak).
- The 27 proof counts are identical (as the identical names predict); wall
  time `Proc/Test3` 17.8s → 8.5s, `Proc/Test2` 3.4s → 1.6s, CADP unchanged.
  The name set was also never reset between commands, so the cost had been
  accumulating across every command in a file.

### What is still open on B3 — and a mistake

Saturating the full LTS (`MeBi Run Saturate p`) ran out of memory. I ran it
**without a memory cap**, and it took the whole machine (and the session)
down. Every 120 tree shapes of a local configuration are silently
interconvertible, so each state's silent closure is large and the saturated
LTS plausibly has orders of magnitude more weak transitions than the 87k
strong ones. `CLAUDE.md` now says to cap any `Test4` run.

**Sized afterwards** with the same throwaway model of the rules, via the
silent-SCC quotient (a per-state version timed out): **81 silent SCCs of
exactly 120 states each** — one per local configuration, holding every tree
shape. That gives 18.7M weak silent pairs and **112M weak visible
transitions** against 87k strong ones, a ~1280x blow-up; at the plugin's
per-transition cost (annotations, constructor trees) that is tens of GB. So
the out-of-memory was inevitable for *explicit* saturation and is not a
saturation bug. Getting `Test4` through would mean saturating over the
silent-SCC quotient (81 nodes) rather than over states — a redesign of what
saturation hands to everything downstream, which all reads concrete states.
Recorded as design work in the backlog, not started.

**Decision (user, 2026-10-01): `Test4` stays a documented technical limit**,
to be revisited once every other outstanding item is resolved. Documented
where it will be met: `_CoqProject`'s `Test4` lines (the stale `### Success`
on `TermTests.v` replaced with `### KNOWN LIMIT (B3)`) and a header comment
in `examples/Bisimilarity/Proc/Test4/TermTests.v` giving the numbers, the
memory-cap warning and what fixing it would take. Docs.

**Verification.** Six suites under `Auto`, 27 of 27 `Solved`, counts
identical. `dune exec test/tests.exe` 34/34, `make dune` and `dune build
@fmt` clean. Experiments (probe, sandbox and name-reset switches) removed.

**Session tally (2026-10-01, this session):** Bug fix 1 · Optimization 1
(one reverted) · Docs 3 · Refactor 0 · New feature 0 · **Tooling 0.**

---

## 2026-10-01 — A2 closed: a positive test for the "multiple actionpairs" branch

Branch `main` (on `fork`). Tooling (a regression test). Closes backlog
item A2 and its `TODO.md` entry.

**The earlier investigations were looking in the wrong place.** The
2026-09-27 and 2026-09-28 passes reasoned about saturation's
`ActionPair.try_update` collapse and the cross-FSM `ActionMap.merge`. But
`ReModel.transition` is only called from `Hyps.get_transition (W.get_fsm_a
())`, and `get_fsm_a`'s `saturated` defaults to `false`: the lookup reads
FSM a's **original, unsaturated** edges. There, `EdgeMap.of_transitions`
gives every extracted transition its own `Action.t` with `trees = {tree}`,
so two *derivations* of the same strong step `from -label-> goto` are two
actions both containing `goto`. Neither saturation nor merging is involved.
The 2026-09-28 lead also had an unexamined problem: it relied on a
truncated LTS, which `MeBi Sim Begin` rejects with `LTS_Incomplete` by
default.

**The test.** `theories/Test.v`'s existing `BisimTest3` LTS already has the
shape: `do_par1` and `do_par2` coincide when `a = b`. A new module
`MultipleDerivations` proves `weak_sim termLTS termLTS (tfix (tpar A A
trec)) (tfix (tact A (tact A trec)))`:

- with `Trace` on, `multiple actionpairs matched (2 candidates)` fires
  twice and the proof closes in 38 iterations;
- **negative control:** with the pre-`6124eeb` behaviour (raise on more
  than one candidate) patched back in, the same proof fails at that branch.
  Patch reverted.

`Test.v` is compiled by every `dune build`, so CI now covers the branch
without touching `_CoqProject`, `dune` or the `.mlpack` (cf. C3).

**Tooling note.** `ulimit -v` cannot cap a Rocq run: OCaml 5 reserves its
heaps up front and dies with "Not enough heap memory to reserve minor
heaps". `systemd-run --user --scope -p MemoryMax=6G -p MemorySwapMax=0`
works; that is what the B3 runs should have used.

**Verification.** `dune build` and `make theories/Test.vo` clean; `make
dune` afterwards. No plugin code changed.

**Session tally (2026-10-01, this session):** Bug fix 1 · Optimization 1
(one reverted) · Docs 3 · Tooling 1 · Refactor 0 · New feature 0.

---

## 2026-10-01 — `LTS_Incomplete` explains itself; limit diagnostics recorded as a backlog item

Branch `main` (on `fork`). Prompted by the user asking whether the tool can
passively warn when a term/LTS will exceed its limits, as `Test4` did.

**What exists already.** Extraction is guarded: the state/transition bound
stops it and raises `LTS_Incomplete`. But the message was literally
`"TODO..."`. **Bug fix:** `check_if_lts_fail` (`src/wrapper.ml`) now says,
e.g. on `Test4` at the default bound: *"exploration stopped at the bound of
100 states, with 106 states and 180 transitions found and more still
unexplored. Raise the bound with [MeBi Config Bounds As Num States <n>] (or
[... Num Transitions <n>]), or accept a partial LTS with [MeBi Config FailIf
Incomplete False]. A large LTS can still be too big to saturate."*

**What does not exist, and was not built.** Nothing guards saturation,
which is where `Test4` actually died. A pre-saturation estimate is cheap
and exact once the LTS is known (silent-SCC quotient plus reachability over
the SCC DAG — 81 SCCs for `Test4`), but it is a new check with a likely new
config knob, i.e. a new capability, so per `CLAUDE.md` it is written up as
backlog item H2 (with H3: other limits that surface badly, such as
`PairNotInProduct` escaping as an `Anomaly`) rather than built. Estimating
from the term *before* extraction is not possible in general — the LTS is
an arbitrary inductive relation — and the extraction bound is already the
right guard there.

**Verification.** Message checked on `Test4/TermTests.v` (copied into a
scratch tree, memory-capped). `dune build`, `dune build @fmt` clean. No
proof-solver code touched.

**Session tally (2026-10-01, this session):** Bug fix 2 · Optimization 1
(one reverted) · Docs 3 · Tooling 1 · Refactor 0 · New feature 0.

---

## 2026-10-01 — H3: `PairNotInProduct` becomes a user error; `CADP/Size2` re-measured

Branch `main` (on `fork`).

**Bug fix: no more "Anomaly" for a pair missing from the mutual block.**
`PairNotInProduct` was a local exception with no `CErrors` handler, so Rocq
reported it as `Anomaly "Uncaught exception ..." Please report at
rocq-prover.org/bugs` — blaming Rocq for a plugin-side inconsistency. It is
now a `CErrors.user_err` that prints both goal terms, says it is a bug in
`Model.Product`, and names the workaround (`MutualCofix False`). The
exception, never caught anywhere, is removed. Checked by forcing the path
(an empty block, temporarily) on a scratch proof:

```
Error:
MeBi: reached a weak_sim goal for a pair outside the mutual cofix block
computed up front, so the proof cannot close it:
  (tpar A A (tfix (tpar A A trec)))
  (tfix (tact A (tact A trec)))
This is a bug in the plugin's product computation (Model.Product).
[MeBi Config Solver MutualCofix False] avoids the mutual block.
```

**Measurement: `CADP/Size2`**, recorded as "FAIL: state-explosion" before the
evar-name fix. Extraction of `c2` is now linear — 2500 states in 67s/2.1GB,
5000 in 135s/3.5GB (~27ms and ~0.56MB per state) — and still incomplete at
5000. So the label was right, but the binding constraint is now memory, not
time: ~9k states fit in a 6GB cap, ~24k in 15GB. The per-state memory
(4x `Test4`'s, CADP terms being larger) is a plausible future optimization
target; not investigated. `_CoqProject`'s comment updated with the numbers.

**Verification.** Six proof suites not re-run: the change is confined to an
error path no passing proof reaches. `dune build`, `@fmt` clean; error
message checked as above.

**Session tally (2026-10-01, this session):** Bug fix 3 · Optimization 1
(one reverted) · Docs 3 · Tooling 1 · Refactor 0 · New feature 0.

---

## 2026-10-01 — C3: a CI check that the three module lists agree

Branch `main` (on `fork`). Tooling. Backlog item C3.

The OCaml sources are listed by hand three times — `_CoqProject` (for
`make`), `src/mebi_plugin.mlpack` (for linking under `make`) and each dune
`(modules ...)` — and drift between them has already broken a build twice
(one build passes, the other fails). Rather than restructure the build,
`scripts/check_module_lists.py` (stdlib-only Python) compares all three to
the `.ml`/`.mli`/`.mlg` files under `lib/` and `src/` and names every
mismatch. It runs as CI's first step, before the opam bootstrap, so drift
fails in seconds. Set membership only — link order is left to `make`, which
reports a misordering loudly itself.

**It found real drift on its first run:** `Wip_annotation`, `Wip_trace` and
`Wip_traces` were still in the `.mlpack`, two days after `lib/model/wip/`
was deleted (A3). Removed; `make` links `mebi_plugin.cmxs` without them.
Two false positives were fixed in the script on the way (interface-only
`base_.mli` is a module; the `rocq.pp` stanza's `(modules g_mebi)` is not a
library claim). Negative-tested both directions: a stray `.ml` is reported
by all three checks, a ghost `.mlpack` entry by one.

`CLAUDE.md` now says to run the script whenever a module is added, renamed
or deleted.

**Verification.** Script passes (56 modules, 103 files); `make
src/mebi_plugin.cmxs` and `dune build` clean.

**Session tally (2026-10-01, this session):** Bug fix 3 · Optimization 1
(one reverted) · Docs 3 · Tooling 2 · Refactor 0 · New feature 0.

---

## 2026-10-01 — C2: `lib/dune` deleted

Branch `main` (on `fork`). Refactor (dead-file removal). Backlog item C2.

`lib/dune` was 9 commented-out lines declaring an umbrella library
`rocq-mebi.mebi_lib` that re-exported the four `lib/` libraries. Its history
settles what it was for: when the sub-libraries were namespaced
`rocq-mebi.mebi_lib.{utils,terms,rocq_tools,model}`, it was their parent.
`148ddbb` (2026-03-30) renamed them to `rocq-mebi.{utils,terms,...}` and
commented the umbrella out in the same commit; it was never revived, and
nothing in the tree references `mebi_lib`. `src/dune` already depends on
each library by name, so finishing it would only add an unused alias.
Deleted.

**Verification.** `dune build`, `dune exec test/tests.exe` 34/34,
`scripts/check_module_lists.py`, `make src/mebi_plugin.cmxs
theories/Test.vo`, then `make dune` — all clean.

**Session tally (2026-10-01, this session):** Bug fix 3 · Optimization 1
(one reverted) · Docs 3 · Tooling 2 · Refactor 1 · New feature 0.

## 2026-10-01 — H2: a size check before every saturation; H3's remaining messages

Branch `main` (on `fork`). Backlog items H2 and the rest of H3. **New
feature** (H2: a new check and two new config commands), raised with the
user before any code was written. They chose: **refuse by default**, mirroring
`FailIf Incomplete`; a **configurable bound** documented with example values
and memory; and the H3 remainder in the same session.

**`Model.SaturationEstimate`** (`lib/model/algorithms/saturation_estimate.ml`,
pure OCaml). Counts the weak actions `FSM.saturate` would materialise (one per
distinct `(from, a, goto)`) exactly, without saturating. It works on the
quotient by silent SCCs: Tarjan, `tau*`-reach as bitsets over the SCC DAG,
then a per-label DP for `tau* a tau*`. The Python prototype unioned reach sets
per member; the DP avoids its cubic worst case on long silent chains.
`tests.exe` gains 7 checks (34 → 41). The one that matters is differential:
the estimate equals the triple count of real saturation on 300 seeded random
LTSs (>250 of them non-trivial, which is also checked).

**The guard.** `Wrapper.check_saturation_size` runs after FSM construction in
`do_saturate`, `do_minimize` and `do_check_bisim` (both FSMs; `Sim Begin`
reaches it through `check_bisimilarity`). It is a no-op without weak labels.
Above `MeBi Config Bounds Saturation <n>` (default 1,000,000; reset by `Reset
Bounds`) it raises `Saturation_Too_Large`, naming the count and a memory range.
`MeBi Config FailIf Oversaturated False` downgrades that to a warning. Each
estimate is logged at `Info`.

**Measurement behind the numbers.** Heap growth across `FSM.saturate`
(full-major GC either side), on partial LTSs cut short by the state bound:
`Proc/Test4` at 251/501/1001 states gave 427/439/443 bytes per weak action;
`CADP/Size2` at 500/1000/2000 states gave 555/648/859. The second set grows
because each action keeps its shortest witness path. So the message and
README quote **450–900 bytes**, and the README has a table of bounds against
memory. Time is **super-linear** in the count: 20k/82k/400k weak actions on
`Test4` took 0.7/6.3/141s, against 0.5/1.1/5.2s for 7k/17k/70k on CADP. The
likely cause is that `Saturation.edge_closure` recomputes each target's
silent closure per visible move. That is noted as a possible optimization,
not done here.

**A correction.** The backlog and `Test4`'s header said its saturation is
"~112M weak transitions", from the throwaway Python model
(`notes/tools/test4_saturation_estimate.py`). The plugin's own figure is
**74,649,600** (7680 per state, against Python's 11,520). The estimator is
verified against real saturation, so the plugin's number is what it would
build. The gap most likely lies in how the Python model distinguishes
labels; I did not chase it. The header, the `_CoqProject` comment and the new
docs now carry the plugin's figure. With the full 9720-state LTS (`Bounds As
Num States 12000`), `MeBi Run Saturate p` now refuses after ~27s of
extraction under a 6GB cap. Before, it took the machine down.

**H3, `Sim Solve` exhaustion.** When `MeBi Sim Solve N` stops on its bound
unsolved, a second notice now says that `N` permits `N + 1` steps, which
cofix strategy is in force and whether `Auto` chose it, and the exact
`MutualCofix True|False` command to force the other. The `(Stopped) Unsolved
after N iterations.` line is byte-for-byte unchanged, since the baseline
greps it. Bug fix (message).

**H3, extraction memory.** `MeBi Config Bounds As Num States <n>` prints a
notice when `n` × 0.56MB (measured, CADP; `Test4` is 0.16MB) passes 1GB,
giving the range. A notice, not a refusal: the bound is an upper limit, and
most LTSs never reach it. Bug fix (message).

**Verification.** `tests.exe` 41/41. End to end against dune's plugin build:
`Test4` refuses as above. Test3 with `Bounds Saturation 50` refuses on
`Saturate` and `Bisim`, and warns then proceeds under `FailIf Oversaturated
False`. Test1 `Solve 10` prints the new hint under `Auto` (nested) and forced
`True` (mutual). Proof suite: all 27 `Solved after` counts unchanged under `Auto` (the check is read-only; no solver heuristic changed, so forced `MutualCofix` runs were not needed).

**Session tally (2026-10-01, this session):** New feature 1 (H2) · Bug fix 2
(H3 messages) · Docs 1 (README, the `Test4` figure, this log's stale "none
has" line) · Refactor 0 · Optimization 0 · Tooling 0.

## 2026-10-01 — 3b: saturation's per-state dedup made linear; closures memoised

Branch `main` (on `fork`). **Optimization**, plus **Tooling** (a new
`test/satscale.ml` shape). A new backlog item that H2's measurement turned up.

**The cost.** `Saturation.edge_closure` collected every witness `from -tau*->
s -a-> t -tau*-> goto` for a state, then deduplicated them with
`ActionPair.merge_lists`. That makes a linear pass over the survivors so far
for each witness, i.e. (witnesses) × (distinct weak actions) per state. It
also recomputed `t`'s silent closure for every visible move into `t`.
`satscale` gains the `Test4` shape (chained silent cycles, `m = 20`): 211,200
weak actions took **468s**.

**The change.** Dedup goes through a hashtable keyed on `(label, goto)`.
Silent closures are memoised per saturation. Witness lengths are compared
before any witness is built, so the note list and annotation are only
allocated for a witness that replaces the one held. The survivors are exactly
the old ones, which needed care. I first misread `Annotation.shorter` as
keeping its first argument on a tie, and corrected that before writing the
code. In fact `merge_lists` saw witnesses newest-first and kept `shorter
existing incoming`, which returns `incoming` on a tie. So the shortest
witness wins, ties go to the earliest generated, and the label comes from the
latest (`Label.equal` ignores `is_silent`). The code reproduces that.

**Results.** `satdiff -- 200` is byte-identical to `test/satdiff.expected`
(2426 lines: every weak transition, destination set and surviving
annotation). The 211k-action `satscale` case runs in **19s** (24×), and the
grid and chain controls are unchanged. In the plugin, partial `Test4` LTSs at
250/500/1000 states saturate in 0.54/2.3/**13.3s**, down from 0.73/6.3/141s
(10.6× at 1000). The 2000-state partial (1.8M weak actions) is now refused
by H2's guard, as it should be. Proof suite: all 27 counts unchanged under
`Auto`. `tests.exe` 41/41. Forced-`MutualCofix` runs were skipped: no solver
heuristic changed, and satdiff shows the solver's input is identical.

**A mistake on the way.** The first proof-suite run failed to build: the
`ActionPair` alias became unused, and `make` treats warning 60 as an error
where `dune build` does not. Fixed by removing the alias, then the suite was
re-run.

**Not done.** Time is still about ×8 per doubling of `k`, because this shape
has ~43M witnesses at `k = 32` and each now costs ~0.44µs. Going linear in the
output means a BFS over `(state, before/after the visible step)` per `from`.
That would pick *different* witnesses among equal-length ones, i.e. change
the annotations the proof solver reads. It is left for discussion, not done
silently.

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 2 · Optimization 1 · Tooling 1 · Docs 1 · Refactor 0.

## 2026-10-01 — A4: the "duplicate things to unfold" TODO, measured and closed

Branch `main` (on `fork`). **Docs** (a comment). Backlog item A4. No code
change.

`try_unfold_any` carried `TODO: make sure you remove duplicate things to
unfold`. Within one term, duplicates are impossible: `collect_component_econstrs`
returns an `EConstrSet`. The only place several terms are combined is
`try_unfold_any_of`. It has one call site, `handle_hyp_transition`, on the
conclusion's `wk_trans` and `wk_sim`, and there a shared constant *would* be
unfolded twice. `Hyps.try_unfold_any` also chains, but per hypothesis, so
repeats there target different hypotheses and are not duplicates.

Measured with temporary instrumentation (since reverted) over the full proof
suite, all 27 `Solve`s: 829 calls, and the two terms **never** share an
unfoldable constant. Fourteen calls find 6 distinct constants; the other 815
find none. Adding a dedup would change tactic structure, i.e. the order of
the `unfold`s, for no measured benefit. So the TODO became a comment
recording the measurement and the one place to look if it ever matters.

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 2 · Optimization 1 · Tooling 1 · Docs 2 · Refactor 0.

## 2026-10-01 — Extraction stops keeping matching evars; my per-state memory figure was wrong

Branch `main` (on `fork`). **Optimization** (`src/graph_builder.ml`) and a
**Bug fix** to figures I introduced earlier the same day. Backlog item
"per-state extraction memory".

**Where the memory went.** Temporary probes in the graph builder (since
removed) logged live heap, evar counts and the reachable size of the evar map
and encoding table every 250 states. On `CADP/Size2` the evar map was ~88% of
live-heap growth: ~180 evars per state, 80% undefined, 590k at 4000 states.
On `Proc/Test4` at 2000 states it held 87.5MB of a 110MB live heap, against
4.2MB for the encoding table. They come from matching each state against
the LTS's constructors (`mk_ctx_substl`, the per-constructor `fresh_evar`,
LHS unification), all in the one command-wide evar map. (I briefly suspected
`Rocq_monad.sandbox` of leaking updates. It doesn't: `state` swaps in a new
context ref rather than mutating the shared one.)

**My earlier figure was wrong.** H3's notice and the README quoted
0.16–0.56MB per extracted state. The 0.56 came from `CADP/Size2/TermTests.v`,
which enables `Output "Result"`, `"DecodeResults"` and `"DumpResults"`.
Probing showed the OCaml heap peaking at 246MB during extraction, then
jumping to 1.6GB while the result was pretty-printed and serialized. With
those outputs off, the same run peaked at 0.75GB, not 1.9GB. So most of the
"extraction" cost I measured was result logging. The `_CoqProject` comment
on `CADP/Size2` (3.5GB at 5000 states) had the same confusion.

**The change.** `get_new_constrs` runs each state's constructor collection
in `M.sandbox`. That restores the evar map and keeps the encoding tables,
which are mutated in place. This is safe only if nothing that escapes refers
to those evars, so I measured that: across CADP/Size2 (7008 encoded terms),
Test4 (2672), Test3 and CADP/Size1, no encoded term contains an evar. The
code also checks per state: if a found term ever does contain one, the inner
evar map is kept, which is the old behaviour.

**Results** (clean A/B, no probes, same machine):

| run | time | peak RSS |
| --- | --- | --- |
| `Test4`, full 9720 states, before → after | 24.7s → 16.7s | 1.44GB → 0.40GB |
| `CADP/Size2`, 5000 states, before → after | 13.0s → 10.9s | 0.81GB → 0.49GB |

The live heap fell from 412MB to 168MB on CADP/Size2 at 4000 states, and
from 110MB to 22.5MB on Test4 at 2000 states. Test4's LTS is unchanged:
9720 states, 87,480 transitions, 81 silent SCCs, 74,649,600 weak actions.
Proof suite: all 27 counts unchanged, `theories/` builds, `tests.exe` 41/41.

**Corrected figures.** `Api.mb_per_extracted_state` is now 0.01–0.07MB on
top of a fixed ~0.1–0.3GB, so the notice fires from ~14,000 states, not
~1,800. The notice and README now also say that result logging costs ~0.65MB
per state. The `CADP/Size2` comment in `_CoqProject` is corrected.

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 3 (one fixing my own earlier figure) · Optimization 2 · Tooling 1 ·
Docs 2 · Refactor 0.

## 2026-10-01 — I2: extraction now says when it skips a premise

Branch `main` (on `fork`). **Bug fix** (a warning; no change to what is
extracted). Backlog item I2, raised by the user as a wider survey of
unsupported constructor shapes, which is still to do.

**The problem, confirmed by test.** `check_updated_ctx` walks every binder of
a constructor. A binder whose type's head is one of the `Using` LTSs is
explored as a premise. Any other is passed to `check_unknown_app`, which
logged at `Debug` and carried on. That is right for a data binder (`xs :
list nat`). For a *premise* like `n = 0`, though, the constructor is applied
whether or not the premise holds. `Inductive st : nat -> bool -> nat -> Prop
:= go n : n = 0 -> st n true (S n)` extracts `0 -> 1 -> 2 -> ...` up to any
bound, instead of the single transition `0 -> 1`. So the LTS
over-approximates, and a `MeBi Run Bisim` verdict on it can be wrong. A
proof cannot be: `Qed` checks the premise.

**The change.** `warn_if_skipped_premise`, called from `check_unknown_app`:
if the skipped binder's type is a proposition (its sort is `Prop`, via
`Retyping.get_sort_quality_of`), emit a `Warning` naming the LTS, the
premise's head and the first instance met. It fires once per (LTS, head),
not per state, using a table in `Unification`. My first wording printed the
first instance as if it were the premise (`0 = 0`, which happens to be
true); reworded before committing.

**Verification.** In a scratch file, the `eq` premise warns once; a
`list nat` data binder and a premise over the `Using` LTS itself stay quiet.
No existing example triggers it: the full proof-suite build and the
`TermTests.v` of `Proc/Test1-3` and `CADP/Size1` all give 0 warnings. So
none of the repository's LTSs were being over-approximated this way. Proof
suite: 27 counts unchanged.

Also a separate `style:` commit: `40c98d5` went in without `dune fmt`
(CI does not check formatting).

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 4 · Optimization 2 · Tooling 1 · Docs 2 · Refactor 0.

## 2026-10-01 — E(a): regression tests for the saturation guard and the unchecked premise

Branch `main` (on `fork`). **Tooling** (tests only). Backlog item E,
option (a).

Two modules appended to `theories/Test.v`, so they run in every `dune build`:

- `SaturationGuard` reuses `MultipleDerivations`' LTS (4 weak actions) with
  `Bounds Saturation 1`. It checks that `Saturate`, `Minimize`, `Bisim` and
  `Sim Begin` are refused; that `FailIf Oversaturated False` lets `Saturate`
  through and `True` refuses again; and that `Reset Bounds` restores the
  default.
- `UncheckedPremise` is I2's `n = 0` example as a documented
  **known-wrong** test: `Fail MeBi Run LTS 0 Using st` at a 20-state bound.
  If such premises are ever supported, it starts failing, deliberately.

`Fail` accepts any error, so each of the six was checked for the right one:
a copy of `Test.v` cut off at that line, with the `Fail` removed, gives
`Saturation_Too_Large` for the first five and `LTS_Incomplete` for the
premise case. Not covered, because a `.v` file cannot assert a plugin
`Warning` or `Notice`: the premise warning's text, `Sim Solve`'s exhaustion
hint, and the large-bound notice.

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 4 · Optimization 2 · Tooling 2 · Docs 2 · Refactor 0.

## 2026-10-01 — E(b): pinned extraction and saturation sizes

Branch `main` (on `fork`). **Tooling** (tests only). Backlog item E,
option (b).

No command reports an LTS's size, but the bounds can be used as
assertions: with `FailIf Incomplete` on (the default), `Bounds As Num States
n` succeeds iff the LTS has at most `n` states. So "succeeds at `n`, fails
at `n - 1`" pins the count, and likewise for transitions. `Bounds
Saturation` (H2) pins the weak-action count the same way.

`theories/Test.v` gains `ExtractionSizes`: a process LTS that recurses on
itself (interleaving `ppar`, a silent `p_tidy`), and a system LTS whose
premise is over that *different* LTS, which is the layered shape of `Proc`
and `CADP`. Until now that shape was exercised only by `examples/`, which
dune does not build. It pins two terms in each LTS (states, transitions)
and two saturation sizes: 8/10, 21/38, 9/11, 22/39, and 11 and 61 weak
actions. The `p1` figures (8 states, 10 transitions, 11 weak actions) are
**counted by hand**, so they check that the LTS is right, not merely that
it is unchanged. The other figures were found by searching for the least
succeeding bound. Each of the 10 `Fail`s was checked to fail for the
intended reason (8× `LTS_Incomplete`, 2× `Saturation_Too_Large`).

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 4 · Optimization 2 · Tooling 3 · Docs 2 · Refactor 0.

## 2026-10-01 — E(c): `lib/terms` unit tests; a two-premise constructor cannot be proved

Branch `main` (on `fork`). **Tooling** (tests only). Backlog item E,
options (c) and (d). The finding below is not fixed.

**`lib/terms` tests** (`tests.exe` 41 → 58). Covered: tree
equality/order, `Trees` dedup, `Constructor_tree` equality; `Tree.minimize`
(a chain flattens root-first, the shortest child is kept, ties go to the
first) and `Trees.min`/`min_opt`; and the encoding counter (`incr`/`reset`)
that `Bi_encoding` hands encodings out from. Only functions with callers are
pinned. `Tree.add`, `Tree.add_list` and `Tree.min` have none outside
`lib/terms`; `add` appends its argument at *every* level of the tree, which
I could not tell was intended. They are dead-code candidates, not tested.

**The finding.** Working out what `Tree.minimize` is *for* turned up a gap.
The solver applies its result node by node
(`handle_appconstrs_update_args`), and `minimize` treats a node's children
as **alternatives**, keeping the shortest. In extraction
(`Constructors.retrieve`), though, a node's children are one derivation
**per premise**, all required. No constructor in the repository has two LTS
premises: a regex scan first flagged `DevTest`'s `do_comm` and CADP_simple's
`LTS_PAR_R`, which were false positives. So I wrote one, a `sync` rule where
both sides of `ppar` must step. Extraction is fine. The proof fails: the
solver proves the first premise, is left with the second (`stepLTS … (Some
A) ?q'`) and tries `rt1n_refl` on it. This is now `TwoPremises` in
`theories/Test.v`, a documented known-wrong `Fail` (checked to fail with
that unification error). Fixing it means the solver walking the derivation
tree instead of a flat list: a proof-solver change, logged as backlog item
A6, not started.

(d) — `lib/rocq_tools` and `src` can only be tested through `.v` files;
(a), (b) and `TwoPremises` are that. Nothing further is planned under E
without a specific target.

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 4 · Optimization 2 · Tooling 5 · Docs 2 · Refactor 0.

## 2026-10-01 — A6: constructors with several LTS premises can now be proved

Branch `main` (on `fork`). **Bug fix** (proof solver, `lib/terms`).
Backlog item A6; design and spike in `notes/8-two-premise-constructors.md`.

**The bug.** The solver replays a transition's derivation tree as a flat
list of constructors, one `constructor i` per step on the focused goal. The
list came from `Tree.minimize`, which keeps a node's *shortest child*, as if
children were alternative derivations. They are not: a node's children are
its constructor's LTS premises, all required (alternatives live in `Trees`,
one level up). With two premises the solver proved the first, ran out of
constructors, and tried `rt1n_refl` on the second.

**The fix.** `Tree.preorder` (depth-first, left to right) replaces
`minimize`; `Trees.min` ranks alternatives by `Tree.size` (constructors to
apply) instead of minimized length. `minimize`, the unused `Tree.min` and
`CannotMinimizeEmptyList` are gone.

**Validated before building**, each claim against a test that could
falsify it (spike with a switchable `minimize`/`preorder`/`reversed`
replay, since reverted). The dumped tree's children are in premise order.
A hand replay in Ltac shows Rocq focusing premise goals left to right
(`Qed` accepts). Two LTSs, three LTSs at constructor indices 0/1/2, and a
nested two-premise node all pass with `preorder` and fail with
`reversed`, so the tests are order-sensitive and the order is right.

**Verified after building**, including three checks the spike could not
make, written down beforehand as a checklist in note 8 (R1–R3):

- R1, R2: temporary instrumentation in `Trees.min`, through which every
  tree choice passes (the solver, `Product.respond`,
  `proof_solver_step.ml:236`). It found **0** trees with a multi-child node
  anywhere, and **0** choices where the new ranking differs from the old,
  over ~2,300 calls per mode.
- R3: a temporary environment override forcing `MutualCofix` past the
  per-file settings. Old code and new code were each run under `Auto`,
  `True` and `False`, and are **identical per mode**: `Auto`'s 27 counts;
  forced nested Test2 `446 278 299 194 446 182`; forced nested Test3
  stops at 1127 in both. One thing gave me pause: forced `True` equals
  `Auto` on Test1/CADP. The log's own 2026-10-01 entry explains it (since
  item 12, mutual Test1 is 22/21, the nested figures), so the override did
  apply.

Tests: `TwoPremises` in `theories/Test.v` is now positive, with the spike's
shapes (same LTS, two LTSs, three LTSs, nested) as regressions. It also has
a known-wrong case, below. `tests.exe` 58 → 61 (`preorder` on a chain, two
premises, nested; `size`; `Trees.min` by size). satdiff identical; module
lists agree; `make` builds plugin and `Test.v`.

**Mistakes on the way.** My first `tree.mli` doc comment had no blank line
after `val compare`. `make` rejects that (warning 50) and `dune build` does
not, so the first new-code half of the verification matrix built nothing
and had to be rerun. During the spike, a text edit went to the signature
instead of the implementation.

**Found, not fixed: `eq` premises break proofs, independent of A6.** Even
with one LTS premise, an `eq` premise *before* it is an Anomaly
(`Constructor_bindings…BindingInstruction_NotApp`: the next node is
applied to the `eq` goal), and one *after* it leaves the proof open. The
solver has no step for non-LTS premise goals; this belongs with I2.
`TwoPremises.wsim_eq` records it as known-wrong.

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 5 · Optimization 2 · Tooling 5 · Docs 2 · Refactor 0.

## 2026-10-01 — The `eq`-premise Anomaly becomes a user error

Branch `main` (on `fork`). **Bug fix** (error reporting only). Follow-up
to A6's finding.

With an `eq` premise before an LTS premise, the solver applied the LTS's
constructor to the focused `eq` goal. `Constructor_bindings` then raised
`BindingInstruction_NotApp`, which escaped as a Rocq **Anomaly** ("please
report at rocq-prover.org"), blaming Rocq for a plugin limitation.
`apply_constructor` now raises a dedicated `GoalNotAnLTSStep`, and
`handle_appconstrs_apply`, which has the goal, turns it into a user error
naming the focused goal and pointing at the extraction warning. My first
version caught it one level down and printed the sub-term where binding
extraction stopped (`act`, the type argument of `eq`), not the goal; I
moved the catch before committing. It now prints `(A = A)`.

Tests: `TwoPremises.wsim_eqfirst`, a second known-wrong case (`Fail MeBi
Sim Solve`, checked to fail with the new message). Proof suite: 27 counts
unchanged. `make` builds plugin and `Test.v`. The limitation itself (no
solver step for non-LTS premises) is unchanged; see I2.

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 6 · Optimization 2 · Tooling 5 · Docs 2 · Refactor 0.

## 2026-10-01 — Two small cleanups

Branch `main` (on `fork`).

- **Refactor**: `Tree.add`/`Tree.add_list` removed. They had no callers
  anywhere (found while writing the `lib/terms` tests; `add` appended its
  argument at every level of a tree, which nothing relied on). `make`
  builds, `tests.exe` 61/61.
- **Bug fix** (wording): `SaturationEstimate.to_string` said "the largest
  of 1 states"; now "the largest with 1".

**Session tally (2026-10-01, this session, cumulative):** New feature 1 ·
Bug fix 7 · Optimization 2 · Tooling 5 · Docs 2 · Refactor 1.

## 2026-10-01 — I2: equation premises are decided, and proved

Branch `main` (on `fork`). **New feature** (flagged before writing; the user
asked for it). Backlog item I2, step (1) of the ideas recorded there, plus
the deferral that step turned out to need. General proof search over
arbitrary premises, idea (3), is **not** built.

**Extraction** (`lib/rocq_tools/rocq_monad_utils.ml`). A premise not over a
`Using` LTS is now classified three ways, never guessed:

- `decide_premise` decides an equation whose sides are **closed** after
  `nf_all`. Convertible sides hold. A constructor difference, at the head or
  under matching constructors, means false (constructors are disjoint),
  so the constructor does not apply from this state. Anything else is
  undecided.
- An equation that is not closed when its constructor is matched is
  **deferred**, carried in a new `Problems.deferred` field, and decided
  after the constructor's LTS premises are unified
  (`sandbox_unify_all_opt`), or at once on the axiom path. A guard
  `a = A` on the label is the motivating case. The label is fixed only by
  the LTS premise, so deciding at match time saw `?a = A`, left it
  undecided, and kept the `B` step. That was my first version, caught by
  `TwoPremises.wsim_guard`'s size pin. `cross_product` drops each
  accumulated problem's fields in favour of the new premise's evar map, so
  `deferred` is copied across explicitly there.
- Only still-undecided premises warn (I2's warning, reworded).

**Proof solver** (`src/proof_solver_step.ml`). (a) In `ApplyConstructors`,
an equation goal with the focus is closed by `reflexivity` (new
`Tacs.reflexivity`), leaving the constructor list for the next goal.
Extraction only keeps an equation premise it decided holds, i.e. one with
convertible sides. (b) **`Hyp.invertibility` grades equation hypotheses
0.** It assumed every hypothesis was an LTS step and graded `a = a` (both
sides a local variable) 3. Inverting it changes nothing and `subst` cannot
remove it, so the solver inverted it **forever**: "Unsolved after 501" when
the `eq` premise came last. This changes which hypothesis is inverted, the
fragile choice Step 0 warns about, but only when an equation hypothesis
exists, which no example has.

**Verification.** All 27 counts unchanged under `Auto`. With `MutualCofix`
forced `True`/`False` (temporary override), identical to the per-mode
baselines measured for A6 (forced nested Test2 `446 278 299 194 446 182`;
Test3 stops at 1127). All parallel builds pass, so `Test.v`'s new proofs
hold under every mode. The eight spike shapes (multi-premise, and `eq`
before/between/after LTS premises) all prove. `tests.exe` 61/61; `make`
builds plugin and `Test.v`.

**Tests** (`theories/Test.v`): `UncheckedPremise` becomes
`DecidedPremise`, a positive size pin (`n = 0`: 2 states, 1 transition).
New `UndecidedPremise` is a known-wrong `Fail`: an equation over an opaque
`Parameter` cannot be decided, so the LTS over-approximates. In
`TwoPremises`, `wsim_eq` and `wsim_eqfirst` are now positive, and the new
`wsim_guard` checks that a false premise (`B = A`) blocks a step: a
2-state pin and a proof. Each `Fail` was checked for its reason
(`LTS_Incomplete`).

**Docs.** README gains "Which constructor shapes are supported".

**Session tally (2026-10-01, this session, cumulative):** New feature 2 ·
Bug fix 7 · Optimization 2 · Tooling 5 · Docs 2 · Refactor 1.

## 2026-10-01 — I1: is a `Test4` proof feasible at all? Measured: not soon

Branch `main` (on `fork`). Measurement only; no code committed (a
temporary print of `Product.estimate` for every proof, since reverted).
Backlog item I1 (the user's idea: saturate on demand, one state at a time,
so that `Test4` fits in memory).

On-demand saturation suits the proof solver, which only asks for one
state's weak moves at a time. But it only matters if the *proof* is
feasible, which can be bounded without building anything. Every left
transition from every reachable state must be matched at least once, so
`Test4` (9720 states, 87,480 transitions) needs ≥ 9,720 pairs and ≥ 87,480
moves. Iterations per move on the same family:

| proof | pairs | moves | iterations | per move |
| --- | --- | --- | --- | --- |
| Test3 `wsim_pq` | 42 | 117 | 1073 | 9.2 |
| Test3 (two more) | 16 / 18 | 48 / 54 | 387 / 519 | 8.1 / 9.6 |
| Test3 (two more) | 24 / 8 | 72 / 24 | 603 / 211 | 8.4 / 8.8 |
| Test1, Test2 | 3-13 | 3-20 | 21-114 | 5.6-7.0 |
| CADP `MutualExclusion` | 13 / 44 | 13 / 44 | 268 / 396 | 20.6 / 9.0 |

So `Test4` needs **≥ ~700k solver iterations** (8 × 87,480). Allowing for
Test3's pairs/states ratio of 1-2.3, that is up to ~1.5M. At Test3's
1.8 ms per iteration it takes **≥ ~21 minutes**, likely more given
`Test4`'s larger terms. `Qed` then has to check a coinductive proof over
≥ 9,720 pairs. The largest proof today is 1,073 iterations. On-demand
saturation would remove the memory wall and leave this one, so I did not
build it. `Test4` stays the documented limit, last in the order by the
user's earlier decision.

**Session tally (2026-10-01, this session, cumulative):** unchanged.

## 2026-10-01 — F: a working `MeBi Help`

Branch `main` (on `fork`). **New feature** (flagged before writing; asked
for by the user, who also asked that `MeBi Help Config Bounds Saturation`
carry the memory guidance). Backlog item F.

`MeBi Help` was a commented-out placeholder; the older implementation was
deleted on 2026-09-27 as it no longer matched the command syntax. New
`src/help.ml`: `MeBi Help` lists the topics, and `MeBi Help <topic>` covers
`Run`, `Sim`, `Benchmark`, `Premises` (supported constructor shapes, from
I2), `Config`, and `Config Bounds`, `Config Bounds Saturation`,
`Config Weak`, `Config FailIf`, `Config Solver`, `Config Output`. Each
topic has an explicit grammar rule, like the rest of `g_mebi.mlg`, so
there is no free-form parsing. The topic structure was my choice; the user
had left it open.

The memory figures are **computed from the same constants** the plugin's
error and notice use, so help and behaviour cannot drift.
`bytes_per_weak_action` and `human_bytes` moved from `Wrapper` to `Api`,
next to `mb_per_extracted_state`, to make that possible. `Config Bounds
Saturation` prints the bound-vs-memory table (1M → 450MB-900MB (default)
… 20M → 9-18GB) and the time caveat.

New module: `_CoqProject`, `mebi_plugin.mlpack` and `src/dune` updated;
`check_module_lists.py` agrees (58 modules). `theories/Test.v` runs every
topic. README: the "disabled" note is replaced by a Help section.

**Session tally (2026-10-01, this session, cumulative):** New feature 3 ·
Bug fix 7 · Optimization 2 · Tooling 5 · Docs 2 · Refactor 1.

## 2026-10-01 — F: `lib/model` docs; the bisimilarity check stops saturating twice

Branch `main` (on `fork`).

- **Docs.** `Bisimilarity` and `Minimization` had the fewest doc comments
  among `lib/model`'s interfaces (7 values each, 2-4 comments); both are
  now documented. The others were already covered. `odoc` is not installed
  in the switch, and installing it changes the dev environment, so I did not
  do it unasked. The comments were checked by reading, not by rendering,
  and `make` (which rejects ambiguous doc comments, warning 50) builds.
- **Optimization**, found while documenting. `Bisimilarity.fsm` saturates
  both FSMs, merges them, then called `Minimization.fsm`, which **saturated
  the merged, already-saturated FSM again**. That pass rebuilt every weak
  action of both FSMs, roughly doubling the check's saturation memory, and
  H2's guard does not cover it. The merged FSM has no silent edges left, so
  the second pass reproduces the same `(from, label, goto)` structure, and
  the partition reads only that. It now calls `partition_states` directly.
  Proof suite: 27 counts unchanged (the solver reads this partition);
  `tests.exe` 61/61.
- **Not done: CADP "no starvation".** `_no_starvation.v` is unfinished
  research content: a fairness heuristic (a process "recently" or
  "routinely" starves) whose intended formulation is in the draft paper. No
  starvation is a liveness property, and the plugin checks (weak)
  simulation/bisimilarity, which does not capture liveness without a spec
  LTS that encodes fairness. Deciding what the property *is* belongs to the
  authors, so it is left for the user / @dcastrop.

**Session tally (2026-10-01, this session, cumulative):** New feature 3 ·
Bug fix 7 · Optimization 3 · Tooling 5 · Docs 3 · Refactor 1.

## 2026-10-01 — Step 0 diagnosed; three more fixes tried; parked

Branch `main` (on `fork`). Investigation only; no code committed (a
reverted spike with environment-variable switches). Backlog item Step 0,
`notes/7-inversion-tie-break.md`, which now has the full record.

**Why the smaller-first inversion tie-break diverges**, confirmed by an
inversion trace on CADP `wsim_lts_bigstep`. `inversion H` keeps `H`;
smaller-first ranks `H` the smallest grade-3 hypothesis, none of its
results is smaller, so it picks **the same `H` every step**: 593 of 601
steps inversions, the last 300 all `H`. Today's later-first order avoids
this only because new hypotheses sort last. That answers the note's open
question. The mutual block was irrelevant: the loop never reaches a
`weak_sim` goal.

**Fixes tried, none viable.** (b) Capping consecutive inversions: healthy
CADP proofs already run ~28 in a row, so the cap would be ~30, likely
costing more than the ≤14% saved; not built. `inversion_clear`: removes
the transition hypothesis `get_transition` needs, so every suite fails.
Remembering inverted hypotheses per phase: Test1 and Test3 fail outright,
Test2 112 → 120, CADP 396 → 413/429. So some "sterile" re-inversions
were needed. A real fix needs the solver to model which re-inversions are
sterile, probably from the extracted constructor trees. That is design
work for a 7-14% saving, so it is parked.

**A mistake on the way:** my first run of the `inversion_clear` matrix
passed the two settings as one unsplit string (zsh does not word-split
unquoted variables), so both runs silently used the default solver and
"passed". I caught it from the identical counts and reran.

**Session tally (2026-10-01, this session, cumulative):** unchanged.

## 2026-10-01 — No plugin exception reaches Rocq as an Anomaly from `MeBi Sim`

Branch `main` (on `fork`). **Bug fix** (error reporting). Found along the
way: twice today a plugin-internal exception (`BindingInstruction_NotApp`,
then `CannotGetTransition` in the Step 0 spike) escaped as a Rocq
**Anomaly**, which tells users to report a bug in Rocq.

`Proof_solver.guard` now wraps the `MeBi Sim Begin`/`Step`/`Solve`
commands. An uncaught exception from inside the plugin is recognised by its
`Mebi_plugin.` name prefix, or as a stdlib `Not_found`/`Invalid_argument`/
`Failure`/`Assert_failure` escaping plugin code. It becomes a user error that
names it, says it is a MeBi problem, and points at `MeBi Help Premises`.
Exceptions with a registered printer (Rocq's own errors, tactic failures,
`MEBI_exn`) pass through unchanged. The guard is at the command level, not
in `step`, because `solve` uses `NothingToDo` for control flow.

Checked by injecting a `Not_found` (temporary): user error, not Anomaly.
Normal proofs are unaffected; proof suite 27 counts unchanged; `tests.exe`
61/61. Also folds in `dune fmt` of the new `minimization.mli` comments.

**Session tally (2026-10-01, this session, cumulative):** New feature 3 ·
Bug fix 8 · Optimization 3 · Tooling 5 · Docs 3 · Refactor 1.

## 2026-10-02 — I2 stage 1: general premises decided by bounded proof search

Branch `main` (on `fork`). **New feature** (flagged in the sketch, built on
the user's go-ahead), plus a **Bug fix** to yesterday's Anomaly guard.
Design: `notes/9-general-premise-support.md`, stage 1.

**Engine** (`lib/rocq_tools/premise_search.ml`, new module). `prove env
sigma goal` → `Proved p` (with a closed proof term), `Refuted`, or `Unknown`,
never a guess. It head-normalizes the goal, so `lt` (a definition) and `In`
(a fixpoint) unfold, and decides equations as extraction does. For any other
inductive proposition it tries each constructor: fresh evars for its
binders, unify its conclusion, search its `Prop` premises left to right,
depth − 1. The result typechecks before `Proved`. Two soundness rules,
both found by working cases, not by tests failing:

- A failed match refutes only if nothing opaque could hide a proof:
  **parameters** must be *evaluated* (no opaque constant, axiom, free
  variable or stuck match after normalization), and **indices** must be
  ground constructor terms. My first version demanded every argument be a
  constructor term, which wrongly made `Forall (fun k => k <= 1) [2; 2]`
  undecidable (its predicate is a parameter). `le (f x) 3` with an opaque
  `f` correctly stays undecidable: its parameter is stuck, and it could be
  provable by rewriting.
- Only the *first* proof of each premise is kept. So once a premise with
  open variables has been solved, a later failure in the same constructor
  is not a refutation (`R x y -> R y z -> R x z` with `y` free).

**Extraction**: `decide_premise` sends every non-equation proposition to
the engine. Open premises stay `Unknown` and are deferred, like open
equations.

**Proof solver**:

- A *premise goal* (a `Prop` headed by an inductive that is not `eq`, a
  MeBi theory constant, either FSM's LTS, or a `clos_*` relation) is closed by
  `exact` of the engine's proof.
- `invertibility` now inverts only LTS hypotheses. A premise hypothesis
  that the engine refutes gets the top grade (4) and is handled by `simpl
  in H; inversion_clear H`: `simpl` so a fixpoint like `In` unfolds,
  `_clear` so it cannot be re-picked forever (the Step 0 loop).
- **A mistake on the way:** my first version graded every non-LTS
  hypothesis 0. That broke the proofs at the first state whose guard is
  false (`3 <= 2`), where the branch closes only by refuting the premise.
  The old shape-based grading had inverted such `le` hypotheses by
  accident.

`MeBi Config Premise Depth <n>` (default 16, reset by `Reset Bounds`);
`Help Premises`/`Config Bounds` and the README updated.

**Verification.** Proof suite identical to the per-mode baselines under
`Auto`, forced `True` and forced `False` (temporary override). Before
relying on the inversion change, I measured that every non-LTS hypothesis
in the existing proofs is a data binder that already graded 0.
`theories/Test.v` gains `GeneralPremises`: size pins for `<=`, `<`, `In`,
`Forall`, `/\`, `\/` (`n <= 2` hand-counted: 4 states), the depth bound
(`Premise Depth 2` cannot settle `le 0 2`), `weak_sim` proofs through `<=`,
`In`, `Forall`, `\/`, and known-wrong cases for an opaque function and a
negation. Each of the nine `Fail`s was checked for its reason. `make`
builds; `tests.exe` 61/61; module lists agree (59). Two `make`-only
warning-50 errors (my new definitions split a doc comment from its
definition) were fixed before the run that counts.

**Guard fix** (separate commit). Yesterday's `Proof_solver.guard`
tested only the outer exception's name. The step logic runs inside
`Proofview.Goal.enter`, and the tactic engine wraps exceptions raised there
in `Logic_monad.TacticFailure`, so a `CannotGetTransition` hit during this
work still came out as an Anomaly. The guard now unwraps; checked by
injection inside the step logic.

**Not in stage 1:** premises that compute outputs (a premise producing what
an LTS premise needs, `In q l -> lts q a q'`), negation, the user tactic
hook. See note 9, stages 2 and 4.

**Session tally (2026-10-02):** New feature 1 · Bug fix 1 · Optimization 0 ·
Tooling 0 · Docs 0 · Refactor 0.

## 2026-10-02 — A duplicate theory entry

**Refactor** (no behaviour change). `Mebi_theories` listed
`clos_trans_1n` twice and `clos_refl_trans_1n` not at all. `Hashtbl.of_seq`
uses `replace`, so the duplicate collapsed to one entry, and nothing looks
either key up. The duplicate is removed. Whether `clos_refl_trans_1n` was
meant, the relation the solver's weak-transition goals use, is left
open: adding it would make those goals count as theory (unfolding, the new
premise-goal check), a behaviour change with no observed benefit. A comment
at the site says so. `dune build` and `make` (plugin and `Test.v`) pass.

**Session tally (2026-10-02):** New feature 1 · Bug fix 1 · Refactor 1 ·
Optimization 0 · Tooling 0 · Docs 0.

## 2026-10-02 — I2 stage 4 and negation: user premise tactic, `~ P`

Branch `main` (on `fork`). **New feature** (negation support and the user
tactic hook, both in note 9; the user said continue after stage 1), plus a
**Bug fix** to the Anomaly guard.

**Negation.** `Premise_search.prove` recognises `P -> False` (so `~ P`
after head reduction). It **holds** iff `P` is refuted by a complete
search, and is false iff `P` is proved. A negation has no
constructor-search proof term, so `proof` gains `ByRefutation`. The solver
proves such a goal with `negation_tac`: `hnf` (since `not` is a constant,
not yet a product, and `intro` failed without it), `intro`, then
`refute_hyp_tac`.

**`refute_hyp_tac`** replaces yesterday's single `simpl; inversion_clear`
step: it refutes recursively in one tactic, clearing every refutable premise
the inversion leaves. A refutable *negation* hypothesis is applied to the
proof of its `P`.

**User tactic hook.** `MeBi Config Premise Tactic <tactic>` (an Ltac
expression; the grammar now opens `Ltac_plugin`/`Tacarg`). For a premise
the search leaves `Unknown`, the tactic is run with
`Subproof.build_by_tactic_opt` on `P`, giving a proof term (holds), and
then on `~ P`, which means false. In proofs: a held premise is `exact`ed
with that term. A false premise hypothesis is closed by `exfalso; exact (np
H)` with the tactic's proof of `~ P`, because inversion cannot refute
something like `f 3 <= 2` with an opaque `f`; my first attempt tried
exactly that and failed. `MeBi Config Reset Premise` clears depth and
tactic; `Reset` clears the tactic too.

**Also needed:** `invertibility` called `to_atomic` first, which raises
on a non-atomic hypothesis such as `H : ~ 3 <= n` (`Rocq_utils_HypIsNot_Atomic`).
Such a hypothesis now goes straight to the premise grading.

**Guard fix** (separate commit). That `Rocq_utils_HypIsNot_Atomic` still
escaped as an Anomaly, because it comes from a `lib/` library, not
`Mebi_plugin.`. So the name test was the wrong criterion. The guard now
treats as internal anything Rocq would print as "Uncaught exception",
i.e. anything with no registered printer.

**Verification.** Proof suite identical to the per-mode baselines under
`Auto`, forced `True` and forced `False`. `Test.v` `GeneralPremises`: the
negation case flips from known-wrong to positive (`~ (3 <= n)`: 4 states,
and a proof), and an opaque `f` with `Axiom f_def` and `Premise Tactic
(rewrite f_def; lia)` gives an exact LTS and a proof. The opaque case
without a tactic stays known-wrong. The new `Fail`s were checked for their
reasons. `make` builds; `tests.exe` 61/61. `.lia.cache` (written by `lia`
during the build) is now gitignored.

**My mistakes this round:**

- The third and fourth `make`-only warning-50 errors this session, both
  from inserting a definition between an existing doc comment and its
  target (`api.ml`, `proof_solver_step.ml`). They cost one matrix run.
  `make` is now run on the plugin before every matrix.
- A wait loop on a mistyped task ID, caught when it never returned.

**Session tally (2026-10-02):** New feature 2 · Bug fix 2 · Refactor 1 ·
Optimization 0 · Tooling 0 · Docs 0.

## 2026-10-02 — I2 stage 2: premises that compute what the transition needs

Branch `main` (on `fork`). **New feature** (note 9, stage 2; the user said
continue). Plus a **Docs** correction to a claim of mine from 2026-10-01.

**Before, measured on five cases.** A constructor whose target is computed
by a premise (`m = S n`, `succ_rel n m`, a two-solution relation, `In q [..]`)
extracted **one state and no transitions** from 0. The premise was still
open when decided, so it was undecided and kept, but the target was never
computed, and an evar target drops the transition. So this was a silent
under-approximation; the only sign was the warning. `In q [n; S n] -> base q
a q' -> sys n a q'`, where `In` fixes the source of the LTS premise `base`,
found 1 of 3 transitions with **no warning**: exploring an LTS from an unknown
source finds only some of its steps. That problem predates this work.

**Extraction.**

- `Premise_search` enumerates (`search ~all`, `enumerate`). Every solution
  is an evar map instantiating the open variables. Open equations are
  solved by unification. Indices may be open patterns, and parameters may
  contain evars: first-order unification against them is complete. There is
  a solution cap of 64. A sub-premise with open variables is always fully
  enumerated, even when only one proof is wanted, because which solution is
  taken can decide whether a later premise holds. Stopping at the first could
  turn a solvable goal into a "refutation".
- `resolve_deferred` takes the deferred premises left to right over a list
  of evar maps: drop on refutation, multiply on solutions, keep and warn when
  undecided. It is used where the LTS premises are unified
  (`sandbox_unify_all`, which now returns a list) and on the axiom path.
  Each final map is a transition.
- An LTS premise whose source is still open first resolves the premises
  that could fix it, then is explored once per solution, each in a sandbox.
  Constructor binders are walked **last to first** (found by testing: the
  generator had to be declared *after* the LTS premise to work), so it also
  looks ahead at the premises declared before it. Changing the walk order
  instead would have reversed the derivation-tree order that A6 depends on.
- New warnings: possibly-incomplete solutions, and an LTS premise explored
  from a source nothing determines (the remaining under-approximation, now
  loud).

**Proof solver.**

1. A premise hypothesis that still mentions variables it determines is
   inverted with `inversion; clear; subst`, not `inversion_clear`. Checked
   by hand: `inversion_clear` reverts the transition hypothesis that
   depends on the premise's variable and reintroduces it with a fresh,
   unconstrained one, so the computed target never reached `get_transition`
   and the solver re-inverted forever.
2. Each step's known target (`this.goto` of the annotation) is bound into
   its first constructor's arguments. `Bindings` used to drop a
   bare-variable target position, since unification usually supplies it, but
   in a proof the target is still open. Target bindings were never used
   before, so keeping them changes only the new path. `Constructor_bindings`
   now de-duplicates by name: the first attempt broke `Test.v`'s
   `TwoPremises` with "q occurs more than once".
3. `constructor` became `econstructor`, so a binder only a premise
   mentions (`q`) is left to unification.
4. After a constructor is applied, its non-LTS premise goals are moved
   behind the LTS ones (`move_premises_last`). My first attempt rotated the
   goal from inside a later step, but each step runs focused on the first
   goal, so `cycle` could never see the sibling. The rotation path was
   removed.

**Verification.** Proof suite identical to the per-mode baselines under
`Auto`, forced `True` and forced `False`. So `econstructor`, the target
bindings and the dedup change no existing proof. satdiff identical,
`tests.exe` 61/61, `make` builds. `theories/Test.v` gains
`OutputPremises`: size pins for the five shapes (states hand-counted),
proofs for the relation, two-solution and `In`-feeds-LTS cases, and a
known-wrong pin for an LTS premise whose source nothing determines. Every
`Fail` was checked for its reason.

**A correction to my own claim.** On 2026-10-01 I wrote, in
`ExtractionSizes`' comment and this log, that "succeeds at n, fails at n−1"
pins the state and transition counts *exactly*. It pins the least bound at
which extraction completes. The bound is checked before each state is
explored, so the LTS can exceed it by the last state's out-degree:
`via_c` completes at 2 transitions with 3. The `p1` hand counts matched only
because its last-explored states are terminal. The comment is corrected.

**Session tally (2026-10-02):** New feature 3 · Bug fix 2 · Refactor 1 ·
Docs 1 · Optimization 0 · Tooling 0.

## 2026-10-02 (second session) — Provenance audit against `rocq-sims`; a solver gap found by its example

**Docs** (audit) · **Tooling** (one known-wrong test). Requested by Jonah: before
evaluating MeBi against related work, check whether anything Claude
contributed was copied from [`rocq-sims`](https://github.com/rocq-sims/rocq-sims)
(N. Chappe, *A Family of Sims with Diverging Interests*, POPL'26), which works
on the same subject, simulations over LTSs in Rocq.

**What `rocq-sims` is.** A pure Rocq library (LGPL-3.0-or-later, ~4.5k lines
of `.v`, no OCaml), first commit 2025-04-07, v0.2 2025-11-21. It defines 12
simulation notions through one parameterised definition, including a
divergence-sensitive weak simulation characterised by two mutually dependent
coinductive relations, with up-to techniques built on Pous's `coinduction`
library. Its proofs are written by hand. It has no LTS extraction, no decision
procedure and no proof search. The subject overlaps with ours; the method
does not.

**Audit scope and method.** All 115 commits carrying the
`Co-Authored-By: Claude` trailer (2026-08-16 to `9dab1b6`): 621 added lines
of `.v` and 10,164 of OCaml (`.ml`/`.mli`/`.mlg`).
- *Text.* Every added `.v` line of 25 characters or more, normalised for
  whitespace, was matched against every line of `rocq-sims`: **0 matches**.
  The same additions as 8-token shingles: **0 of 5,455 shared**. Over the
  whole repository's `.v` (37 files, assisted or not), 10 shingles are
  shared, all generic tactic or match idioms (`inversion H; subst; clear H`,
  `| _, _ => False end`) and none from assisted lines. The OCaml has nothing
  to compare against: `rocq-sims` contains none.
- *Access.* The session transcripts from 2026-09-27 onward (10 sessions and 6
  subagents) contain no web search, web fetch or clone of any outside
  repository before this session. **Gap:** the 13 assisted commits of
  2026-08-16 to 2026-08-18 (the Rocq 9.2 port, toolchain pinning, logger
  refactor, encoding table, the first `tests.exe`) come from sessions whose
  transcripts are not on disk, so for them there is only the text check.
- *Ideas.* The techniques Claude introduced are standard and unrelated to
  `rocq-sims`' contribution: weak-transition saturation by silent closure and
  silent-SCC counting (textbook; Tarjan); one mutual `cofix` over the
  reachable product (a Rocq proof-term construction, not a mutually
  coinductive *definition* as in `rocq-sims`); bounded proof search to decide
  constructor premises. That last one is close in spirit to QuickChick's
  derivation of semi-decision procedures from inductive relations
  (Paraskevopoulou, Eline, Lampropoulos, PLDI'22). It was not consulted, but
  should be cited as related work.
- *Limit of the audit.* A model trained on public code could reproduce an
  idea without a session ever fetching it. The text check above is the
  mitigation for verbatim reuse; it cannot rule out an idea absorbed in
  training.

**Verdict:** no copying from `rocq-sims` found; nothing to attribute. The
one place `rocq-sims` content now enters this repository is deliberate and
attributed: the LTS of its `examples/SimExample.v`, re-encoded for the
finding below.

**Finding (open, not fixed): the solver cannot answer a silent step by
moving silently.** Porting `SimExample.v`'s 9-state LTS, `weak_sim t0 u0`
stops on an internal `CouldNotGetGoalTransition`. Reduced to 5 states:
`q` is `p` renamed, both `τ.a + b`, and `weak_sim p q` fails. To answer
`p -τ-> p1`, `q` must move silently to `q1`, since staying at `q` is not
bisimilar to `p1`. But `Saturation.edge_closure` records only weak moves
with a visible action, so `Product.respond` finds no silent reply.
`handle_wk_concl` covers only the case where staying put works, which
is why every `Proc`/`CADP` proof passes: their silent steps are structural
congruence and never change the bisimilarity class. ~~The bisimilarity
*checker* is unaffected (it correctly rejects Milner's `τ.a + b` vs
`a + b`).~~ **Wrong, corrected the same day:** the checker *is* affected. It
rejected Milner's pair only because `p1` had no partner in the other FSM;
the partition itself put `τ.a + b` and `a + b` in one block. Partition
refinement splits on visible labels of the visible-only saturated FSM,
leaving out the `=ε=>` (τ*) relation that the standard reduction of weak to
strong bisimilarity needs. With one extra `c`-branch, so that every state
has a partner (`p = τ.p1 + b + c.p1`, `r = a + b + c.r1`,
`p1 = r1 = a`), `MeBi Run Bisim` reports them bisimilar, which
classical weak bisimilarity rejects. A related question is open and for the
authors: `theories/Bisimilarity.v`'s `weak_bisim` is *mutual similarity*
(two `weak_sim`s), under which that pair, and Milner's, *are* related. So
the checker currently matches neither notion. No proof can be wrong because
of any of this, since `Qed` checks it; the solver's gap makes proofs
missing, not wrong. Pinned as known-wrong `theories/Test.v`
`SilentResponse`. The `Fail` was checked for its reason.

Also measured: `weak_sim` is divergence-insensitive, so a τ-loop and a stuck
state prove similar both ways (5 and 2 iterations). That is correct for our
definition, and is the main semantic difference to state in any comparison
with `rocq-sims`.

## 2026-10-02 (second session) — Weak bisimilarity done properly: rooted verdict, `=ε⇒` split, silent answers

**Bug fix** ×3 · **Tooling** (tests). On branch `fix/weak-bisim-silent-closure`
(pushed to `fork`, not `main`), by Jonah's decision that changes to a core
mechanism's correctness get their own branch. Jonah also chose the notion
the plugin is meant to mechanise: **classical weak bisimilarity**
(Milner's observation equivalence), as the README's Sangiorgi citation
says. This entry does the part that needs no change to `theories/`
("option C"). Making the Rocq statement itself a bisimulation ("option A")
is to be agreed with Jonah before starting; see "Still open" below.

### Background: why weak bisimilarity needs `=ε⇒`, and why it is reflexive

An LTS has steps `p –α→ p'` where `α` is visible (`a`, `b`, ...) or silent
(`τ`). Weak transitions hide silent steps:

- `p =ε⇒ p'`: `p –τ→ ··· –τ→ p'` in **zero** or more steps. Zero steps is
  allowed, so `p =ε⇒ p` for every `p`, whether or not `p` has a `τ` step.
- `p =a⇒ p'`: `p =ε⇒ –a→ =ε⇒ p'`, for each visible `a`.

A relation `R` is a **weak bisimulation** if, whenever `p R q`: every
`p –a→ p'` is matched by some `q =a⇒ q'` with `p' R q'`; every `p –τ→ p'`
is matched by some `q =ε⇒ q'` with `p' R q'`; and symmetrically for `q`.
Weak bisimilarity `≈` is the largest such relation. A silent step is matched
by `=ε⇒`, i.e. by zero or more silent steps, not by exactly one: that is
what makes `τ` invisible. Sources (see "On the citations" below for what was
checked): Milner, *Communication and Concurrency*, Prentice Hall 1989, the
chapter on bisimulation and observation equivalence; Sangiorgi,
*Introduction to Bisimulation and Coinduction*, CUP 2011, ch. 4 ("Weak
equivalences"). Milner writes the matching move as `q =α̂⇒ q'`, where `α̂`
erases `τ`.

**The standard reduction.** Because a weak move is answered by a weak move,
`≈` on an LTS `L` coincides with *strong* bisimilarity `~` on the
**saturated** LTS `L̂`. `L̂` has the same states and two kinds of
transition: `p =a⇒ p'` for each visible `a`, and `p =ε⇒ p'` as one more
label. This is how finite-state tools decide `≈`: compute `L̂` (a transitive
closure), then run a strong-bisimilarity algorithm on it (Kanellakis and
Smolka, "CCS expressions, finite state processes, and three problems of
equivalence", *Information and Computation* 86(1):43–68, 1990, for
partition refinement on CCS).

**What MeBi had.** `Saturation` built the `=a⇒` half of `L̂` only, and
`Minimization.partition_states` split blocks by visible labels only. With
`=ε⇒` missing, the partition is in general **coarser** than `≈`. Milner's
standard example shows it: `τ.a + b` and `a + b` have identical `=a⇒`/`=b⇒`
moves, but are not weakly bisimilar. After `τ.a + b –τ→ a`, the other side
must answer by `=ε⇒`; `a + b` can only stay put, and `a ≉ a + b`.

**Why `=ε⇒` must be reflexive.** Leaving out the zero-step case breaks the
most basic law of weak bisimilarity, `τ.P ≈ P` (the first of the `τ`-laws in
both sources). Take `a` and `τ.a`. With a reflexive `=ε⇒`, both reach the
block of `a` silently (`a` by zero steps, `τ.a` by one), so they are not
split. Without the zero-step case, `τ.a` reaches that block and `a` reaches
nothing, so a correct partition algorithm would *separate* two bisimilar
processes. `tests.exe` checks exactly this pair, and was confirmed to fail
when the closure is made non-reflexive (two failures).

**What reflexivity does not mean.** It does not mean adding a `τ`
self-loop to any state of the LTS, and the fix does not. That would be wrong
in three ways:
1. It changes the system: every state would get a real `τ` step, i.e.
   every state would *diverge*. `≈` ignores divergence, but a
   divergence-sensitive relation (e.g. `rocq-sims`' μdiv-simulation) would
   then relate nothing usefully. It is also wrong for strong bisimilarity,
   where `τ` is an ordinary label that must be matched step for step.
2. It conflates `=ε⇒` (zero or more) with `–τ→` (exactly one). An answer
   built from it would have to exhibit a silent step the system cannot take.
3. In a proof it cannot be used: the Rocq LTS has no such constructor, so
   no `weak` derivation can step along it.

`=ε⇒`'s reflexivity is a property of the *weak* relation, `silent` =
`clos_refl_trans_1n` in `theories/Bisimilarity.v`. The plugin's Rocq side
already has it: `silent` is `clos_refl_trans_1n` of `tau`, and the
zero-step answer is `wk_none` + `rt1n_refl`. The fix keeps it there. `=ε⇒` is
never stored as an edge of any FSM. It is computed on the side from the
**unsaturated** LTS's silent steps, and used only (a) as a splitting
criterion in partition refinement and (b) as the set of answers the solver
may give to a silent move. Saturation's output is unchanged (`satdiff -- 200`
byte-identical).

### What was wrong, and the fixes

1. **The verdict was not rooted** (`6976ea0`). `Result.are_bisimilar` asked
   whether every block of the merged partition held states of both systems,
   never whether the two *initial* states shared a block. `a.b.x` against
   `b.a.y` is not even strongly bisimilar, yet both blocks ({x, b.y},
   {b.x, y}) are shared, and `MeBi Run Bisim` said bisimilar. The verdict
   now compares the roots, and falls back to the old test only for an FSM
   without an initial state. **This was Claude's error twice over:** the
   `tests.ml` comment written with the first test binary (`c078308`,
   2026-08-17) noticed that the verdict ignores `init` and argued it was
   sound "because every state is reachable from the root". That argument is
   false, as this pair shows. The comment is replaced.
2. **No `=ε⇒` split** (`f162126`). `partition_states ?silent` now also
   splits each block by the set of blocks its states reach by `=ε⇒`.
   Closures are computed from the unsaturated silent edges and memoised,
   not stored. `Bisimilarity.fsm` passes the merged *originals*' edges, and
   `Minimization.fsm` its own. The interface doc said a saturated FSM's
   partition "is weak bisimilarity"; that was Claude's (`2144f5c`,
   2026-10-01) and is corrected.
3. **The solver could not answer a silent move by moving** (`2e6eb0d`).
   It stayed put when that was bisimilar, and otherwise asked the
   saturated FSM, which has no silent moves. `Product.respond ?silent` now
   answers a silent label with the nearest acceptable state reachable by
   one or more silent steps (`Saturation.silent_paths`, the existing BFS,
   now exported), annotated with that path. The solver's existing
   `wk_none` + `rt1n_trans` walk applies it unchanged.
   `successors`/`reachable`/`estimate` pass it on, so the mutual-cofix
   product agrees with the solver. Also fixed: `respond` let `Not_found`
   escape for a state with no saturated moves at all.

### Tests and verification

- `Test.v`:
  - `CheckerVerdicts`: both pairs are now `Fail`s. Each was pinned
    known-wrong first (`11bd121`) and flipped by its fix. Positive checks
    `p1 ≈ r1` and `a.z ≈ τ.a.z` sit beside them.
  - `SilentResponse`: now proves. It also has `rocq-sims`' `SimExample`
    LTS (attributed, both directions: 30 and 26 iterations) and a
    divergence pair (`loop ≈ stop`, by design).
- `tests.exe` **67/67** (was 61): rooted verdict, `=ε⇒` split (Milner's
  pair; `a ≈ τ.a`, mutation-checked as above), silent `respond` (path
  length 2; no answer without `?silent`).
- `satdiff -- 200` byte-identical. `make` on the plugin and `Test.v` clean.
- Proof suite, all 27 `Solve`s, **identical to the baseline in all three
  modes**:
  - `Auto` and forced `MutualCofix True`: the `CLAUDE.md` figures.
  - Forced `False`: Test2 `446 278 299 194 446 182`, and Test3 stops at
    1127, as before.

  As predicted: every silent step in `Proc`/`CADP` stays inside its
  bisimilarity class, so neither the extra split nor the new answers ever
  fire there. Which is also why none of this was caught: the corpus never
  exercised it.

### On the citations

Checked this session: the bibliographic details of Kanellakis and Smolka
(1990), and that chapter 4 of Sangiorgi (2011) is "Weak equivalences",
covering weak bisimilarity and the `τ`-laws (publisher's summary). Not
checked against the text: the chapter of Milner (1989) is cited by title,
not number (sources disagreed). Whether Kanellakis and Smolka state the
saturation reduction in exactly this form is also unchecked: their paper is
the standard citation for partition-refinement bisimilarity checking on
finite CCS processes, and the reduction is textbook, but a reader wanting a
precise pointer should check §-level references before citing this log.
The definitions above are standard and match `theories/Bisimilarity.v`'s
own `weak`/`silent`.

### Still open (for "option A", to agree with Jonah first)

- `theories/Bisimilarity.v`'s `weak_bisim` is **two separate `weak_sim`s**
  (mutual similarity), strictly coarser than `≈`. Every `PluginProofs.v`
  therefore proves mutual similarity, while the checker now decides `≈`.
  This is sound, since `≈` implies mutual similarity. Closing the gap means
  a single coinductive bisimulation in `theories/` and a solver that proves
  both directions in one proof (backlog G).
- `MeBi Sim Begin` refuses a valid `weak_sim` goal between states that are
  similar but not bisimilar. Checked: `a.b ≤ a.(b+c)` stops with
  `Not_Bisimilar` before any step, and the solver also steers by the
  bisimilarity partition. Fine for `≈`, but `weak_sim` alone is promised
  more than it delivers.

**Session tally (2026-10-02, second session):** Bug fix 3 · Tooling 2 ·
Docs 1 · New feature 0 · Refactor 0 · Optimization 0.

## 2026-10-02 (second session) — `theories/`: `weak_bisimilar`, a single weak bisimulation (option A, part 1)

**New feature** (theory only). On branch `theories/weak-bisimilar`, kept
apart from plugin-code branches so that the change to `theories/` has its
own history. Jonah's decision: the project's aim is to mechanise
bisimilarities, and the intended notion is classical weak bisimilarity. To
be reviewed with @dcastrop later; built to be easy to revert (below).

**Why.** `theories/Bisimilarity.v`'s `weak_bisim s t` is
`weak_sim s t /\ weak_sim t s`: two simulations, each free to pick its own
relation. That is mutual similarity, strictly coarser than weak
bisimilarity, which asks for *one* relation that is a simulation both ways.
So every `PluginProofs.v` "bisimilarity" proof has so far established
mutual similarity. The textbook separating pair is `a.b + a` and `a.b`.
Each simulates the other, but after the `a` step into the stuck branch,
`a.b` can only reach a state that still offers `b`. Mutual similarity does
not preserve deadlock; bisimilarity does.

**What.** Strictly additive. Nothing existing is edited, and the plugin
does not refer to anything new.
- `theories/Bisimilarity.v`, new section after `wk_bisim_sym`:
  - `bisimF G m1 n1`: a record with `bisim_l` (as `simF`'s `sim_weak`) and
    `bisim_r` (its mirror image);
  - `CoInductive weak_bisimilar := In_bisim { out_bisim : bisimF … }`,
    built the same way as `weak_sim`;
  - lemmas: `weak_bisimilar_sim`, `weak_bisimilar_sym`,
    `weak_bisimilar_weak_bisim` (so `weak_bisimilar` implies the old
    statement), `weak_bisimilar_refl`, `weak_bisimilar_silent_clos`,
    `weak_bisimilar_act_clos`, `weak_bisimilar_trans` (heterogeneous, like
    `weak_sim_trans`).
- `theories/Test.v`, new module `WeakBisimilarVsMutualSim` at the end: a
  hand proof that the pair above is `weak_bisim` and
  `~ weak_bisimilar`. This backs the claim "strictly coarser" with a
  checked proof, rather than a comment.

No new name clashes with `examples/`, `src/` or `lib/` (checked by grep;
`loader.v` re-exports the theory). `dune build` and `make` on the plugin and
`Test.v` are clean. The plugin's code is untouched, so no proof-suite run is
needed.

**Next (option A, part 2, a separate branch):** have the solver prove
`weak_bisimilar` goals in one proof: both obligations under one cofix,
with FSM a and FSM b swapping roles for `bisim_r`. That needs the
`fix/weak-bisim-silent-closure` branch merged first, since it builds on
`Product.respond ?silent`.

### How to revert this, if @dcastrop disagrees

The change is the commits on `theories/weak-bisimilar` from `f4d268e`
onwards (this log entry included), touching only `theories/Bisimilarity.v`,
`theories/Test.v` and this file.

- **Before it is merged:** do not merge it. Delete it with
  `git push fork --delete theories/weak-bisimilar` and
  `git branch -D theories/weak-bisimilar`.
- **After it is merged into `main` with a merge commit** (the intended
  route; find the commit with
  `git log --merges --oneline --grep weak-bisimilar main`):
  ```
  git switch main
  git revert -m 1 <merge-commit>
  dune build && dune exec test/tests.exe
  git push fork main
  ```
  `-m 1` keeps `main`'s side and undoes everything the branch brought in,
  as one new commit; history is not rewritten.
- **If plugin code has since started using `weak_bisimilar`** (option A,
  part 2): revert that branch's merge commit first, the same way, then this
  one. Reverting this one alone would leave the plugin referring to
  missing names, and `dune build` would fail on the first reference.
- **To keep the theory but drop only the proof of strictness:** delete
  module `WeakBisimilarVsMutualSim` from the end of `theories/Test.v`. It is
  self-contained.

**Session tally (2026-10-02, second session), cont.:** New feature 1
(theory only).

## 2026-10-02 (second session) — The solver proves `weak_bisimilar` (option A, part 2)

**New feature.** On branch `solver/weak-bisimilar`, after `theories/`'s
`weak_bisimilar` (part 1, merged as PR #2). Agreed with Jonah in advance
(option A, "additively"). `MeBi Sim` now proves `weak_bisimilar` goals:
weak bisimilarity, both directions in one proof. Before, a "bisimilarity"
proof meant two `weak_sim` examples, which establish only mutual
similarity.

**Design: one solver, read both ways.** `In_bisim; Pack_bisim; intros`
leaves two goals per pair:
- `bisim_l`: `H : ltsM m1 a m2 ⊢ ∃ n2, weak ltsN n1 n2 a ∧ weak_bisimilar m2 n2`
- `bisim_r`: `H : ltsN n1 a n2 ⊢ ∃ m2, weak ltsM m1 m2 a ∧ weak_bisimilar m2 n2`

`bisim_r` is `bisim_l` with the systems exchanged. So instead of a mirrored
copy of the solver, there is a flag: `Results.swapped`, which
`get_fsm_a`/`get_fsm_b` follow. `Concl.orientation` sets it at the top of
every step from the goal's shape: in an `∃`-goal, the witness is the
relation's *left* argument exactly when the *right* system moved. Goals
that do not say (an LTS step or premise, part-way through an answer) keep
the last setting. The one place the solver read a fixed argument position,
`get_a'_from_wk_sim`, now reads 6 rather than 5 when swapped. Everything
else (hypothesis lookup, `Product.respond`, constructor application) works
unchanged through the swapped FSMs. The rest is goal-kind dispatch:
- `is_weak_goal` accepts either goal kind;
- `In_bisim`/`Pack_bisim` and `weak_bisimilar_refl` are used for
  `weak_bisimilar` goals;
- the mutual block and `Auto`'s estimate use `Product.successors_bisim`,
  which gives each pair both sides' obligations, built from `successors`
  applied once each way;
- the new theory names are registered in `Mebi_theories`.

**Results.**
- `weak_sim` proofs are unchanged: all 27 counts identical under `Auto`,
  forced `MutualCofix True`, and forced `False` (Test3 stops at 1127, as
  before).
- New `examples/Bisimilarity/**/BisimProofs.v`: one `weak_bisimilar` proof
  per pair, beside each of the six passing `PluginProofs.v`. All 14 prove
  under `Auto` and forced `True`, with identical counts:
  ```
  Proc/Test1   709, 709, 51
  Proc/Test2   702, 852, 852
  Proc/Test3   14427, 6787, 10243, 4467, 6755
  CADP/Size1   2875, 2875, 185
  ```
- Forced `False` (nested) leaves all but `Glued/MutualExclusion` (3859)
  unfinished at 20000 steps. `Auto` picks the mutual cofix for every
  `weak_bisimilar` proof, because its estimate already shows the nested walk
  exploding ("over 692 goals" for Test1's 45 pairs).
- `Test.v` `WeakBisimilarProofs` (built in CI): `τ.a + b` against a renamed
  copy (silent answers on both sides), reflexivity, `rocq-sims`' pair, and
  `MeBi Sim Begin` refusing `a.b + a` vs `a.b` (`Not_Bisimilar`, checked
  to be the reason).
- `tests.exe` 70/70 (a bisimulation-game test).
- `satdiff` identical; `make` and the module-list check clean.

**Cost, and where it comes from.** A `weak_bisimilar` proof costs far more
than the two `weak_sim` proofs together (Test1 `pq`: 709 against
114 + 105; Test3 `p3`: 14427 against 1073). Per pair it is not worse:
about 47 iterations against 27.5, for twice the obligations. The relation
is larger: Test3 `p3` has 306 pairs against 39. Each answer is chosen
independently (the least state in the target class), so `bisim_r`'s
answers make pairs that `bisim_l` never visits, and the relation must be
closed under both. **A likely optimization, not done:** prefer answers that
land on a pair already in the relation. That changes the solver's choices
and its counts, so it should be raised before it is built.

**Not changed:** `weak_sim` keeps its meaning and its proofs, and
`weak_bisim` is untouched. Whether the `PluginProofs.v` files should move to
`weak_bisimilar`, and the cost question above, are for later.

### How to revert this, if @dcastrop disagrees

The change is the commits on `solver/weak-bisimilar` from `a4a7fda`
(this log entry included). It touches `lib/model/algorithms/product.*`,
`lib/rocq_tools/{mebi_theories,theories}.*`, `src/{proof_solver,
proof_solver_step,proof_solver_tactics,results,help}.*`, `test/tests.ml`,
`theories/Test.v`, `README.md`, `CLAUDE.md`, `_CoqProject`, the six
`BisimProofs.v` and this file.
- **Before it is merged:** delete the branch
  (`git push fork --delete solver/weak-bisimilar`,
  `git branch -D solver/weak-bisimilar`).
- **After merging with a merge commit:**
  ```
  git switch main
  git log --merges --oneline --grep solver/weak-bisimilar main   # find it
  git revert -m 1 <merge-commit>
  dune build && dune exec test/tests.exe     # expect 67/67
  git push fork main
  ```
  Revert this **before** part 1 (`theories/weak-bisimilar`) if both are to
  go. `Mebi_theories` loads every registered name at once, so reverting the
  theory alone would leave the plugin failing on the first `MeBi Sim` of
  any kind.

**Session tally (2026-10-02, second session), cont.:** New feature 2
(1 theory, 1 solver).

## 2026-10-02 (second session) — `theories/`: `mutual_sim`, the honest name for `weak_bisim`

**New feature** (theory only, additive). On branch `theories/mutual-sim`. Jonah's
decision: every statement should say what it proves. `weak_bisim` is two
separate `weak_sim`s (mutual similarity), and its name suggests the
stronger `weak_bisimilar`. Added `mutual_sim`, with the same body, and:
- `mutual_sim_weak_bisim` (the two are equivalent, `<->`);
- `mutual_sim_refl`, `mutual_sim_sym`, `mutual_sim_trans`;
- `weak_bisimilar_mutual_sim`.

`weak_bisim` and its lemmas are untouched, so existing proofs (`ManualProofs.v`,
`LtacProofs.v`) still build. `Test.v`'s strictness module also states the
separating pair as `mutual_sim`. No name clashes (grep).

**How to revert:** before merging, delete the branch. After merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep theories/mutual-sim main`). Revert any
later branch that uses `mutual_sim` first: the `PluginProofs.v` split
(`examples/honest-plugin-proofs`).

## 2026-10-02 (second session) — `MeBi Sim Begin` accepts `weak_sim` goals between similar, non-bisimilar states

**New feature** (flagged to Jonah before writing: it needed the weak
simulation preorder, backlog G's similarity algorithm) and **Bug fix**. On
branch `solver/weak-sim-preorder`. Jonah: "it doesn't sound correct for
`Sim Begin` to refuse similar states that aren't bisimilar."

**What was wrong.** `MeBi Sim Begin` runs the bisimilarity check first,
and with `FailIf NotBisimilar` (the default) it stops on `Not_Bisimilar`.
A `weak_sim` goal asks only for *similarity*, which is coarser.
`a.b ≤ a.(b+c)` holds, and was refused. Removing the refusal alone would
not have helped: the solver picks every answer from the *bisimilarity*
class of the move's target, and for that goal the class has no answer.

**The fix.**
- `Model.Product.simulation a b b_saturated`: the greatest weak simulation
  as `weak_sim` defines it (each strong move of `a` answered by `=l⇒` in
  `b`, `=ε⇒` with zero steps for a silent one). Naive refinement from all
  pairs.
- `Proof_solver.init`, for a `weak_sim` goal: run the check without
  `FailIf`'s error. If the roots are not bisimilar, compute the preorder;
  refuse with a clear message only if the roots are not even similar (still
  under `FailIf NotBisimilar`), else store each state's simulators in
  `Results.simulators` and say so at Notice. `weak_bisimilar` goals keep
  the bisimilarity check.
- The solver's answer (`handle_visible_transition`) and
  `Product.successors` (hence the mutual block and `Auto`'s estimate) try a
  bisimilar answer first, exactly as before, and only on failure fall back
  to a simulator: stay put if a silent move and the current state
  simulates the target, else respond into the simulators. Same order in
  both places.
- Found on the way: a silent move answered by a system that never moves
  silently (`τ.a + b ≤ a + b`) crashed resolving the label in that
  system's alphabet. It now falls back to the move's own label, which is
  the same term.
- The negative-verdict log was labelled "LTS Incomplete"; it now says
  "Not Bisimilar".

**Verification.**
- Proof suite: all 27 `weak_sim` counts identical under `Auto`, forced
  `True` and forced `False` (Test3 stops at 1127, as before). All 14
  `BisimProofs.v` counts identical under `Auto` and forced `True`. Existing
  proofs are between bisimilar states, so the fallback never fires there.
- Scratch cases in all three modes: `a.b ≤ a.(b+c)` (15 iterations), both
  directions of `τ.a + b`/`a + b` (18, 16), and `a.(b+c) ≤ a.b` refused.
- `Test.v` `SimilarNotBisimilar` (built in CI): the same cases, plus
  `mutual_sim` proved from the two directions and `weak_bisimilar` refused.
- `tests.exe` 74/74 (`simulation`).
- `make` clean. It was not the first time: I started the matrix before
  running `make`, and warning 50 (a `val` inserted between a doc comment
  and its declaration) failed every file. That is the gotcha `CLAUDE.md`
  warns about, and it cost a matrix run.

**How to revert:** delete the branch before merging. After merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep solver/weak-sim-preorder main`). It is
independent of the other branches; `Test.v`'s `SimilarNotBisimilar` uses
`mutual_sim` from `theories/mutual-sim`, which must stay if this does.

## 2026-10-02 (second session) — `PluginProofs.v` say what they prove

**Tooling** (examples only). On branch `examples/honest-plugin-proofs`.
Jonah's decision: every use of the `MeBi Sim` commands should be honest
about the relation it establishes, and the change stays additive. In each
of the six passing `PluginProofs.v`:
- the two `weak_sim` directions per pair are unchanged (names, bounds,
  order);
- after each pair, a new `msim_*`: `mutual_sim`, proved from those two by
  `split` (no `MeBi` step). This is what the files used to call a
  bisimilarity proof;
- a new final section with one `wbis_*` `weak_bisimilar` proof per pair,
  moved in from the `BisimProofs.v` files (deleted, with their
  `_CoqProject` lines).

The `weak_bisimilar` proofs come **last** on purpose: under a forced nested
strategy they do not finish, and a run with the strategy forced should
still reach all 27 `weak_sim` counts. `Proc/Test3`'s `p3` has only one
`weak_sim` direction, so it has a `weak_bisimilar` proof but no
`mutual_sim`. `CLAUDE.md`'s baseline is updated: 27 `weak_sim` counts, then
14 `weak_bisimilar`.

**How to revert:** delete the branch before merging, or after merging with
a merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep examples/honest-plugin-proofs main`).
That restores the `BisimProofs.v` files. Revert this before
`theories/mutual-sim` if both are to go.

## 2026-10-02 (second session) — One place to choose an answer (`Product.answer`)

**Refactor.** On branch `refactor/answer-table`. Step 1 of the
answer-selection plan agreed with Jonah: make the choice of answer a single
function before any policy for choosing answers is tried.

**Before.** How FSM "b" answers a move was written out twice:
- in `Product.successors`, which builds the mutual block's product: a stay
  check, then `respond` into the bisimilarity class, then the simulators
  fallback;
- in the solver: `handle_wk_concl`'s stay check, then
  `handle_visible_transition`/`try_get_visible_transition`, which
  re-resolved `b` and the label from the goal's terms, then the same
  fallback.

The two had to agree by hand. A disagreement is a pair outside the mutual
block, and PR #5 had to add its fallback to both.

**After.** `Product.answer ?silent ?sim b pi y label x'` returns
`Stay | Move transition`. `successors` and the solver's `handle_wk_concl`
both call it, on the same inputs: `b`, the move's own label and its target.
`try_get_visible_transition`, `handle_visible_transition` and
`CouldNotFindGotoState` are gone. So is the label-lookup fallback PR #5 added:
the label is now the move's own, which is what `successors` always used.
Any future policy for choosing answers changes this one function.

**Verification.** Every count is identical: all 41 under `Auto` and forced
`True`; under forced `False`, the documented `weak_sim` counts, then a stop
at the first `weak_bisimilar`. `tests.exe` 74/74, `make` clean, `Test.v`
(silent answers, similarity, `weak_bisimilar`) builds.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep refactor/answer-table main`).

## 2026-10-02 (second session) — Measuring answer policies on the model (step 2)

**Tooling** (measurement only; the solver does not read it). On branch
`tooling/answer-policies`. Step 2 of the answer-selection plan agreed with
Jonah: before the solver changes, measure what other ways of choosing
answers would cost.

**What was added.** `Model.Product.Policy`, pure OCaml with a
consistency test in `tests.exe` (79/79). It describes a game (`sim_game`,
`bisim_game`, matching how `successors`/`successors_bisim` play it) as each
pair's obligations: the answer `Product.answer` picks, and every valid
answer, each with its witness length. Three policies choose answers:
- `Default`: today's choices. Checked to reproduce the product exactly.
- `Greedy`: breadth first, prefer an answer whose pair was already
  reached, else the default.
- `Minimal`: start from every pair any answer reaches (always a valid
  relation), delete pairs while every remaining pair can still answer
  everything within what remains, then answer each move with its cheapest
  remaining candidate. This is minimal by inclusion, not the smallest.

`measure` reports pairs, moves (answers needed), total witness length, and
moves left unanswered (0 in every case below).

**How it was measured.** A temporary block in `Proof_solver.init`
(not committed) printed each policy's measure at every `Sim Begin`, across
all 41 proofs under `Auto`. Raw output is kept locally in `notes/`. To turn
the measures into iterations, I fitted a linear model to the `Default` rows,
where the real counts are known. On the 23 proofs that use the mutual cofix
(Test3, and every `weak_bisimilar` proof):

  iterations ≈ 3.0·pairs + 6.0·moves + 3.3·witness   (R² = 0.998)

Over all 41 the fit is about the same (2.3/6.0/3.5, R² = 0.998). Nested
proofs also pay for re-exploration, which the model does not see. The fit
is dominated by the large Test3 proofs: it over-predicts small ones (Test1's
709-iteration `weak_bisimilar` is predicted at 1055).

**Results** (totals over all proofs; pairs / moves / witness):

| policy | `weak_sim` (27) | `weak_bisimilar` (14) |
|---|---|---|
| default | 415 / 825 / 553 | 1388 / 6628 / 3096 |
| greedy | 368 / 702 / 484 | 880 / 3839 / 2675 |
| minimal | 401 / 860 / 652 | **374 / 1667 / 2957** |

Predicted iterations over the 23 mutual-cofix proofs (actual 57,066):
default 58,710, greedy 38,176 (−35%), **minimal 26,218 (−55%)**. Per proof:
- Test3 `wbis_p3`: actual 14,427; greedy ~4,270, minimal ~3,184.
- CADP `weak_bisimilar`: 2,875; minimal ~1,503.

**What this says, and what it does not.**
1. **For `weak_bisimilar`, smarter answers pay off.** `Minimal` cuts pairs
   by 73% and moves by 75%. The 8× blow-up seen in PR #3 is mostly a choice
   of answers, not a property of bisimilarity.
2. **For `weak_sim`, `Minimal` is often worse.** Test3's 211-iteration proof
   is predicted at ~464. Minimal-by-inclusion depends on deletion order, and
   it ignores witness length. `Greedy` was never worse than `Default` on any
   of the 41 by these measures, and helps where pairs repeat (Test3
   `wsim_p3` 1073 → ~432).
3. **No policy wins everywhere.** The measures are cheap to compute before a
   proof starts, as `Auto` already does for the cofix strategy, so a
   per-proof choice of the cheapest predicted policy is possible. So is a
   `Minimal` that deletes in order of cost rather than of pair.
4. **These are predictions from a fitted model, not runs.** Step 3 must
   measure real iteration counts before any claim. The order of goals, and
   how the nested strategy re-explores, are not in the model.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep tooling/answer-policies main`).

## 2026-10-02 (second session) — Answer policies in the solver (step 3)

**New feature** (agreed with Jonah: an opt-in setting, defaulting to today's
behaviour, measured before anything becomes a default). On branch
`solver/answer-policies`.

`MeBi Config Solver Answers Default | Greedy | Minimal | Auto`:
- **`Default`** (the default) is the old path, untouched. The solver
  answers move by move with `Product.answer`, and no plan is built.
- **Any other policy** is planned at `Sim Begin` by `Product.Policy.plan`:
  the chosen answer, with its transition, for every
  `(swapped, mover, move, answerer)` key the game can reach, and the pairs
  that makes. The solver's `handle_wk_concl` looks its answer up in the plan
  (a move outside it is answered move by move, with a notice). The mutual
  block takes its pairs from the plan, and `Auto`'s cofix-strategy estimate
  walks the plan. So the three cannot disagree.
- **`Auto`** plans all three and keeps the lowest `predicted` cost (the
  fitted `3·pairs + 6·moves + 3.3·witness`). Ties go to `Default`.
- `Begin` announces the plan and its prediction at Notice.

**Measured: real iteration counts, all 41 proofs,** each policy run with
loose bounds so that none was cut off. Cofix strategy `Auto`, as checked in
(Test3 forces mutual itself):

| | `weak_sim` (27) | `weak_bisimilar` (14) |
|---|---|---|
| Default | 7,142 | 52,489 |
| Greedy | 6,131 (−14%) | 33,217 (−37%) |
| Minimal | 7,603 (+6%) | 21,117 (−60%) |
| **Auto** | **5,894 (−17%)** | **21,023 (−60%)** |

Every proof completed and passed `Qed` under every policy. The `Default`
column reproduces the 41-count baseline exactly. **`Auto` is never slower
than `Default` on any of the 41, and on every one it achieves the best of
the three.** Selected proofs:
- Test3 `wbis_p3`: 14,427 → 3,579.
- CADP `weak_bisimilar`: 2,875 → 1,674.
- Test3 `wsim_p3`: 1,073 → 438.
- Test1 `wsim_pq`: 114 → 88.

`Minimal` alone is worse than `Default` on many `weak_sim` proofs (Test3
`wsim_pr` 211 → 438), which is why it is not offered as the policy to use
unconditionally.

**Also:**
- `Product.Pair.Map`.
- `Stdlib.Option` qualified in `product.ml`: in the packed `make` build a
  plugin module named `Option` shadows it. `dune` accepted the code; `make`
  caught it.
- `Test.v` `AnswerPolicies` proves goals under each policy (CI).
- `MeBi Help Config Solver` and the README document both solver settings
  (the README had been missing `MutualCofix`).

**Not decided:** making `Auto` the default. It was measured only under the
`Auto` cofix strategy. Under forced `MutualCofix True`/`False` the plans
interact with goal order, which is unmeasured. A default change also
changes every checked-in count, so the bounds would need re-measuring.
Jonah's call, and possibly @dcastrop's.

**Verification under the default policy:** the standard matrix with the
checked-in bounds, identical in all three modes: all 41 counts under `Auto`
and forced `True`; forced `False` gives the documented `weak_sim` counts,
then stops at the first `weak_bisimilar`. `tests.exe` 79/79, `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep solver/answer-policies main`). Nothing
else depends on it.

## 2026-10-02 (second session) — `MeBi Help` on answer policies; @dcastrop items grouped

**Docs.** Straight onto `main` (documentation only).
- `MeBi Help Config Solver` now explains what an answer is, and what
  `Default` does. It is both the name of a policy and the default setting:
  each answer is chosen alone, with no look-ahead (stand still if silent and
  bisimilar, else the shortest witness into the target's class, to its
  lowest-numbered state; for merely similar states the same against the
  simulators), which is why proofs can visit more pairs than they need. The
  text also says what the other policies do, what was measured, and that a
  policy can only change how long a proof is, never whether it is correct.
- `TODO.md` has a new "To discuss with @dcastrop" section. It gathers the
  `paper/`, LICENSE and `MutualCofix Auto` items (moved from "Project
  Structure & Tooling"), whether `Answers Auto` should be the default, the
  semantics decisions Jonah made on 2026-10-02 (classical weak bisimilarity,
  `mutual_sim`/`weak_bisimilar`, `weak_bisim`'s fate, the `Sim Begin`
  similarity change, divergence), the `rocq-sims` audit, and CADP
  no-starvation.

## 2026-10-02 (second session) — Premise shapes surveyed; implication and `forall` premises were silently dropped

**Bug fix** · **Tooling** (tests). On branch `fix/product-premises`. This is
section 2 of the evaluation plan (the premise shapes never tested), agreed
with Jonah.

**Survey.** Each shape probed with `MeBi Run LTS`, and the LTS compared
with the one worked out by hand:

| shape | result |
|---|---|
| mutually inductive LTSs (`odd_step … with even_step …`) | correct |
| value-passing labels (`send n`, `recv n`) over a bounded domain | correct |
| `exists` premise, true or false | correct |
| `~ P` premise | correct |
| `Type`-valued relation | refused, `Invalid_Sort_LTS` (loud) |
| parameterised LTS used through a `Definition` (`st bool`) | refused, `Invalid_Ref_LTS` (loud) |
| **`P -> False` premise** | **wrong, silently**: 6 states where there are 4 |
| **`P -> Q`, `forall k, …` premise** | **wrong, silently** (same) |

**The bug.** Extraction collects a constructor premise only when its type
is an application (`App (h, args)`: `n < 5`, `not (n = 3)`). A premise
whose type is a product, such as an implication or a `forall`, fell into
the case for a variable's type (`n : nat`) and was skipped. It never reached
the decision procedure, and never reached the "cannot decide" warning that
`MeBi Help Premises` promises. The constructor then applied from every
state, so the LTS gained transitions. `~ (n = 3)` worked only because `not`
is a constant applied to an argument; its own unfolding, `n = 3 -> False`,
did not.

**The fix.** In `check_updated_ctx`, and in the look-ahead walk that
gathers earlier premises, a binder whose type is a `Prop` but not an
application is a premise. It goes through `check_unknown_app` like any
other, as `(type, [||])`. So:
- `P -> False` is now decided, through `Premise_search`'s negation case.
- Premises the bounded search cannot decide (universals, other
  implications) are deferred and, if still undecided, **warned about**.
  They still over-approximate, but now say so.

**Tests** (`Test.v` `ProductPremises`, in CI):
- `n = 3 -> False` pinned at exactly 4 states (completes at a bound of 4,
  fails at 3). On the pre-fix code the same pin fails at 4: checked by
  stashing the fix.
- A known-wrong pin for the `forall` premise, with the usual note.

**Not done (possible follow-ups, raise first):**
- Deciding bounded universals (`forall k, k < n -> P k` with `n` closed is
  a finite conjunction). That would be new capability.
- Clearer messages for the two refusals.

**Verification:** all 41 counts identical under `Auto` and forced `True`;
forced `False` matches its documented baseline. None of the examples has a
premise of this shape. `tests.exe` 79/79, `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/product-premises main`).

## 2026-10-02 (second session) — What a CCS example exposed: two unsound search answers, a crash, and a non-terminating proof

**Bug fix** ×3. On branch `fix/refute-closed-steps`. Found while writing the
CCS examples (section 2 of the evaluation plan). CCS uses one `step`
relation at every layer (prefix, choice, parallel, restriction,
definitions), where `Proc` and `CADP` use one relation per layer. That
exercised paths the corpus never had.

**1. `Premise_search` "refuted" true premises (soundness).** When a
constructor's conclusion failed to unify with the goal, the search counted
that as a complete refutation. That is right when the conclusion's indices
are patterns (constructors and variables), and wrong when one is computed
by a function of the binders: `e k : ev k (dbl k)` against `ev 2 4`, or
`do_fix : termLTS (tfix t) None (subst (tfix t) t)`. `w_unify` on the whole
application can fail although the goal holds.
- *Consequence on `main`:* a true constructor premise like `ev 2 4` was
  "refuted", and extraction **silently dropped** its transitions (1 state
  instead of 3, no warning).
- *Fix:* a failed match counts only when the conclusion's indices are
  patterns (the check the search already made for the goal's own indices).
  Before giving up, the match is retried argument by argument, normalizing
  each constructor argument once earlier ones have fixed its variables. So
  `ev 2 4` is now *proved*, not just left undecided.
- *Test:* `Test.v` `ComputedIndex` pins the 3-state LTS; on `main` it
  fails (checked by stashing the fix).

**2. Absurd branches crashed the solver (`CannotGetTransition`).**
Inverting a step such as a handshake opens a branch per way it could have
been derived. The impossible branches carry fully closed false steps
(`step S0 (Some (Out s0)) S0` for a sender that only inputs). The
inversion grade, being shape-based, gives closed steps 0, so nothing
refuted them. The solver went on to look the branch's top-level
transition up in the model, which (not existing) it isn't.
- *Fix:* lazily, only when `get_transition` fails (until now, the crash),
  `Hyps.refutable_step` finds the *smallest* closed step the search
  refutes, memoised by term, and `refute_premise` closes the branch. The
  smallest, because a large one (eight `res` layers) can need more
  inversions than the refutation's depth allows. Trying this eagerly
  first, in the inversion grade, ran the search on every closed
  hypothesis at every step: far too slow. That is also how bug 1 was
  found: it "refuted" a true step in `Test.v`'s `MultipleDerivations`.
  Healthy proofs never reach the lazy path, so their counts cannot move.

**3. Not fixed, documented: the Alternating Bit Protocol does not
terminate one way.**
- *What works:* `MeBi Run Bisim` finds ABP weakly bisimilar to a one-place
  buffer (the textbook result), and `Buf ≤ ABP` proves (331 iterations).
- *What doesn't:* `ABP ≤ Buf`, and so `weak_bisimilar`, runs until memory
  runs out. Inverting a top-level ABP step while its label is still a
  variable gives a goal in which the solver re-inverts the same kept
  `step (def 8) a q'` forever: each round spawns a branch that is refuted
  and one that keeps the hypothesis.
- *Diagnosis:* this is backlog **Step 0** (note 7: inversion keeps the
  hypothesis and the tie-break re-picks it), which on `Proc`/`CADP` costs
  7–14% and here never ends.
- *Status:* the earlier attempts to fix Step 0 were rejected because they
  broke other proofs, so this needs design, not a patch. It is recorded in
  the CCS examples and in the backlog.

**Verification:** all 41 counts identical under `Auto` and forced `True`;
forced `False` matches its documented baseline. `Test.v` builds (with
`MultipleDerivations` at its recorded 38 iterations), `tests.exe` 79/79,
`make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/refute-closed-steps main`).

## 2026-10-02 (second session) — CCS examples: textbook pairs and the Alternating Bit Protocol

**Tooling** (examples). On branch `examples/ccs`. This is section 2 of the
evaluation plan (new term shapes and textbook cases), agreed with Jonah.

`examples/CCS.v`: value-free CCS (Milner 1989) as one inductive LTS.
Names, input/output actions, τ as `None`; prefix, choice, parallel with
handshake (both orientations), restriction of a *list* of names, and
recursion through numbered definitions `def`. One relation, `step`, serves
every layer, unlike `Proc`/`CADP`; that shape is what exposed the bugs fixed
in PR #11. The list restriction is deliberate: eight nested single-name
restrictions made every inversion eight layers deep.

`examples/Bisimilarity/CCS/PluginProofs.v` states exactly what holds for
each pair:
- **Milner's `τ.a + b` / `a + b`:** `weak_sim` both ways (24, 22),
  `mutual_sim`, `weak_bisimilar` refused (`Not_Bisimilar`).
- **`a.b + a.c` ≤ `a.(b + c)`** (34); the converse refused ("not weakly
  simulated").
- **Vending machines:** VM2 ≤ VM1 (37); the converse refused.
- **Two one-place buffers** chained through a restricted name **≈** a
  two-place buffer, as `weak_bisimilar` (204).
- **Alternating Bit Protocol vs. a one-place buffer:** `MeBi Run Bisim`
  decides them weakly bisimilar (the textbook result), and `spec ≤ ABP`
  proves (148). `ABP ≤ spec` is left out with an explanation: it does not
  terminate (backlog Step 0; see the previous entry).

Every refusal was checked for its reason (each run once without its
`Fail`). The files build by default and in CI: the whole `make` takes
about 58 s, CCS under 10 s of it. No plugin code changed.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep examples/ccs main`).

## 2026-10-02 (second session) — Step 0, option A: hypotheses in creation order

**Bug fix** · **Tooling** (tests for option C). On branch
`fix/hyp-creation-order`. Jonah chose option A of note 7's 2026-10-02
re-evaluation; the other options (B–E) are written up there for a later
session.

**The bug.** `try_invert_any` breaks ties toward the *later* candidate,
meant as the newest hypothesis: note 7 recorded that "the old later-first
order works only because each newly added hypothesis sorts last". But
`Hyps.get_non_cofixes` sorted by `Names.Id.compare`, a **string** order
(`H79 < H8 < H80`), and Rocq reuses freed names. Past about ten
hypotheses, "later" was not "newest", and the solver could re-pick a
hypothesis it had already inverted. On the CCS ABP it did so forever
(`inversion H8`, `inversion H80`, …). It is now ordered by introduction
(`Proofview.Goal.hyps` reversed). `get_cofixes` keeps its name order: it
only chooses among equivalent coinduction hypotheses.

**Measured, all 41 proofs, all three modes.**
- *Nothing worse anywhere.*
  - `Auto` and forced `True`: Test1 and Test2 identical; Test3 `weak_sim`
    1073 387 519 519 603 211 331 603 331 → **995 355 483 483 555 195 307
    555 307** (−7 to −10%); Test3 `weak_bisimilar` −8% (14427 → 13203,
    …); CADP `wsim_lts_bigstep` **396 → 355** (−10%, the very proof the
    earlier Step 0 attempts broke); CADP `weak_bisimilar` 2875 → 2760.
  - Forced `False`: Test1/Test2 identical, CADP 396 → 355, Test3 stops at
    1127 as before.
- *ABP:* `abp ≤ spec` no longer loops. In a capped run the open goals fall
  steadily (107 → 97 → 85 at 500 / 1500 / 3000 steps), but at about 200
  steps per goal it would still need some 17,000 more. That remaining cost
  is blind inversion through deep single-relation terms: option D in
  note 7. So `abp ≤ spec` stays out of the CCS example for now.
- `CLAUDE.md`'s baseline is updated, with the old figures kept alongside.
  The checked-in bounds are upper limits, so they still hold.

**Tests for option C** (`Test.v` `InversionShapes`, Jonah's request): small
proofs that pass today, with their counts as reference.
- *Counterexample shapes* for "clear a hypothesis once inverted": `Sync`
  (two LTS premises share a label; 29 / 105) and `Source` (an `In` premise
  fixes the LTS premise's source; 71).
- *The shape C is for*: `Deep`, one relation at every layer (120).
- *A known-wrong pin, found while writing these and present on `main`
  before A too:* with a guard premise (`n < 3`) before a target-computing
  premise, the solver applies `rt1n_refl` to the guard ("Unable to unify
  clos_refl_trans_1n … with 0 < 3"), as `weak_sim` and `weak_bisimilar`,
  in every mode. Not fixed: the next session's.

**Verification:** as above; `tests.exe` 79/79, `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/hyp-creation-order main`). The
counts go back to the old baseline.

**Session tally (2026-10-02, second session), final:** Bug fix 11 ·
New feature 5 · Refactor 1 · Tooling 9 · Docs 5 · Optimization 0.
(PRs #1–#13 and two docs commits; see the entries above.)

## 2026-10-02 (third session) — Step 0, option D′: refute a dead LTS step instead of inverting it

**Optimization.** On branch `perf/refute-dead-hyps`. Jonah agreed option
D′ of `notes/11-dead-hypothesis-refutation.md`, which replaces note 7's
option D (matching inversion branches against the extracted constructor
trees) with something simpler that does the same job.

**What was measured first.** A log-only spike on a capped `abp ≤ spec`
run: 85% of steps were LTS inversions, ~50 per answered move, and **61% of
those inversions were of a step that `Premise_search` already proves has
no instance**. Examples: the receiver asked for an output it never makes
first, or a handshake on a name one side never uses. Every such verdict
was complete. The solver inverted these layer by layer through `var k` /
`def k`, which Rocq's `inversion` cannot discriminate, and refuted each
impossible branch in turn.

**The change.**
- `Premise_search.dead`: turn a hypothesis's local variables into evars
  (`abstract_vars`); the hypothesis is dead when the search finds no
  solution and was complete. Memoised, keyed by the type with its
  variables numbered by occurrence.
- `refute_hyp_tac` also refutes such *open* hypotheses (before, only
  closed ones). After each `inversion_clear` it closes the remaining goals
  from, in order: a dead hypothesis it introduced; a closed one `prove`
  refutes (the old case); else, for premises false only together (a
  handshake's two sides), it inverts the newest one it introduced.
- `try_invert_any`: when the step it has *already chosen* to invert is
  dead, it emits `refute_hyp_tac H || inversion H`. Which hypothesis is
  picked is unchanged. A wrong verdict costs one step, never the proof.

**Two mistakes on the way, both caught before committing.**
- *4× slower at first.* `refute_goal` ran the old un-memoised `prove` scan
  over the whole context before anything else, at every level, and the
  memo lived only in the solver. Reordered (introduced dead hypotheses
  first) and the memo moved into `Premise_search.dead`: per refutation
  ~400 ms → ~70 ms.
- *A quarter of refutations failed and fell back to inversion.* I told
  "hypotheses this refutation introduced" apart by name, and
  `inversion_clear` frees a name that Rocq gives to the next premise. This
  is the same trap as option A's bug. They are now compared by name *and*
  type, and no refutation falls back on the ABP.
- Also corrected before committing: the `Sync` pin's comment first claimed
  inner `comp` premises were refuted. A trace showed it is the top-level
  step of a pair with no move (`sys (s2, t0) a m2`).

**Measured.**
- *ABP* (`abp ≤ spec`, capped at 600 steps): 18 obligations closed instead
  of 9, at about the same time per step (22.7 s vs 20.8 s).
- *All 41 `PluginProofs.v` counts identical* under `Auto`, forced `True`
  and forced `False`, against `main` built the same way; forced `False`
  stops at the same places as on `main`. Wall times within noise.
  `Proc`/CADP use a relation per layer, and `inversion` already discards
  their dead branches, so there was nothing to save there.
- *CCS examples:* chained buffers `weak_bisimilar` 179 → 175 (`Auto` and
  forced `True`), 440 → 434 (forced `False`); the rest unchanged.
- *`Test.v`:* 17 counts drop, none rises, none changes outcome (e.g.
  `TwoPremises` 28 → 27, `OutputPremises` 43 → 40,
  `InversionShapes.Sync` 29/105 → 26/81). `sync_bis` is now pinned at its
  new least bound (`Solve 80`); on `main` it runs out of steps (checked).
  `Deep` stays 120: constructor-headed sources are discriminated by
  `inversion` itself.
- `tests.exe` 79/79; `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep perf/refute-dead-hyps main`).

**Session tally (2026-10-02, third session):** Optimization 1.

## 2026-10-02 (third session) — The Alternating Bit Protocol, proved both ways

**Tooling** (examples). On branch `examples/abp-proofs`. This is step 2 of
note 11's order. With dead steps refuted (previous entry), the proofs the
CCS example had to leave out now go through:

| proof | before | now |
|---|---|---|
| `abp ≤ spec` | did not terminate | 6494 steps, ~3.7 min, ~4.2GB peak |
| `weak_bisimilar abp spec` | did not terminate | 9914 steps, ~3.8 min, ~4.2GB peak |

(`spec ≤ abp` was already in `PluginProofs.v`, 148.) This is the textbook
result (Milner 1989) proved by the plugin, not only decided by `MeBi Run
Bisim`.

They are in `examples/Bisimilarity/CCS/ABPProofs.v` and
`ABPBisimProofs.v`, **commented out in `_CoqProject`** (the CCS example
proper is built by default and in CI, in under 10 s). `CLAUDE.md` lists them
as the check for changes to inversion or to dead-step refutation, which the
six Proc/CADP suites barely exercise. Bounds are the least that close each
proof.

**A mistake on the way:** I first put both proofs in one file. Under a 6GB
cap the second was OOM-killed (the first proof's term stays in memory), so
they are one per file, each run in its own capped process.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep examples/abp-proofs main`).

**Session tally (2026-10-02, third session), cont.:** Optimization 1 ·
Tooling 1.

## 2026-10-02 (third session) — A premise headed by a definition (`n < 3`) was taken for a silent step

**Bug fix.** On branch `fix/premise-under-definition`. This is the
known-wrong pin `Test.v` `InversionShapes.Computed`, found while writing the
option C tests (second session).

**The bug.** For `go n m : n < 3 -> succ_rel n m -> st n (Some true) m`,
the solver applied `go'` on the answering side and then applied
`rt1n_refl` to the premise `0 < 3` ("Unable to unify clos_refl_trans_1n …
with 0 < 3"). This happened as `weak_sim` and as `weak_bisimilar`, in every
cofix mode. `Concl.is_premise` judged the goal by its syntactic head, and
`n < 3` is headed by the *constant* `lt` (it unfolds to `le (S n) 3`). So
it was not seen as a premise, and the constructor-application state machine
took it for the closing silent-step goal. `n <= 3` would have worked.

**The fix.** A goal headed by a constant that is not one of the plugin's
theory definitions is classified by its weak-head normal form, as
`Hyp.premise_grade` already does for hypotheses.

**Tests.** The pin is now `computed_sim` (42) plus a new `computed_bis`
(83). Both also pass with `MutualCofix` forced `True` (42, 83) and `False`
(42, 283).

**Also, a slip of mine from PR #14, fixed here as a separate style
commit:** I inserted the memo table between `Premise_search.dead`'s doc
comment and its definition, so the comment documented the table. `make`
accepted it, because a doc comment before a module is legal. Moved back;
the rest of that commit is ocamlformat drift from the same PR, which went
in unformatted.

**Verification:** the 41 `PluginProofs.v` counts and the CCS ones are
identical to PR #14's in all three modes (21 runs); `Test.v` is otherwise
unchanged; `tests.exe` 79/79; `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/premise-under-definition main`).

**Session tally (2026-10-02, third session), cont.:** Optimization 1 ·
Tooling 1 · Bug fix 1.

## 2026-10-02 (third session) — Step 0 closed: no sterile re-inversions remain

**Docs** (a measurement; no code committed). This is step 4 of note 11's
order. Its result decides whether options B (smaller-first tie-break) and
C (clear a hypothesis once inverted) are worth building: both target
*sterile* steps, where the goal is unchanged and the only new hypothesis is
an exact duplicate.

**Method.** A temporary detector in the solver loop compared each step's
before and after (goal count, focused conclusion, hypothesis types). It
ran over all seven suites (41 Proc/CADP proofs, 6 CCS) and `abp ≤ spec`.
**Validated first**: with the pre-option-A name-order tie-break switched
back on, counts return to the old baseline, and the sterile steps found
equal option A's saving *exactly*, proof by proof. For example, Test3
`wsim_p3` has 78 sterile steps and went 1073 → 995; CADP
`wsim_lts_bigstep` has 41 and went 396 → 355; CADP `weak_bisimilar` has
115 and went 2875 → 2760.

**Result: zero sterile steps on current code**:
- 0 of 55,793 under `Auto` (forced `True` is identical);
- 0 under forced `False`;
- 0 of 6494 on the ABP.

Option A removed all of them. So B and C have nothing to save and are
closed unbuilt. D′ stage 2 is unneeded (the ABP closes), and so is note 7's
tree-path D. **Step 0 is closed.**

**A discrepancy, recorded rather than chased:** with the old order, this
detector gives Test3 8.4% and CADP 4.4% overall, and Test1/2 0%. That is
lower than note 7's 12% / 14.1% / 2.6–4.3% (measured 2026-09-29, before
B2, on earlier code). The exact match with option A's savings is the
evidence that the detector is right for the current code.

The detector and the forced-strategy override are kept as patches in the
local `notes/tools/`.

**Session tally (2026-10-02, third session), final:** Optimization 1 ·
Tooling 1 · Bug fix 1 · Docs 1 (PRs #14–#16 and this entry).

## 2026-10-03 — Comparison cases: instances of CTrees' CCS laws

**Tooling** (examples). On branch `examples/ctrees-ccs-laws`. Note 10, A.4:
CTrees' CCS case study (vellvm/ctrees, `examples/CCS/Denotation.v`, section
`Theory`, at `cabcf9b`) proves ten algebraic laws by hand. This adds
`examples/Bisimilarity/CCS/LawProofs.v`, one `MeBi Sim Solve` per law
instance, built by default (~20 s, ~0.8GB). No plugin code changes.

**Not like for like, stated in the file:** CTrees proves each law for all
`p, q, r` and as *strong* bisimilarity; MeBi proves closed instances and
`weak_bisimilar`, a weaker statement on the same pair. The instances use
three processes that synchronise in a cycle, so regrouping a parallel
composition moves handshakes across the brackets. CTrees'
`unfold_bang'` (`!p ~ !p | p`) is out of scope: replication has no finite
LTS. Beyond CTrees' list: Milner's three tau-laws and `tau.p ≈ p` (weak
only), an expansion-law instance, and three negative neighbours, each
checked to fail as a decided `Not_Bisimilar` (rerun without `Fail`).

| law | Auto | forced True | forced False |
|---|---|---|---|
| `plsC`, `plsA`, `pls0p`, `plsp0`, `plsidem` | 36, 58, 19, 19, 56 | same | same |
| `tau1`, `tau2`, `tau3`, `tau0` | 42, 53, 28, 15 | **29**, 53, 28, 15 | 42, 53, 28, 15 |
| `expansion` | 98 (mutual) | 98 | 155 |
| `paraC`, `para0p`, `parap0` | 278, 357, 325 (mutual) | same | 2218, 3127, 2853 |
| `paraA` | 3192 (mutual) | same | **OOM-killed at 5GB after ~2 min** |

Bounds are the least that close each proof under `Auto`. Two findings,
neither acted on: `paraA` is the strongest case yet for the mutual
strategy (the nested one does not finish at all), and `Auto` picks nested
for `tau1` although mutual is cheaper (42 vs 29), a small mis-estimate by
`Product.estimate` on a tiny case. The parallel laws are last in the file,
so a forced-`False` run reports every other count before it stops.

**A slip on the way, caught before committing:** my first comment called
the expansion law weak-only; it holds strongly too.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep examples/ctrees-ccs-laws main`).

**Session tally (2026-10-03):** Tooling 1.

## 2026-10-03 — An LTS premise whose source nothing determines

**Bug fix** (two) · **Docs** (a correction). On branch
`fix/open-source-premise`. This closes the gap note 9 left open, pinned by
the known-wrong test `OutputPremises.open_c`. I labelled it a bug fix,
not a new capability: the plugin already accepted this shape and gave an
answer that depended on the order the constructors were declared in.
Jonah agreed the approach (note 9, option B) after three small
experiments, recorded there.

**The bug.** For `open_go q n a q' : base q a q' -> n <= 5 -> open_c n a
q'`, nothing fixes `q`, so `base` was explored from an open source `?q`.
Matching a constructor commits its unifications to the shared evar map
(`Pair.unifies`), so the first constructor that matched fixed `?q` for all
its siblings. Only one constructor's steps were found (2 states of 3), and
swapping `base`'s constructors gave a *different* 2. A recursive
constructor was cut off the same way, and that was all that kept it
finite: in a spike with each constructor sandboxed instead (option A),
`ur q a q' -> ur (S q) a (S q')` was OOM-killed at 3GB within 18 s.

**The fix.** Such a premise is now enumerated by `Premise_search` (the
bounded search that already decides general premises, with a completeness
flag), and each distinct source it finds is explored as usual
(`handle_app`'s new `explore_sources`). If the search is cut off, the
existing "may not have found all" warning says so.

**Also fixed, found on the way.**
- *Silent drops.* A found transition whose label or target still had an
  unknown in it (a binder nothing fixes: `u n : ung (S n) None n` reached
  from an open source, or as a plain premise) was dropped without a word
  when the target was a bare evar, and kept as if it were one state when it
  was not (`S (S ?n)`). Both are now dropped with a new warning, "cannot
  determine", naming the term (`warn_undetermined`).
- *Warnings silenced across commands* (separate commit). The once-only
  premise warnings were keyed by the LTS's encoding, which every command
  numbers afresh, so a later command's *different* LTS with the same
  premise head got no warning (reproduced: two LTSs over an opaque `f n =
  0`, only the first warned). Keyed by the LTS's name now.

**Tests** (`Test.v`, `OutputPremises`). `open_c` is now a positive test
(3 states, least bound; `weak_sim` proved in 55 steps), plus a
swapped-constructor copy (same LTS) and a guarded recursive premise (5
states). Known limits, each warned: `open_u` (both ways), `open_u2`,
`open_ur` (stops at the premise depth). Every new `Fail` was checked for
its reason. **A new known-wrong pin:** `w_open_rec`, the guarded recursive
case, stops on an internal `Not_found` in the solver. It is not from this
change: the same LTS with its source fixed by `In q [0; 1; 2; 3]` fails the
same way on `main`, while `rb` alone proves. Left open (note 9).

**A mistake of mine, corrected here.** PR #17's `CLAUDE.md` text and log
entry said forced `MutualCofix False` stops `LawProofs.v` at `paraC`. It
stops earlier, at `expansion`: its bound is 97 and the nested path needs
155. I measured `False` before setting the least bounds and did not
re-check after. `CLAUDE.md` is corrected; the claim above that "a
forced-`False` run reports every other count before it stops" was wrong
for the same reason.

**Verification.** All 27 `weak_sim` and 14 `weak_bisimilar` counts, the
CCS counts and the 14 `LawProofs.v` counts unchanged under `Auto`, forced
`True` and forced `False` (which stops where it did before). ABP:
`abp <= spec` 6494 and `weak_bisimilar abp spec` 9914 (`Auto`), unchanged, ~3.7 min and ~4.4GB each. `tests.exe` 79/79, `satdiff -- 200` identical, `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/open-source-premise main`).

**Session tally (2026-10-03), cont.:** Tooling 1 · Bug fix 2 · Docs 1.

## 2026-10-03 — The solver read a premise's step as the transition to answer

**Bug fix.** On branch `fix/solver-not-found`. Found while fixing the
open-source premise (previous entry), pinned there as the known-wrong
`OutputPremises.w_open_rec`, and present on `main` before it.

**The bug.** To answer a step of the system being simulated, the solver
reads it back from a hypothesis (`Hyps.get_transition`): the first one
headed by *any* relation in `Using` whose terms are states of the model.
After inverting `open_rec 0 a 3`, a premise's own step, `rb 2 (Some true)
3`, is among the hypotheses, and since both relations' states are
numbers, it reads as a step of `open_rec` from state 2. State 2 has no
edges, so `EdgeMap.find` raised a bare `Not_found`, which the scan does
not catch, and the solver stopped on an internal error. Had state 2 had
edges, the solver would have answered the wrong transition and only `Qed`
would have caught it. `via_c` (Test.v) proved only because its top-level
hypotheses happened to come first.

**The fix.** `Hyps.get_transition` takes the relation being simulated,
read from the conclusion's `weak_sim`/`weak_bisimilar` conjunct (argument
3, or 4 when swapped, beside the `a'` that `get_a'_from_wk_sim` already
reads), and reads only steps of that relation. Defensively,
`ReModel.transition` raises its own `CouldNotFind_Transition` for a state
with no edges, which the scan skips, instead of `Not_found`.

**Tests.** `w_open_rec` is now a proof (47 steps). The same LTS with its
source fixed by `In q [0; 1; 2; 3]` (failing on `main` too) proves in 53.

**Verification.** `Test.v`'s 38 other counts identical to `main`'s
(compiled side by side with notices on). All Proc, CADP, CCS and
`LawProofs.v` counts identical under `Auto`, forced `True` and forced
`False`. ABP 6494 and 9914, unchanged. `tests.exe` 79/79, `satdiff -- 200`
identical, `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/solver-not-found main`).

**Session tally (2026-10-03), cont.:** Tooling 1 · Bug fix 3 · Docs 1.

## 2026-10-03 — An approximate LTS is incomplete

**Bug fix.** On branch `fix/approximate-lts-incomplete`. Point 2 of the
open list after PR #18, agreed with Jonah: an LTS that extraction knows to
be only an approximation was still reported complete.

**The problem.** Extraction warns when an LTS may contain transitions
that do not exist (a premise it could not decide: over-approximation) or
may be missing some (a premise search cut short, a transition it could
not determine: under-approximation). But the LTS was still marked
`complete`, so `MeBi Run Bisim` gave a verdict on it as if it were exact,
and `MeBi Sim Begin`, which decides bisimilarity first, could refuse a
true goal on a wrong "not bisimilar". The warnings are printed once per
LTS and session, so a later command on the same LTS had no sign at all.

**The fix.** No new option: the existing `MeBi Config FailIf Incomplete`
covers it. A record (`Rocq_monad_utils.Approximations`, outside the
functors) is reset at the start of each extraction and noted by the three
warnings *every* time they apply, printed or not. A non-empty record marks
the LTS incomplete, and `LTS_Incomplete` then says why: "only an
approximation", with the reasons (up to three, "and N more"), and/or the
bound, as before. `FailIf Incomplete False` accepts it, as it already did
for the bound. Help texts for `FailIf` and `Premises` updated.

**Tests.** The known-limit pins `open_u` (three ways) and `open_ur` now
`Fail` by default, each checked to fail for the approximation and not the
bound, and are also run under `FailIf Incomplete False`. The older
`Fail`s over undecided premises (`st`, `le_c`, `univ`) still fail on the
bound, and now give the approximation as a second reason.

**Verification.** No example's LTS is approximate: all Proc, CADP, CCS
(including ABP's extraction, by its `Run Bisim`) and `LawProofs.v` counts
unchanged under `Auto`. The solver is untouched, so forced modes were not
rerun. `tests.exe` 79/79, `satdiff -- 200` identical, full `make` clean.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/approximate-lts-incomplete main`).

**Session tally (2026-10-03), cont.:** Tooling 1 · Bug fix 4 · Docs 1.

## 2026-10-03 — A premise's witness that no LTS step fixes, in proofs

**Bug fix.** On branch `fix/premise-witness-in-proofs`. Point 3 of the
open list after PR #18: a constructor whose binder appears only in
premises that are not LTS steps extracted correctly, but its proof
stopped.

**The bug.** With `base` *not* in `Using`, `open_go q n a q' : base q a q'
-> n <= 5 -> open_c n a q'` gives the right LTS: the premise search
enumerates `base`. In the proof, `econstructor` leaves `q` an evar, and
since no LTS premise's derivation is replayed to fix it, the solver met
`base ?q (Some true) 1` and stopped: the search proves closed goals only
("cannot prove the constructor premise"). The same with two plain
premises sharing the witness (`In q [0; 1] -> base q a q' -> ...`), where
choosing `q` for one premise alone can break the other.

**The fix.** A new step in the constructor tactic, after
`move_premises_last` while all the subgoals are visible
(`fix_premise_witnesses`): the evars that occur in premise goals but in no
LTS goal are witnesses nothing else will fix. The premise goals mentioning
them are enumerated *together*, as one conjunction, and the first solution
giving each a closed value is committed. Any such solution will do, since
no other goal mentions them. A goal that also mentions an evar an LTS
premise fixes is left alone, so as not to pre-empt that replay (`via_c`'s
`In q [n; S n]`, whose `q` the `base` step fixes, is unchanged). With no
such witnesses (every example) the step does nothing.

**Tests.** `Test.v` `OutputPremises`: `w_open_plain` (55 steps) and
`w_two_plain`, the shared witness (25), both stopping before.

**Verification.** `Test.v`'s 39 other counts identical to `main`'s.
All Proc, CADP, CCS and `LawProofs.v`
counts identical under `Auto`, forced `True` and forced `False`. ABP 6494
and 9914, unchanged. `tests.exe` 79/79, `satdiff -- 200` identical, full
`make` clean.

**A slip, caught before committing:** factoring out `is_lts_goal`, I put
it between `move_premises_last`'s doc comment and its definition, the
warning-50 trap the notes describe; `dune build` accepted it, `make`
rejected it. Moved back.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/premise-witness-in-proofs main`).

**Session tally (2026-10-03), cont.:** Tooling 1 · Bug fix 5 · Docs 1.

## 2026-10-03 — Saturation on demand, and bisimilarity on the silent-SCC quotient

**New feature** (a config option and a new code path for large FSMs) ·
**Refactor** · **Tooling** (tests). On branch `feature/on-demand-saturation`.
Stage 1 of note 13, agreed with Jonah after measuring: `Proc/Test4`'s
saturation (74.6M weak actions, 34-67GB) was refused outright; the
plan is to decide it (this entry) and, separately, to prove it up to
silent steps (stages 2-3).

**What it does.**
- `FSM.saturate_on_demand`: an FSM whose saturated edges are filled one
  state at a time when first asked about (`FSM.ensure`), holding at most
  a budget of weak actions (oldest dropped, recomputed if needed). It runs
  the *same* per-state code as `FSM.saturate` (refactored out as
  `Saturation.state_actions`), so the weak actions and witnesses are
  identical: no "3b" divergence. `Product`'s three readers of saturated
  edges call `ensure` first.
- `SaturationEstimate.partition`: weak bisimilarity on the quotient by
  silent SCCs (states in one silent SCC have the same weak moves and are
  weakly bisimilar), refined by weak and `=eps=>` moves and expanded to
  states. Memory is the number of SCC-level weak moves: 5,184 for
  `Test4`'s 9720 states.
- `MeBi Config Saturation OnDemand True | False | Auto` (default `Auto`,
  reset by `Reset Bounds`). `Auto`: an FSM whose saturation would exceed
  `Bounds Saturation` -- until now an error for `Run Bisim` and `Sim
  Begin` -- is saturated on demand, with a warning giving the estimate and
  the memory cap; `True` forces it (for measuring); `False` restores the
  refusal. `Run Saturate` and `Run Minimize` still saturate whole. Help
  topics updated.

**Tests.** `tests.exe` (84/84): on demand = whole, state by state, on 300
random LTSs at budgets 1M and 1 (every fill evicting); quotient partition
= saturated partition on 300 LTSs and 150 merged pairs; Milner's pair
kept apart. `Test.v` `SaturationGuard`: at bound 1, `Run Bisim` and a
`weak_sim` proof now succeed on demand, in the same 38 steps as saturated
whole and as forced on demand; `OnDemand False` refuses as before.

**Evaluation (Jonah's request: should it be used always?).**
- *E1, the default:* every count unchanged (all six suites, CCS,
  `LawProofs.v`, `Test.v`'s 41); no example triggers it.
- *E2, forced on demand for every example:* first run, every count
  identical, peak memory identical to within 0.05GB, wall time within
  run-to-run noise either way. But see the fix below: on demand, `Auto`
  now takes the nested cofix, so forced on demand reproduces forced
  `MutualCofix False` exactly (CADP stops at 2875, Proc/Test1 at 709,
  Test2 the nested counts, CCS 434, `LawProofs` at `expansion`). Using it
  always would give up `Auto`'s mutual choice and fail checked-in bounds,
  for no memory gain on these examples: **not as a default**.
- *E3, `Proc/Test4`:* `Run Bisim p q` now **succeeds in 45s** (two 21s
  extractions and the quotient check), instead of being refused (and
  before the guard, taking the machine down). `Sim Begin` takes 45s too.
  The proof itself remains out of reach without stage 3: 301 solver steps
  took 68s (0.23s a step, ~70x Proc/Test3's, since dropped states are
  saturated again) and 2.0GB, against ~525k steps needed.

**A problem found by E3, and fixed (separate commit).** On demand,
`Sim Begin` on `Test4` was still running after 29 minutes: `Auto`'s
strategy estimate (and any non-default answer policy's plan) walks every
reachable pair of the product game up front, saturating state after state
as the cache drops them. When either FSM is saturated on demand both are
now skipped, with a notice: answers move by move, nested cofix.

**Mistakes on the way, caught before committing.** A test of mine set
`OnDemand True` and then `Reset Bounds`, which resets the mode, so that
proof ran saturated whole (spotted: it gave no warning). `make` rejected
two things `dune build` accepts: a doc comment directly after a `val`
(warning 50, "ambiguous"), and `Option.value`, which the packed build
resolves to another module (`Stdlib.Option` now).

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep feature/on-demand-saturation main`).

**Session tally (2026-10-03), cont.:** Tooling 2 · Bug fix 5 · Docs 1 ·
New feature 1 · Refactor 1.

## 2026-10-03 — Proofs up to silent steps: transfer lemmas

**New feature** (theory; additive). On branch
`theories/silent-transfer-lemmas`. Stage 2 of note 13. Nothing existing in
`theories/Bisimilarity.v` is changed, and the plugin does not use the new
lemmas yet (stage 3).

**The lemmas.** With `silent` the reflexive-transitive closure of silent
steps:
- `weak_sim_silent_l`: `silent ltsM r m -> weak_sim r n -> weak_sim m n`;
  every move of `m` is a weak move of `r`, answered by `weak_sim_act_clos`.
- `weak_sim_silent_r`: `silent ltsN n n' -> weak_sim m n' -> weak_sim m n`;
  `n` answers as `n'` does, after the silent steps.
- `weak_bisimilar_silent_l` / `_r`: the same for `weak_bisimilar`, needing
  silent reachability *both ways* (within a silent SCC): the other side's
  moves must be answered too, which `m` can only do by first reaching `r`.
- Helpers `weak_silent_prefix`, `weak_after_silent`.
None needed a new coinduction; all are closed under the global context
(`Print Assumptions`, in `Test.v`). **Corrected later the same day:** these
lemmas cannot shrink a proof as intended (used inside the coinduction they
are circular and unsound); see "Stage 3 abandoned" below.

**Tests.** `Test.v` `SilentTransfer`: on a two-state silent cycle, the
plugin proves one pair per relation (16, 37, 11 steps) and the lemmas give
the other member, on both sides, for `weak_sim` and `weak_bisimilar`.

**A bug found on the way, not fixed here (open).** Writing those tests,
`weak_sim cyc lin 0 0` stopped on an internal error although it holds.
When the two systems' state terms coincide (both `nat`, both from `0`),
the bisimilarity check merges them into one state: `MeBi Run Bisim` then
called `lin` (does `a`) and `other` (does `b`) **bisimilar**. Renumbered
apart, both are right. Proofs stay sound (`Qed`), but a `Run Bisim`
verdict can be wrong. The checked-in examples compare distinct terms, or
structurally identical copies where the merge happens to be harmless. The
tests number the second system apart, with a comment. Fix to be agreed:
note 13. **Corrected in the next entry:** that claim about the examples was
wrong; CADP `Glued` was affected.

**A slip of mine:** my first commit on the branch did not build. I tested
the new `Test.v` module in a standalone copy that imported
`Relation_Operators`, which `Test.v` does not, and chained the commit
after the build in one command without gating on it. Fixed in the next
commit (qualified `Relation_Operators.rt1n_trans`); both builds checked
before it.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep theories/silent-transfer-lemmas main`).

**Session tally (2026-10-03), cont.:** Tooling 2 · Bug fix 5 · Docs 1 ·
New feature 2 · Refactor 1.

## 2026-10-03 — Two systems whose state terms coincide

**Bug fix.** On branch `fix/overlapping-states`, two commits: a guard,
then the fix it guards. Found during stage 2 (previous entry). Jonah's
concern was overhead for everyone for a corner case; both parts cost
nothing unless the case arises.

**The bug.** `Bisimilarity.fsm` merges the two FSMs, taking a shared
state (the same term, so the same encoding) for one state. Right when both
sides use the same relation (Proc's `p` vs `q`, the CCS pairs: a shared
term moves the same way on both sides); wrong when they do not. Two
relations over `nat`, both from `0`: `Run Bisim` called `lin` (does `a`)
and `other` (does `b`) bisimilar, and a true `weak_sim` stopped on an
internal error.

**Not a corner case after all -- my earlier claim was wrong.** I said the
checked-in examples were unaffected. The guard, run over the matrix,
stopped CADP `Size1/Glued` and `Size1/Glued/MutualExclusion` at once:
their `bigstep` proofs compare two different semantics whose states share
terms, 8 of which move differently. Their bisimilarity checks had been
running on a conflated merge; the proofs passed because `Qed` checks them
independently, and (measured below) the conflation did not change their
counts.

**The fix.**
- `Bisimilarity.conflicts a b`: shared states whose moves (labels and
  targets) differ. Cheap: only shared states are compared.
- The guard (first commit): refuse, naming a state, in `Run Bisim`,
  `Sim Begin` and `Run Merge`. Kept after the fix as an assertion.
- The fix (second commit), only on conflict: `B`'s copies of *all* shared
  states are renamed apart (`Wrapper.separate`), each to a fresh encoding
  that decodes to the same term (`Bi_encoding.alias`), so proofs and output
  read the right terms; the solver's term-to-state lookup
  (`ReModel.state`) falls back to a term's aliases. A notice says so.
  `FSM.rename` is the model side. With no conflict nothing changes.

**Tests.** `tests.exe` 93/93: `conflicts`, `FSM.rename`, and the bug
pinned at model level (not renamed: wrongly bisimilar; renamed: not).
`Test.v` `OverlappingStates`: `lin`/`other` now `Not_Bisimilar` (checked);
`cyc`/`lin` bisimilar, with proofs in 16, 37 and 11 steps -- exactly the
counts of the same pairs numbered apart by hand (`SilentTransfer`);
identical copies still share states, no notice.

**Verification.** All Proc, CADP, CCS and `LawProofs.v` counts identical
under `Auto`, forced `True` and forced `False`, with the CADP `Glued`
proofs now separated; `Test.v`'s 47 other counts identical to `main`;
`satdiff` identical; `make` clean. ABP not rerun: same relation on both
sides, so nothing is renamed, and the solver change is a fallback on a
lookup miss that needs aliases.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/overlapping-states main`).

**Session tally (2026-10-03), cont.:** Tooling 2 · Bug fix 6 · Docs 1 ·
New feature 2 · Refactor 1.

## 2026-10-03 — Stage 3 abandoned; Test4 under a normalised semantics

**Docs** · **Tooling** (tests). On branch `docs/test4-proof-limit`.

**Stage 3 abandoned, and a wrong estimate of mine.** Note 13's plan was
to prove `Test4` one state per silent SCC, transferring to the other
members with the stage-2 lemmas. Working out how the solver would use them,
before writing code: inside a coinduction, a representative's silent move
to another member of its SCC would be "answered" by transfer from the very
pair being proved. That is circular, and unsound: `r <-tau-> m`, `m -b->`,
against `n` with no moves, "proves" `n` simulates `r`. Rocq's guard check
rejects the circular term, so no false proof could have resulted -- the
stage would just never have closed a proof. A sound version must answer
every member's moves (>= 87,480 for `Test4`), so my estimate of "81
representatives x ~9 moves, ABP-sized" (note 13, (b)) was wrong.

**Is earlier work now redundant?** Reviewed with Jonah:
- stage 1 (PR #22, saturation on demand and the quotient partition): no;
  it decides FSMs too large to saturate (`Test4` in 45s), and costs
  nothing below the guard;
- the overlap fix (PR #24): no; independent, a real wrong-verdict bug;
- stage 2's lemmas (PR #23): they no longer serve stage 3, but are sound
  library lemmas for transfer *outside* a coinduction (`SilentTransfer`);
  kept, and their comment in `Bisimilarity.v`, which suggested using them
  to shrink proofs, corrected.

**Tests.** `Test.v` `CircularTransfer`: the plugin refuses the false goal
(checked: "not weakly simulated"); the circular term fails `Guarded`
(checked: "Recursive definition ... ill-formed"); and the goal is proved
false by hand (`~ weak_sim ...`).

**Structural congruence (Jonah's question), measured, not committed.**
`Test4`'s semantics makes congruence silent steps (`do_comm`,
`do_assocl`/`r`), hence 81 SCCs x 120 shapes. Explicit congruence rules
as premises would not help (the plugin would enumerate all 120 congruent
targets); a canonical representative does: `core p a q -> nLTS p a (norm
q)`, with `norm` flattening, sorting and rebuilding the parallel
components, and `core` = `compLTS` without the congruence rules -- all
expressible with the plugin as it is. In a scratch file: 82 states (not
9720), `Run Bisim` 2.2s, and **`weak_sim nLTS nLTS p q` proved** (46,269
steps, 345s, `Qed` 43s, 3.9GB peak). `weak_bisimilar p q` predicted 52,088
moves and did not finish within 25 minutes. It proves `nLTS`, not
`compLTS`: linking them is one generic hand proof (congruence is a
bisimulation, `norm p == p`). Next step proposed to Jonah: an example
splitting the two semantics.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep docs/test4-proof-limit main`).

**Session tally (2026-10-03), cont.:** Tooling 3 · Bug fix 6 · Docs 2 ·
New feature 2 · Refactor 1.

## 2026-10-03 — Test4 proved, via structural congruence made explicit

**New feature** (example and theory; no plugin code). On branch
`examples/test4-congruence`. Jonah's suggestion: split `Test4`'s semantics
into one with structural congruence embedded (as `Proc.Layered.compLTS`
has it, as silent steps) and one with it explicit.

**What was added.** `examples/ProcCongruence.v`, module `Normalised`:
- `congr`: structural congruence as an explicit relation (comm, assoc,
  the unit law `tend | x == x`, contexts, equivalence);
- `norm`: a canonical representative (unfinished components, sorted,
  rebuilt right-nested); `core`: `compLTS` without its congruence rules;
  `nLTS`: `core` with every target normalised;
- `link`: `Permutation (comps c) (comps d) -> weak_bisimilar compLTS nLTS
  c d`, one cofix (congruence steps answered by standing still, component
  steps by the same component on the other side); `congr_link`;
  `wsim_transfer` and `wbis_transfer`: what the plugin proves over `nLTS`
  holds over `compLTS`. All axiom-free (`Print Assumptions`).

**A detail the scratch experiment had glossed over.** `compLTS`'s
`do_par_end` (`tend | tend -tau-> tend`) cannot fire under normalisation
unless the two finished components happen to be adjacent, so `norm` must
also drop finished components (the unit law); without that, `link` is
false. `Test4` never reaches `tend`, so its 82 states are unaffected.

**Results.** `Test4/NormTermTests.v` (built by default): 82 states from
each of `p`, `q`, `r` (least bound pinned), `Run Bisim` 0.4s each.
`Test4/NormProofs.v` (not built by default): the plugin proves `weak_sim
nLTS nLTS p q` in 48,821 steps (356s, `Qed` 42s), and `wsim_transfer`
gives **`weak_sim compLTS compLTS p q`** -- `Test4` proved in the semantics
it was written in, 6.7 min, 3.8GB peak. `weak_bisimilar` is not attempted:
52,088 moves predicted, unfinished after 25 minutes.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep examples/test4-congruence main`).

**Session tally (2026-10-03), cont.:** Tooling 3 · Bug fix 6 · Docs 2 ·
New feature 3 · Refactor 1.

## 2026-10-03 — On demand: refuse explicit whole-game settings

**Bug fix** (behaviour change). On branch `fix/on-demand-strategy-refusal`.
Item 2 of the open list; Jonah's decision: refuse as the baseline, then
discuss bounding.

When an FSM is saturated on demand, planning the whole proof up front --
the mutual cofix's pair set, or a non-default answer policy's plan -- walks
every reachable pair, saturating state after state. Until now a forced
`MutualCofix True` was not guarded (it would stall silently) and a
non-default answer policy was silently downgraded to `Default`. Both are
now refused with an error saying why and what to use instead. `Auto` is
the tool's own choice, so it still takes the nested cofix, with a notice.
Help (`Config Saturation`) updated.

**Tests.** `Test.v` `SaturationGuard`: forced on demand with `MutualCofix
True`, and with `Answers Greedy`, `Sim Begin` fails (each checked: the new
error). A slip caught by the build: my first version of the test left
`OnDemand True` set, so the next test (expecting `OnDemand False` to
refuse) passed instead of failing; fixed.

**Verification.** `tests.exe` 93/93, `make` clean; the on-demand path is
not triggered by any checked-in example by default, so their counts are
unaffected by construction.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep fix/on-demand-strategy-refusal main`).

**Session tally (2026-10-03), cont.:** Tooling 3 · Bug fix 7 · Docs 2 ·
New feature 3 · Refactor 1.

## 2026-10-03 — The Minimal answer planner, near-linear; Test4's weak_bisimilar proved

**Optimization** · **Tooling** (example). On branch `perf/minimal-relation`.
Item 3 of the open list: why `Test4`'s `weak_bisimilar` (under the
normalised semantics) was out of reach, walked through with Jonah.

**Why it was expensive.** Every `Test4` state is weakly bisimilar to every
other, so the default answers ("shortest witness into the class, lowest
state") scatter over nearly all 82 x 82 pairs: 6592 pairs, 52,088 moves.
The opt-in planned policies predict `Greedy` 1874 pairs / 14,400 moves
(133k steps) and **`Minimal` 245 / 1912 (32k steps)** -- but `Minimal`'s
plan took **19.5 minutes**.

**The fix, in two rounds (the first was not enough).**
`Policy.minimal_relation` removes, from every pair any answer reaches, the
first pair (in `Pair.Set` order) whose removal keeps every remaining pair
able to answer all its moves, then trims what the root no longer reaches,
until nothing is removable. It re-validated every remaining pair per
candidate removal, and re-walked the whole relation per removal. My first
rewrite (a reverse index and answer counts for the removability test) kept
the per-removal re-walk and still ran past 10 minutes; I had not measured
which cost dominated. The second: removability as a count of moves a pair
alone still answers, with the removable pairs in an ordered set; and
reachability as a spanning tree, where a removal can only disconnect its
own subtree, re-attached through any other remaining predecessor.

**Same relation.** The removals and their order are the original's, so the
relation is the same: `tests.exe` keeps the original algorithm as a
reference and compares on 400 random games (95/95); `Test.v`'s `Greedy`,
`Minimal` and `Auto` proofs unchanged (51 counts identical to `main`);
`Test4`'s plan identical (245 pairs, 1912 moves, witness 6082); `make` clean after a blank line it wanted (warning 50, again). Default
answers do not use it, so the proof matrix is unaffected by construction.

**Outcome.** `Test4`'s `Minimal` plan: **19.5 min -> 5.6s** (`Auto`,
planning all three, 9.2s). Then **`weak_bisimilar nLTS nLTS p q` proved**:
61,161 steps (the plan predicted ~32k; the fit is 2x low here), 202s,
`Qed` 41s, 2.8GB peak; `wbis_transfer` gives `weak_bisimilar compLTS
compLTS p q`. In `Test4/NormBisimProofs.v`, not built by default.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep perf/minimal-relation main`).

**Session tally (2026-10-03), cont.:** Tooling 4 · Bug fix 7 · Docs 2 ·
New feature 3 · Refactor 1 · Optimization 2.

## 2026-10-03 — A bound on the up-front game walk, required on demand

**New feature** (a config option). On branch `feature/game-bound`. Item 2
of the open list, after PR #27's refusal baseline. Jonah: add bounding,
and enforce that a bound is given for the explicit settings.

**What.** `MeBi Config Bounds Game <n>` (unset by default; reset by
`Reset Bounds`). Every game walk (`Product.reachable_by`, the planners'
walk) counts the pairs it visits and, under `Product.with_cap n`, raises
`Game_too_large` past `n`. When an FSM is saturated on demand:
- `MutualCofix True` or an answer policy other than `Default` is refused
  unless the bound is set (the message says how);
- with it, the walk runs within the bound: the plan, or for `MutualCofix
  True` a validating estimate at `Begin` (the mutual block's own walk, in
  the first step, is then known to fit); past it they are refused, naming
  the bound;
- `Auto` without the bound takes the nested cofix (as before); with it, it
  estimates within the bound, and past it takes the nested cofix with a
  notice.
Below the saturation bound nothing changes: no cap is ever set.

**Why bounding helps (as explained to Jonah).** On demand, the walk's
size is unknown until it is done and each pair may saturate a state; a
pair bound turns "never on demand" into "on demand when the game is
small", with the cost bounded. The per-state saturation speed-up (PR #28,
35ms a state on `Test4`) makes moderate bounds affordable. Caveat: pairs
bound time only indirectly.

**Tests.** `tests.exe` (99/99): a capped walk raises past the cap, not at
it, and the cap is lifted afterwards. `Test.v` `SaturationGuard`, forced on
demand: with `Bounds Game 1000`, `MutualCofix True` proves (38) and
`Answers Greedy` proves (31); with `Bounds Game 1`, both are refused
(checked: "more than 1 pairs"), and `Auto` falls back to nested (notice,
38). `Test.v`'s other counts identical to `main`; `make` clean. The proof
matrix is unaffected by construction (no cap is set below the saturation
bound).

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep feature/game-bound main`).

**Session tally (2026-10-03), cont.:** Tooling 4 · Bug fix 7 · Docs 2 ·
New feature 4 · Refactor 1 · Optimization 2.

---

## 2026-10-03 — Saturation per state, linear in its output (3b's remainder)

**Optimization** (behaviour change in tie-breaking only). On branch
`perf/action-hash`, two commits. Item 1 of the open list, done at Jonah's
request, with the outcome reported for a keep/revert decision.

**Measured first, and a wrong guess of mine.** I estimated that on
`Proc/Test4` a state's saturation examined only ~2.5x its output, so the
planned linear rewrite would gain little. A probe loading `Test4`'s dumped
FSM into the pure-OCaml model (kept out of the repo:
`notes/tools/satprobe.ml`) showed otherwise: **2.9s per state**, with
**2.3M-4.7M candidate witnesses** for 5,280-7,680 weak actions. My estimate
assumed silent closures stay within an SCC; they do not (`do_fix` and
`do_seq_end` are silent steps that change a component's local state).
`perf` is not permitted here, so phases were timed in a replica:
enumerating witnesses dominated; building annotations, the set and the map
took under 45ms together.

**Two changes.**
- `Action.hash` hashed only the label, so all of a state's weak actions
  under one label shared one bucket of the action map, and each insertion
  scanned it with a deep equality. Now a saturated action also hashes its
  witness (length, first and last state); one without a witness hashes as
  before, keeping unsaturated FSMs' table order, which
  `ReModel.transition` breaks ties by. (Alone: state 1 508 -> 349ms.)
- `Saturation.edge_bfs` replaces `edge_closure`: per label, a
  breadth-first search over silent steps from every `t` with `from -tau*->
  s -a-> t` (each at its shortest distance), each state settled once.
  `edge_closure` and its `Key` table are removed.

**Outcome.**
- Speed on `Test4`: **2.9s -> 35ms per state** (83x); state 0, building
  closures, 8.9s -> 46ms. On demand, 301 solver steps on `Test4` (original
  semantics): **68s -> 1.8s**, peak **2.0GB -> 0.76GB**.
- Output: same weak actions, same witness lengths everywhere; among
  witnesses of equal length, a different one kept for **2 of the 2426** in
  `satdiff` (the silent path after the visible step). `satdiff.expected`
  regenerated for those 2 lines.
- Proofs: every count unchanged -- all Proc, CADP, CCS, `LawProofs.v`
  under `Auto`, forced `True` and forced `False`; `Test.v` identical to
  `main`; ABP 6494/9914; `Test4/NormProofs.v` 48,821. `tests.exe` 93/93,
  `make` clean.

**Mistakes on the way.** A `pkill -f` matched my own shell and killed it.
Removing the dead `edge_closure` I first cut `closures_of` too, which sat
between it and the new code; `make` caught it, restored from `HEAD`, and
`satdiff` checked byte-identical to the verified version afterwards.

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep perf/action-hash main`).

**Session tally (2026-10-03), cont.:** Tooling 4 · Bug fix 7 · Docs 2 ·
New feature 4 · Refactor 1 · Optimization 3.

## 2026-10-04 — Bounded universal premises decided, and proved by cases

**New feature.** On branch `feature/bounded-universals`. Item 1 of the
new-capability agenda (notes/14), agreed with Jonah before any code, with
items 3, 4 (as "design B": `Run Bisim` opening the proof itself, no
cache) and 8 to follow; item 5 closed as covered by `weak_bisimilar`; 2, 6
and 7 deferred.

**What.** A premise `forall k, k < n -> P k` or `forall k, k <= n -> P k`
over `nat` (and `n > k`, which is `k < n`), with `n` a numeral (at most
1024) once the premise is closed, used to be beyond the bounded proof
search: undecided, applied as if it held, the LTS refused as incomplete
(`Test.v` `ProductPremises.univ`, pinned KNOWN WRONG). Now
`Premise_search.prove` recognises the shape (on Peano's `le` after
unfolding, so `lt`/`gt` come for free) and decides it instance by instance:
true iff every `P i` is proved, false as soon as one is refuted, else
undecided as before. Each `P i` goes through `prove` itself, so negations,
equations, nested bounded universals and the user tactic all apply.
- **Extraction** only needs the verdict, a new `Proved ByCases`: no proof
  term is built per state.
- **Proofs**: `Premise_search.premise_tac` builds the term when the
  solver asks, proving each `P i` as a sub-proof and chaining four small
  lemmas in a new `theories/Premises.v` (`bounded_le_0`, `bounded_le_S`,
  `bounded_lt_0`, `bounded_lt_S`; exported by `MEBI.loader`); the solver
  now classifies such a goal as a premise (it had taken it for a silent
  step and tried `rt1n_refl`). A false one in a hypothesis is refuted at
  its first false instance: `H i (le_S ... le_n)`, then the usual
  refutation of `P i`.
- Not covered: a bounded universal nested inside another inductive's
  constructor search (only premises in their own right), bounds that stay
  open, and forms other than `<`/`<=` over `nat` (`In k l`, `Forall`
  already go through the constructor search).

**Tests.** `Test.v` `ProductPremises`: the KNOWN WRONG pin is now positive
(4 states, not 6), plus `<=`, `>`, a negated universal, and two `weak_sim`
proofs that use both directions (proved at states 0..2, refuted at 3:
33 iterations; the negated form: 23); `Print Assumptions` closed. Every
other `Test.v` count identical to `main`. Proof matrix: `Auto` and forced
`True` match `CLAUDE.md`'s baseline (27 + 14 + `LawProofs`' 14); forced
`False` identical to `main` file by file. `CCS/ABPProofs.v` 6494 and
`ABPBisimProofs.v` 9914, as before (221s, 227s). `tests.exe`
99/99; `make` clean. Docs: README and `MeBi Help Premises`.

**Review (Jonah), same branch.** `bounded_universal` was one dense,
uncommented nest of matches. Rewritten as a short `Option` pipeline over
helpers with one job each (`split_forall_implies`, `is_nat`, `as_le`,
`strictness`, `to_int`), and the proof tactic split the same way
(`bounded_lemmas`, `subproof`, `bounded_le_chain`, `bounded_proof`); every
new function now has a header comment saying what it does with its
arguments. Two flaws in my first version, found while doing so: `n` was
fully normalised *before* the cap was checked, so a bound like `2 ^ 30`
would have been computed in full (now read one `S` at a time, giving up
past the cap); and refuting a hypothesis ran the counterexample search
twice (now once). `Test.v` counts identical before and after.

**Cost, measured** (one premise, one state, `weak_sim` to a copy). With
`P k := k = k`: n = 100 0.2s / 0.3GB in all, n = 1000 6s / 0.4GB. With
`P k := k <> 5000` (each instance refuted by inversion over unary
numerals): extraction 0.24s / 0.88s / 9.1s at n = 100 / 300 / 1000; the
whole proof 12s / 37s / 190s, peak 0.6 / 1.3 / 3.9GB. So the cost is
dominated by each instance's own decision, and the fixed cap of 1024 is a
crude guard: fine for cheap predicates, far too generous for dear ones.
Raised with Jonah, who chose a setting with a lower default: **`MeBi
Config Premise Range <n>`** (default 256; reset by `Reset Premise` and
`Reset Bounds`, as `Premise Depth` is) caps the values of `k`, and a
premise over it now gets its own warning naming the range (it used to fall
into the generic "not closed, opaque, or deeper search" warning, none of
which was the reason). `Test.v` checks the edge: `univ` at state 3 needs 3
values, decided with `Range 3`, refused as incomplete with `Range 2` (the
`Fail` checked for that reason). After the refactor the proof matrix is
identical to the first run in all three modes; after the range, under
`Auto` (forced modes not rerun: the change only applies to bounded
universals, which no example has).

**How to revert:** delete the branch before merging; after merging with a
merge commit, `git revert -m 1 <merge-commit>` on `main` (find it with
`git log --merges --oneline --grep feature/bounded-universals main`).

**Session tally (2026-10-04):** New feature 1 (with its `Premise Range`
setting) · Refactor 1 (review).

## 2026-10-04 — Structural congruence as a recipe (docs)

**Docs.** On branch `docs/congruence-recipe`. Item 8 of the agenda, agreed
as documentation only (no plugin hook until a second calculus needs one).

`doc/structural-congruence.md` turns `examples/ProcCongruence.v` (PRs #26,
#29) into a recipe: split off the core semantics, choose an invariant and a
*computable* canonical representative, normalise every step's target, prove
the link between the two semantics once by coinduction, derive the
transfer theorems, and let the plugin prove over the small semantics. It
explains why it works (a congruence step is silent and keeps the
invariant, so the other side answers it by standing still, which is also
when it does not apply) and the pitfall (congruence as a *premise* brings
the state space back). Includes `Test4`'s figures (9720 -> 82 states, the
two proofs' steps, time and memory) and a checklist. Linked from the README
(`MeBi Sim` section) and the example's header comment.

Also: `TODO.md`'s "Solve both directions in main bisimilarity proof" is
closed as covered by `weak_bisimilar` (agenda item 5), and the
codebase-wide comment and function-size pass Jonah asked for is logged
there as the next piece of work.

**How to revert:** `git revert -m 1 <merge-commit>` (find it with `git log
--merges --oneline --grep docs/congruence-recipe main`).

**Session tally (2026-10-04), cont.:** New feature 1 (with its `Premise
Range` setting) · Refactor 1 (review) · Docs 1.

## 2026-10-04 — bench/: the proof-suite harness, tracked

**Tooling.** On branch `tooling/bench`. Item 3 of the agenda, agreed as
tooling, not a plugin command.

The measurements of the last sessions ran on throwaway scripts in the local
`notes/tools/` (hardcoded scratch paths, and a patch to `api.ml` to force
the solver strategy that had to be reverted by hand each time). `bench/` now
holds tracked versions:
- `proofs.sh` runs the `default` suite (the six `### Success` files, CCS's
  `PluginProofs.v` and `LawProofs.v`), `abp` or `test4`, under any of
  `auto`/`True`/`False`, one memory-capped process per file, against the
  dune build, from a fresh copy of `examples/`; writes `solves.tsv` (one
  row per `MeBi Sim Solve`) and `runs.tsv` (exit, seconds, peak MB per
  file).
- **Forcing without patching the plugin**: the harness drops the copy's own
  `MutualCofix` lines and sets the mode after `Require Import MEBI.loader`
  (only a bare `MeBi Config Reset` could undo it, and no proof file has
  one). Checked: all 24 file/mode results identical to the patch-based run.
- `compare.sh` diffs two runs (exit 1 on any changed or missing Solve;
  checked on a doctored copy); `testv-counts.sh` lists `Test.v`'s counts
  (reproduces the 56 from earlier today).

`CLAUDE.md`'s verification section points to it; the manual `make` route is
unchanged. Mistake on the way: the first version handed a shell function
to `systemd-run`, so every file "ran" in 0s with exit 127; caught by the
first run's table.

**How to revert:** `git revert -m 1 <merge-commit>` (find it with `git log
--merges --oneline --grep tooling/bench main`).

**Session tally (2026-10-04), cont.:** New feature 1 (with its `Premise
Range` setting) · Refactor 1 (review) · Docs 1 · Tooling 1.

## 2026-10-04 — `MeBi Run Bisim ... As <name>` opens the proof

**New feature.** On branch `feature/bisim-opens-proof`. Item 4 of the
agenda, as Jonah chose it ("design B"): no cache. The alternative, keeping
`Run Bisim`'s result for a later `Sim Begin`, would have held whole FSMs in
memory between commands (`Test4`: everything, growing during the proof
under on-demand saturation) and gone stale when the user steps back and
redefines a relation; rejected.

**What.** `MeBi Run Bisim x With a And y With b As <name> [Using rs]`
states `weak_bisimilar a b x y` as the `Example <name>`, opens its proof
and runs `Sim Begin` on it (`Proof_solver.start`: interpret the statement,
`Declare.Proof.start`, then the existing `init`). The check is done once,
inside the command, and nothing outlives it. If the two are not bisimilar,
`init` refuses (`Not_Bisimilar`) and no proof is opened. `Sim Begin` is
unchanged. `As` comes before `Using`, because grammar words are not
reserved: a `reference_list` read `As <name>` as more relations (found on
the first try).

**Review (Jonah).** The first version was `As Bisim <name>` / `As Sim
<name>`, the latter stating `weak_sim`. Jonah asked why `Bisim` appears
twice. It did carry a choice, but a misplaced one: `Run Bisim ... As Sim`
succeeded on a pair that is *not* bisimilar (only similar), a similarity
check under a command named Bisim. Now `Run Bisim ... As <name>` always
states `weak_bisimilar`, and `weak_sim` gets its own `MeBi Run Sim` (next
branch, with the similarity computation restricted to reachable pairs:
`Product.simulation` starts from all |A|x|B| pairs, ~6GB a copy on
`Test4`, which `Sim Begin` already risks on a large non-bisimilar
`weak_sim` goal).

**Tests.** `Test.v` `RunBisimAs`: a bisimilar pair, with and without
`Using` (43 each, the same as `Example ... MeBi Sim Begin`); a
similar-but-not-bisimilar pair refused (checked: `Not_Bisimilar`, and no
`rp_bis` declared). Other `Test.v` counts identical; proof matrix under
`Auto` identical (the change only adds a function); `tests.exe` 99/99.
Mistake on the way: I inserted the new code between `guard`'s doc comment
and `guard`, which `dune build` accepts and `make` rejects (warning 50),
exactly the trap `CLAUDE.md` describes; `make` caught it before commit.
Docs: README (`MeBi Sim` section), `MeBi Help Run`.

**How to revert:** `git revert -m 1 <merge-commit>` (find it with `git log
--merges --oneline --grep feature/bisim-opens-proof main`).

**Session tally (2026-10-04), cont.:** New feature 2 · Refactor 1 (review)
· Docs 1 · Tooling 1.

## 2026-10-04 — One game step, ~10x cheaper on large on-demand FSMs

**Optimization.** On branch `perf/respond-scan`. Jonah held PR #35 until a
slow case found while measuring it was explored (notes/15). The
exploration first corrected my own misreading (logged on the PR #35
branch): the slow case is not a bisimilarity check, it is one *game step*,
`Product.answer` -> `respond`, at ~165ms per visible answer on `Test4`-sized
FSMs saturated on demand. Pre-existing on `main`; paid by `Auto`'s estimate
(with `Bounds Game` set) and by every proof-solver step on such FSMs.

**Measured split** (250 calls, ~8,150 weak actions each): building the
ordered set of (action, destinations) pairs (`to_actionpairs`) 85%,
`filter_map` 10%, the rest small.

**What.**
- `respond` now finds its answer in one pass over the state's actions
  (`best_response`): shortest annotation, ties to the least action by
  `Action.compare`. That is exactly the old choice: the fold kept the first
  of the shortest in ascending `ActionPair.compare` order, and distinct
  table keys never compare equal. A differential unit test keeps the old
  pipeline verbatim and compares: 77,652 cases over 150 generated saturated
  FSMs, 1,680 of them ties; with the tie-break reversed it fails on exactly
  those 1,680.
- `estimate_by` memoises its step function (`memo_step`): it stepped every
  pair three or more times.

**Effect.** The case (`Sim Begin`, `weak_sim compLTS compLTS (a1|a2|b1) p`,
`Bounds Game 20000000`): `main` did not finish in 20 minutes; one-pass
`respond` 121s; with the memo 55s, ~20s of it extraction and result dumps.
Same estimate (2628 pairs, 15768 moves), same strategy chosen. Elsewhere,
no change: the proof matrix is identical in all three modes (times within
noise, memory unchanged), ABP 6494/9914 and `Test4` normalised
48,821/61,161 as before, at the same times (those FSMs are not on demand,
where `respond` was cheap). `Test.v` counts identical; `tests.exe`
101/101; `make` clean.

**How to revert:** `git revert -m 1 <merge-commit>` (find it with `git log
--merges --oneline --grep perf/respond-scan main`).

**Session tally (2026-10-04), cont.:** New feature 2 (+1 open, PR #35) ·
Refactor 1 (review) · Docs 1 · Tooling 1 · Optimization 1.

## 2026-10-04 — Result dumps off by default

**Bug fix** (a default that should not have shipped). On branch
`fix/dumps-off-by-default`. Found while accounting for the probe case of
notes/15: `DumpResults` was on by default (since `e53a386`, March, "wip:
file dumps for inspecting"), so every `Run Bisim` / `Sim Begin` wrote JSON
dumps of its FSMs into `./_dumps/`. Jonah: it was only ever meant to be on
during active development, and disk writes are opt-in.

**What.** `dump_results` defaults to `false`; `MeBi Config Output
"DumpResults" True` turns it on. `Api.output_config_default` is now a
function returning a fresh record: the fields are mutable and the setters
update the current record in place, so they used to mutate the default
itself. Documented in `MeBi Help Config Output`, the README, and in
`CLAUDE.md`, which tells future sessions they may turn dumps on for
debugging (Jonah's request, as `_dumps/` will no longer be refreshed by
every run; he deleted the old one, 2.9GB).

**Effect.** The probe case (`Sim Begin`, `Test4`'s `p`, on demand): 55s ->
**35s**, peak memory 1.84GB -> **0.84GB**, nothing written to disk. Every
checked-in example already set `DumpResults` explicitly (`False`, or
`True` in the two CADP `Size2` term tests that inspect a dump), so none
changes. `Test.v` counts identical; `tests.exe` 101/101; `make` clean.

**How to revert:** `git revert -m 1 <merge-commit>` (find it with `git log
--merges --oneline --grep fix/dumps-off-by-default main`).

**Session tally (2026-10-04), cont.:** New feature 2 (+1 open, PR #35) ·
Refactor 1 (review) · Docs 1 · Tooling 1 · Optimization 1 · Bug fix 1.

## 2026-10-04 — `MeBi Run Sim`, and similarity over reachable pairs only

**New feature.** On branch `feature/run-sim`. Agenda item 6 (deferred
this morning), taken up after Jonah's review of PR #34: `Run Bisim ... As
Sim` ran a similarity check under the name Bisim, so `weak_sim` gets its
own command. Agreed scope: the verdict command, `As <name>`, and the
scaling fix underneath.

**What.**
- `MeBi Run Sim x With a And y With b [Using rs]` reports whether `x` is
  weakly simulated by `y` (a preorder: the order matters). Bisimilar states
  are similar outright; otherwise the similarity below decides. Not similar
  is an error under `FailIf NotBisimilar` (the default), else a warning.
- `MeBi Run Sim ... As <name> [Using rs]` states `weak_sim` and opens its
  proof, as `Run Bisim ... As` does for `weak_bisimilar` (same
  `Proof_solver.start`).
- **The scaling fix, which also changes `Sim Begin`.**
  `Product.simulation` started from all |A| x |B| pairs (`Test4`: ~94M,
  ~6GB a copy), and `Sim Begin` ran it unguarded for any non-bisimilar
  `weak_sim` goal. It now takes the two start states and computes the
  greatest weak simulation among the pairs *reachable* from them in the
  simulation game. That is exactly the greatest simulation's pairs among
  them, so the verdict is unchanged, and the proof search's choices are
  unchanged too (`Product.answer` only consults a state's simulators among
  a pair's own answers, all reachable). Split into documented helpers
  (`weak_answers`, `simulation_game`, `refine_simulation`).
  `Wrapper.similarity` applies the on-demand rule of PRs #27/#30: when an
  FSM is saturated on demand the walk needs `MeBi Config Bounds Game` and
  stays within it, else a user error saying how to allow it. Both `Run
  Sim` and `Sim Begin` go through it.
- `do_check_bisim` is split into `bisimilarity_of` (the shared pipeline)
  and its verdict check; `do_check_sim` reuses the pipeline.

**Measured.** The existing similarity uses (four in `CCS/PluginProofs.v`,
`Test.v`'s `SimilarNotBisimilar`) are tiny: at most 12 pairs in all, at
most 4 kept. A medium case (`weak_sim compLTS compLTS (a1|a2)
(a1|a2|b1)`, `Test4`'s components): 5832 pairs in all, 3888 reachable and
kept, 0.31s, out of `Sim Begin`'s 18.6s. So for these shapes the saving is
a constant factor (the right side's silent closure reaches every
arrangement of its components), not a change in kind; what protects the
large case is the cap. On a large similar-but-not-bisimilar pair (one `A`
then stop, against `Test4`'s `p`, on demand), `Run Sim`'s walk took 0.1s
for 3841 reachable pairs, 26s in all (extraction and dumps).

**Correction (same day).** I first reported a large case, `Sim Begin` on
`(a1|a2|b1)` against `Test4`'s `p`, as "still deciding bisimilarity of a
non-bisimilar pair after 20 minutes" and logged that in `TODO.md`. Wrong on
both counts: the pair is bisimilar (in `Proc` a send and a receive on one
channel carry the same label), and the check took 0.6s. I had inferred the
phase from the last line printed, which is printed *before* the check. A
timestamped trace (Jonah asked for a preliminary exploration before
merging this PR) put the time in `Auto`'s estimate walk: `Product.answer`
-> `respond` takes ~165ms per visible answer on such FSMs (2628 pairs in
368s). Pre-existing on `main`, a performance cost, not in this PR's code;
`TODO.md` now says so, and the follow-up is planned.

**Tests.** `tests.exe` 101/101 (new: the walk is restricted to reachable
pairs; a capped walk raises past the cap). `Test.v` `RunSim`: similar,
bisimilar-hence-similar and not-similar verdicts (error, and warning with
`FailIf NotBisimilar False`); `As <name>` proved (9, the same as `Sim
Begin`) and refused for a non-similar pair (nothing declared); on demand,
refused without `Bounds Game`, allowed within it (`Sim Begin` proves, 13),
refused past it. Every `Fail` checked for its reason. Other `Test.v`
counts identical (`SimilarNotBisimilar` included); proof matrix in all
three modes identical to the previous run (`bench/compare.sh`); `make`
clean. Mistake on the way: my first on-demand test used an LTS with no
silent steps, which is never saturated, so the "refused" check passed
the walk instead; caught by the `Fail` not failing. Docs: README, `MeBi
Help Run`; `TODO.md`'s similarity item closed.

**How to revert:** `git revert -m 1 <merge-commit>` (find it with `git log
--merges --oneline --grep feature/run-sim main`).

**Rebased** onto `main` after the `respond` speed-up (PR #36) and the
dumps fix (PR #37), which merged first while this PR was on hold, and
re-verified there (see the PR).

**Session tally (2026-10-04), cont.:** New feature 3 · Refactor 1 (review)
· Docs 1 · Tooling 1 · Optimization 1 · Bug fix 1.

## 2026-10-04 — Documentation pass, part 1: `lib/model/algorithms`

**Docs + Refactor.** On branch `docs/model-algorithms`. The first library
of the codebase-wide pass Jonah asked for after reviewing
`Premise_search.bounded_universal`: every function gets a comment at its
definition saying what it does with its arguments, and large or convoluted
functions are split into single-purpose ones. His choices for the whole
pass: a function documented in its `.mli` keeps the contract there and
gets a short "how" or a pointer at the definition (no duplicated
contracts); per library, one comments-only commit, then one commit per
split, each verified.

Before: of 131 functions in the library, 106 had no comment directly above
them (82% across the whole codebase: 1,021 of 1,240).

**Comments** (`f091a80`, `a7c406a`; with comments stripped the code is
identical, checked file by file). Also corrected stale text: two comments
that my `respond` speed-up (PR #36) had separated from `respond` and
`respond_silently`; `Policy` described as "measurement only" though the
solver answers from plans; `respond`'s tie-break and `estimate_by`'s step
cost out of date; `Saturation` pointing at "the enumeration above", long
removed.

**Splits**, one commit each, the code inside each step moved unchanged:
- `Saturation.edge_bfs` -> `visible_sources`, `silent_bfs`,
  `emit_weak_actions` (`satdiff` byte-identical; matrix identical);
- `Saturation_estimate.quotient` and `partition` -> `number_states`,
  `split_edges`, `scc_dag`, `tau_reach`, `scc_moves`, `refine_blocks`,
  `expand_blocks` (tests: quotient partition = saturated, estimate =
  saturated count, on 300 random LTSs);
- `Product.Policy.candidates` -> `stay_candidate`,
  `silent_move_candidates`, `visible_move_candidates`;
- `Product.Policy.walk`: a 7-tuple fold accumulator -> a record,
  `walk_state`, and `answer_pair`;
- `Product.Policy.minimal_relation`: ~240 lines of closures over a dozen
  tables -> a record, `shrink_state`, and eleven documented functions
  (test: same relation as the original algorithm on 400 random games).

**Verification.** Each commit: build, `tests.exe` 103/103, `Test.v`
counts identical. The two saturation splits: the proof matrix in all three
modes, each in its own worktree. The final commit: the matrix in all three
modes, and the `Test4` normalised suite (its `weak_bisimilar` runs under
`Answers Minimal`, which exercises the three `Policy` splits). `make`
clean. Mistake on the way: I queued one verification with a mistyped
commit hash; it failed at checkout and was re-run.

**Review (Jonah), same branch.** The comment style was revised: "[f x] is
..." saying what the value is (a unit function: what it does), an overview
first for orchestrating functions, the main clause before the qualifiers,
and every doc comment ending with what it raises, directly or propagated
("Raises nothing" otherwise). Restyled throughout (`bf4ef1a`, comments
only). And a split rule: a non-trivial lambda passed to fold/map/iter
becomes a named, documented function, unless it leans on the enclosing
scope, in which case values are passed as directly as possible. Applied in
four more commits: `saturation` (`visible_steps`, `offer_source`; a
`search` record with `seed`/`settle`/`relax`; `weak_action`),
`saturation_estimate` (`transitions`), `minimization` (`goes_with`,
`silent_successors`), `product` (`moves_of_action`, `obligation_of`,
`answer_obligation`, `index_obligation`). Each: `tests.exe` 103/103 and
`Test.v` identical (the saturation one also `satdiff` byte-identical); the
proof matrix in three modes on the saturation and minimization commits and
on the last, plus the `Test4` suite on the last.

**How to revert:** `git revert -m 1 <merge-commit>` (find it with `git log
--merges --oneline --grep docs/model-algorithms main`); or a single split,
by its commit.

**Session tally (2026-10-04), cont.:** New feature 3 · Refactor 10 (1
review, 9 split commits) · Docs 3 · Tooling 1 · Optimization 1 · Bug fix 1.

---

## Outstanding

- ~~Sharing the encoding table between command-time and proof-time (part of `99b0501`) should be backed out.~~ Done in `328a26f`, 2026-08-18.
- The term-equality problem in `ReModel` is unaddressed: goal terms are resolved to model elements by syntactic hashtable lookup, which can miss on evars, universe instances or local context. **Measured 2026-09-28 (see above) and found latent** — zero misses across all five cheap `PluginProofs.v` suites — so the unification rewrite is deliberately not done. Still unmeasured on the *failing* examples (`Proc/Test3`, `CADP/Size2/Glued`), which is where a miss would actually explain something.
- ~~Collapsing the model component cluster (71 of `model.mli`'s 80 sharing constraints; `Saturation.Make` at 13 arguments) is deliberately deferred until after any hand refactoring of individual model components.~~ Done in `16bbe37`, 2026-09-27, together with a nested-submodule rename and a Showable/JSON-dump unification — see below.
- ~~`examples/Bisimilarity/CADP/Size1/Glued/MutualExclusion/PluginProofs.v` fails with "The reference compose was not found", raised in the `Example` statement before any `MeBi` command runs.~~ Fixed, 2026-09-27 (see above) — root cause was a rename this file missed, not a Rocq 9.2 regression.
- ~~The "Verification baseline" table below (`268`/`396` for `CADP/Size1/MutualExclusion` and `CADP/Size1/Glued`) doesn't match the bounds checked into those files (`267`/`395`).~~ Resolved, 2026-09-27 (see above): `Proof_solver.solve` permits one step beyond its nominal bound, so this is expected behaviour, not a discrepancy.
- ~~`_CoqProject:53` comments out `examples/Bisimilarity/Proc/Test4/PluginProofs.v` by name, but the file doesn't exist on disk. Separately, `Proc/Test3/PluginProofs.v` has two duplicate example names.~~ Both addressed 2026-09-27 (see above): the Test3 duplicates are renamed (not build-verified — see the caveat there), and the `_CoqProject` comment for Test4 now says plainly that the file was never written, rather than implying it exists. Writing an actual `Proc/Test4/PluginProofs.v` remains undone.
- `lib/showable/` and `lib/json/` were never added to `_CoqProject` when introduced (2026-09-26), so only `dune build` ever compiled them — `make` silently skipped both libraries entirely. Fixed in `e037c18`, 2026-09-27, as a side effect of `lib/model/components.ml` becoming their first real consumer; see below for what that uncovered.
- ~~`@fmt` drift outside `lib/model`~~ — fully resolved, 2026-09-27 (see below, two entries): non-`proof_solver*` half (`rocq_monad.mli`, `thing.ml`, `tests.ml`) and `proof_solver*` half (`graph_extract_lts.ml`, `proof_solver_wrapper.ml`, `proof_solver_step.ml`, baseline-reverified). `rocq_monad_utils.ml`/`theories.ml`/`proof_solver.ml`/`graph_type.ml`, all listed as drifted in the original review, turned out already clean on re-check.
- ~~No CI job — the Rocq 9.2 port broke the build for months without anyone noticing.~~ Added, 2026-09-27 (see below): `.github/workflows/ci.yml`.
- ~~`.gitignore` lists `src/commandOLDunify.ml`, which no longer exists.~~ Removed, 2026-09-27 (see below). The rest of `TODO.md`'s C6 "stale detritus" item turned out to already be resolved or not actually a problem — see below for what was checked.
- ~~`Saturation.edge_action_destinations` silently dropped all but the last-visited destination when a single action had more than one — a real correctness bug (found 2026-09-27 during the A2 investigation).~~ Fixed, 2026-09-27 (see below), with a regression test. `notes/2-unify-instead-of-lookup.md`'s A2 (multiple-actionpairs positive test case) remained separately open; ~~it~~ done 2026-10-01 (`theories/Test.v`, `MultipleDerivations`).
- **Open as of 2026-10-02** (the chronological entries above have the detail): an LTS premise whose source nothing determines is explored from an unknown term and finds only some of its steps (warned; known-wrong test `OutputPremises.open_c`); Step 0, the inversion tie-break (closed in the third 2026-10-02 session: options A and D′ built, residual sterile steps measured at zero); saturation is still cubic in witnesses on `Test4`'s shape (going linear changes which equal-length witnesses survive); `Proc/Test4` remains a documented limit (saturation refused at 74.6M weak actions; a proof would need ≥ ~700k solver iterations); C4/C5/C8 and the CADP no-starvation property are for the upstream authors. Found in the second 2026-10-02 session and fixed on branch `fix/weak-bisim-silent-closure`: the solver could not answer a silent step by moving silently, and the bisimilarity verdict was neither rooted nor split by `=ε⇒`. Open from it: `weak_bisim` in `theories/` is mutual similarity, not bisimilarity, and `MeBi Sim Begin` refuses similar-but-not-bisimilar `weak_sim` goals.

- **Open as of the end of 2026-10-03** (superseding the bullet above; the chronological entries have the detail): the 2026-10-02 items are all closed -- open-source premises (PR #18), `Proc/Test4` (decided on demand, PR #22; `weak_sim` and `weak_bisimilar` proved via explicit structural congruence, PRs #26, #29), and saturation's cost (PR #28). The plugin's known limits are pinned in `Test.v` (`KNOWN WRONG` / `KNOWN LIMIT`), e.g. undeterminable transitions (warned, refused). What remains is new capability, to be agreed before any code (bounded universals in premises, goals with an unknown state, wider benchmarking, auto-starting `Sim`, a similarity command, collapsing self-referential definitions, structural congruence as a general recipe), and @dcastrop's decisions (C4/C5/C8, `Auto` defaults, the CADP no-starvation property).

Working notes live in `notes/` (local only, excluded via `.git/info/exclude`, so
not present in a fresh clone). Note 1 is done; its analysis was incomplete on two
points, both recorded in the 2026-08-18 entry above.

## Verification baseline

**Historical, kept for the record. The current baseline is in `CLAUDE.md`**:
27 counts under `Auto`, with per-mode figures in the 2026-10-02 entries.
This table predates B2 (2026-09-29): its `Proc/Test2` row is now the
*forced-nested* figure, and `Proc/Test3` was not yet passing. Do not compare
a run against it.

Proof-solver iteration counts from the five `PluginProofs.v` marked `### Success`
in `_CoqProject`, unchanged from `main` through `e037c18`, and complete as of
`CADP/Size1/Glued/MutualExclusion`'s fix on 2026-09-27. Recorded per file, in
emission order, because a sorted aggregate cannot tell two files apart:

| file | counts |
| --- | --- |
| `Proc/Test1` | 114 105 106 109 22 21 |
| `Proc/Test2` | 446 278 299 194 446 182 |
| `CADP/Size1/MutualExclusion` | 268 396 |
| `CADP/Size1/Glued` | 268 396 |
| `CADP/Size1/Glued/MutualExclusion` | 81 63 |

All 18 `Solve` commands in the sources now reached and accounted for. To
reproduce, build each file as its own `make -j1` target — `make -j$(nproc)`
interleaves the concurrent `rocq` processes line by line and the counts
cannot be reliably attributed to one file this way (confirmed the hard way
on 2026-09-27: a `-j$(nproc)` run's interleaved "Solved after 268/396
iterations" lines were initially, and wrongly, attributed to
`Glued/MutualExclusion` before a `-j1` rebuild showed those actually belong
to its two siblings). Note also that `MeBi Sim Solve N` permits up to
`N + 1` solver steps before giving up (see `src/proof_solver.ml`'s `solve`),
so a checked-in bound one below its file's baseline count (as with
`MutualExclusion`/`Glued` above, `Solve 267`/`Solve 395`) is expected, not
a bug. `make` enforces warnings (32, 50) that `dune build` accepts, and
caught three failures during the 2026-08-17 session that `dune build`
waved through — always finish with a `make` run, not just `dune build`.
