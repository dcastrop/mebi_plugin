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
after the fact. To date, none has.

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
- ~~`Saturation.edge_action_destinations` silently dropped all but the last-visited destination when a single action had more than one — a real correctness bug (found 2026-09-27 during the A2 investigation).~~ Fixed, 2026-09-27 (see below), with a regression test. `notes/2-unify-instead-of-lookup.md`'s A2 (multiple-actionpairs positive test case) remains separately open.

Working notes live in `notes/` (local only, excluded via `.git/info/exclude`, so
not present in a fresh clone). Note 1 is done; its analysis was incomplete on two
points, both recorded in the 2026-08-18 entry above.

## Verification baseline

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
