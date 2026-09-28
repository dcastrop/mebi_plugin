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

## Outstanding

- ~~Sharing the encoding table between command-time and proof-time (part of `99b0501`) should be backed out.~~ Done in `328a26f`, 2026-08-18.
- The term-equality problem in `ReModel` is unaddressed: goal terms are resolved to model elements by syntactic hashtable lookup, which can miss on evars, universe instances or local context.
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
