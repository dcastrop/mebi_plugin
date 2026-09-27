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
`Co-Authored-By: Claude` trailer — 16 commits, all from 2026-08-16 onward. The
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

## Outstanding

- ~~Sharing the encoding table between command-time and proof-time (part of `99b0501`) should be backed out.~~ Done in `328a26f`, 2026-08-18.
- The term-equality problem in `ReModel` is unaddressed: goal terms are resolved to model elements by syntactic hashtable lookup, which can miss on evars, universe instances or local context.
- ~~Collapsing the model component cluster (71 of `model.mli`'s 80 sharing constraints; `Saturation.Make` at 13 arguments) is deliberately deferred until after any hand refactoring of individual model components.~~ Done in `16bbe37`, 2026-09-27, together with a nested-submodule rename and a Showable/JSON-dump unification — see below.
- `examples/Bisimilarity/CADP/Size1/Glued/MutualExclusion/PluginProofs.v` fails with "The reference compose was not found", raised in the `Example` statement before any `MeBi` command runs. Pre-existing and unrelated to the above; looks like a Rocq 9.2 port casualty despite being marked `### Success` in `_CoqProject`.
- `lib/showable/` and `lib/json/` were never added to `_CoqProject` when introduced (2026-09-26), so only `dune build` ever compiled them — `make` silently skipped both libraries entirely. Fixed in `e037c18`, 2026-09-27, as a side effect of `lib/model/components.ml` becoming their first real consumer; see below for what that uncovered.

Working notes live in `notes/` (local only, excluded via `.git/info/exclude`, so
not present in a fresh clone). Note 1 is done; its analysis was incomplete on two
points, both recorded in the 2026-08-18 entry above.

## Verification baseline

Proof-solver iteration counts from the five `PluginProofs.v` marked `### Success`
in `_CoqProject`, unchanged from `main` through `e037c18`. Recorded per file, in
emission order, because a sorted aggregate cannot tell two files apart:

| file | counts |
| --- | --- |
| `Proc/Test1` | 114 105 106 109 22 21 |
| `Proc/Test2` | 446 278 299 194 446 182 |
| `CADP/Size1/MutualExclusion` | 268 396 |
| `CADP/Size1/Glued` | 268 396 |
| `CADP/Size1/Glued/MutualExclusion` | *(none — fails at `Example`, see below)* |

16 counts against 18 `Solve` commands in the sources; the missing two are
`Glued/MutualExclusion`'s, never reached. To reproduce, build each file as its
own `make -j1` target — `make -j$(nproc)` interleaves the concurrent `rocq`
processes line by line and the counts cannot be attributed. Note that
`make` enforces warnings (32, 50) that `dune build` accepts, and caught three
failures during the 2026-08-17 session that `dune build` waved through — always
finish with a `make` run, not just `dune build`.
