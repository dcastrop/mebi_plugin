# CLAUDE.md

Guidance for Claude Code sessions working on `mebi_plugin`.

## Hard rules (read before any design)

1. **No proof by computation in Rocq.** A proof is Sylvan's certificate:
   the log of what Sylvan computed while the engine decided, which the
   plugin parses and loads **as a proof term** (one lemma application per
   operation), for the kernel to typecheck. Never add `vm_compute`,
   `native_compute`, a VM or native cast, or a reflective checker (a
   Gallina function over certificate data that returns a boolean, proved
   by `eq_refl`). Before writing any Rocq-side proof step, write down in
   the plan what Rocq computes; anything beyond typechecking the engine's
   term is refused: put it in the engine, or present evidence and wait
   for an explicit OK. `python3 scripts/check_no_computation.py` (first in
   CI) fails on any use; there is no allowlist; never loosen it. The
   project's PreToolUse hook (`.claude/settings.json`, local:
   `scripts/hook_no_computation.py`) refuses an edit that writes one.
   **Optimisations are added on the certificate, never dropped:**
   confluence reduction, static confluence, movers, compositional proofs,
   proofs up to structural congruence (`doc/optimisations.md`).
2. **A route the user chose is binding.** If it looks wrong, challenge it
   with evidence (measurements, a counterexample, `file:line`), then wait.
   Without an explicit OK, follow it: no substitute route, not even
   "temporarily" or "as a stepping stone".
3. **Every file the plugin writes goes next to the file being checked:**
   for `File.v`, the directory `.File.v.mebi/` beside it (`files_dir` in
   `src/user_tactics.ml`): the proof log per lemma (`<Lemma>.log`, with
   `Set MeBi Log Mode "keep"`; otherwise it stays in memory), and a
   log's facts while they are checked, deleted once read. Never
   `~/.cache`, never `/tmp` (a tmpfs here: a file there is memory). Work
   files of a session stay in the repo too (`_certify/`, as `bench/`
   does), never in the scratchpad under `/tmp`.
4. **Run nothing that does not end in a `Qed` closed under the global
   context** (2026-11-10), beyond building what you change: no benchmark
   or test run for its own sake until the certificate proves goals.

## Working assumption

The engine decides; Sylvan's log is the certificate; the plugin
translates it into facts and builds from them the proof of the goal,
which the kernel checks at `Qed` (`doc/certificate.md`); every example,
three-process CADP included, ends with `Qed`. What remains: the
optimisations on the certificate.
If a change would add a genuine new capability to the plugin (something
it could not do before), say so explicitly *before* writing it.
Net-new code that is purely infrastructure (a pure-OCaml test binary, a
script) is fine, but label it as such.

## Keep `ASSISTED-CHANGES.md` current

After any substantive piece of work, add or extend an entry in
`ASSISTED-CHANGES.md` (repo root). Tag each change **Refactor**, **Bug fix**,
**Optimization**, **New feature**, **Tooling** or **Docs**, and keep a running
per-session tally. Be honest about mistakes there. **Name no people** in
it or in any other document: say what was decided and why, not who.

## Keep `OUTSTANDING.md` current

`OUTSTANDING.md` (repo root) is the one *index* of what is still open:
one line per item, saying where its detail is. Detail goes in a scoped
file, never in the index: `doc/certificate.md` (the certificate's next
steps), `doc/optimisations.md`, `doc/limitations.md` (what the tool
cannot do today), `doc/future-work.md` (possible work), and the technical
report's "Decisions to double-check" (`doc/report/sections/10-status.tex`).
The index should only shrink: when an item is resolved, delete its line
and record the resolution in `ASSISTED-CHANGES.md`.

## How a goal is decided and certified

The tactics `sim`/`bisim` (`strong_sim`/`strong_bisimilar`) and
`weak_sim`/`weak_bisim` (`weak_sim`/`weak_bisimilar`):
1. **decide** the goal symbolically (`lib/learn`, on Sylvan's LDDs,
   `lib/ldd`): rules from the constructors (`lib/rules`), states as
   containers (`lib/skeleton`), each rule's steps learned by an
   interpreter (a step it cannot follow is `Learn.Unfollowable`), the
   game to its greatest fixpoint;
2. **log the certificate**: the fixpoint's check made again as images
   with Sylvan's proof log on (the fork's local branch `proof-log`:
   `lddmc_log_open`, `lddmc_log_top`, `lddmc_log_close`), in memory
   (`lddmc_log_open(NULL)`, `lddmc_log_take`: no file; collection stays off
   until the facts are written, `Ldd.log_free`), or with
   `Set MeBi Log Mode "keep"` into `.<File>.v.mebi/<Lemma>.log`: each
   mover's image of `z` (complete)
   within the pairs the answerer can answer, images of `z` by its inverse
   relations (sound), and per mover summand the coverage of `z` by the
   projections it learned; each top-level image is recorded with the way
   the proof uses it (`Ldd.log_top_relprod`). A pair's vector holds the
   answering side's positions first. The engine records what it logged as
   a plan (`Learn.Plan`, `Learn.last_plan`);
3. **prove the goal from it** (`Lts_proof.qed_term`): the log's records
   become facts of `theories/DdTree.v` (`Dd_cert.write_log ~requests`,
   each line handed to `Dd_read.proof_reader`: no file); the game on
   vectors is proved from
   them (`src/game_proof.ml`, `theories/DdGame.v`); the goal's own LTSs are
   related to the vectors by generated Gallina text over named local
   definitions, elaborated (`src/lts_proof.ml`, `theories/DdEnc.v`): each
   system's encoding (a trie of shape predicates), the answerer's
   relations sound (a lemma per relation and target shape, a derivation
   per tuple), the mover's complete (a case lemma per LTS, small inversion
   by match predicates, coverage of the slots by the learned projections,
   a step lemma per skeleton), then `DdGame`'s theorem applied. Each named
   definition is a constant, declared as `abstract` declares a sub-proof
   (a side effect of the proof, exported at `Qed`): proofs opaque (the
   elaborator sees only their statements), data transparent, one over the
   facts abstracted over those it uses; the kernel checks each once. The
   tactic closes the goal with a term naming them (the facts, the game,
   the theorem); the kernel checks it at `Qed`. `MeBi Check Log
   "<file>"` (relative to the file) checks a
   kept log's facts alone, a chunk at a time, each chunk a constant
   (`Dd_read.read`).
`MEBI_CERT_STATS=1` times the phases (with memory) on stderr, the proof's
too (`[qed]`: elaborations, text, definitions); `MEBI_CERT_STOP=decided`
stops once the engine has decided; `MEBI_PLAN_DUMP=1` prints the plan;
`MEBI_QED_TEXT=<file>` appends every elaborated text to `<file>`;
`MEBI_QED_LETS=1` makes the definitions `let`s of the proof's term
instead of constants (for comparison);
`MEBI_CERT_KEEP=1` keeps a log's facts file; `MEBI_LOG_FACTS=<file>`
makes `MeBi Check Log` read a facts file instead; `MEBI_CHUNK_FACTS=<n>`
sets the facts per chunk (default 20,000: fewer chunks, more memory, the
same time); `test/cert_tool.exe --log <log> [<facts>]` translates alone,
with the facts per lemma.

To verify a change (rule 4):
0. `python3 scripts/check_no_computation.py` and
   `python3 scripts/check_module_lists.py`.
1. `dune build -j 2`, capped (about 2.5GB): the library, the plugin,
   `test/rocq/*.v` (`LogTest.v`: the four relations proved, each closed
   under the global context; a log kept and its chunk checked; "off"
   deletes the log; a refused mode; a negative verdict) and the examples,
   each goal proved from its certificate (`Qed`).
2. `dune exec test/ldd_tests.exe` (30 tests, 12 of the proof log, 5 of them
   kept in memory).
3. Heavy: `python3 bench/prooflog.py` (each step's cap: available memory
   less 4GB, at most 7GB): two-process CADP to `Qed`, the file in about
   6s, 0.83GB at most; three-process CADP to `Qed`, the proof built in
   124s, `Qed` 144s, the file in 293s, 6.6GB at most, so it needs about
   11GB available (2026-11-11).
4. For a change to `DdTree.v`: `Print Assumptions` of a chunk stays
   "Closed under the global context" (`LogTest.v`).

## Working conventions and gotchas


**Conventions**
- Changes stay uncommitted on `main` for review unless a commit is asked
  for; no branches unasked. Run `dune fmt` before committing.
- Gate a commit on the build. A `git commit` chained after a build in one
  command commits a broken tree.
- Never push unless asked. (50 commits once reached `origin/main` by
  mistake, from a session whose git setup sent the push there.) Before
  any push, read `git remote -v` and `git branch -vv`, and name the
  remote explicitly.
- A new capability is flagged and agreed before code (see the top of this
  file).

**Rocq and the plugin**
- **Rocq reuses freed hypothesis names** (after `inversion_clear` or
  `clear`). Never identify a "new" hypothesis by name; use context order,
  or name *and* type.
- **`rocq compile` hides the message a `Fail` caught.** To read them all,
  feed the file to `rocq repl -q ... < file.v` and grep "indeed failed".
- **With the native compiler installed** (`rocq-native`), every
  `rocq compile` also builds the file's native code, about a second a
  file; `bench/prooflog.py` passes `-native-compiler no`. dune builds
  native code too (detected from Rocq's configuration).
- **`dune build -j 2` can be killed by its cap** when two big compile
  steps meet; the journal says which process (`journalctl --since -5min |
  grep -i "killed process"`).
- **In `Proc`, a send and a receive on one channel carry the same label**
  (`Some B`). Adding a component often leaves systems bisimilar, so check
  the verdict before calling a pair "non-bisimilar".

**Builds**
- **Sylvan and Lace are git submodules** (`vendor/sylvan`, `vendor/lace`,
  forks; `lib/ldd/README.md`): a fresh clone needs
  `git submodule update --init`. Never edit them in place: change the fork,
  then move the submodule pointer. Sylvan's changes for the proof log
  (`doc/certificate.md`, A) are on a local branch `proof-log` in
  `vendor/sylvan`, kept rebased onto the fork's `master`; it is not
  pushed unless asked, so until then CI and fresh clones cannot fetch the
  pointer's commit.
- **Sylvan's explicit `sylvan_gc()` shrinks the node table to fit**
  (since the fork's `master`, 2026-11-10); only the collection Sylvan
  triggers when the table is full (`sylvan_gc_collect`) calls the main
  hook, which grows it. With collection off (the proof log), a table that
  fills exits the process ("MDD Unique table full"), so `Ldd.log_open`
  grows the table to its maximum first, through `sylvan_gc_collect`.
- **Write `Stdlib.Option`, not `Option`,** in plugin code: inside the
  plugin `Option` can resolve to Rocq's module. Keep each `(** ... *)` directly before what it documents.
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
- **`dune build src` does not update `_build/install`,** which is what a
  `rocq compile ... -I _build/install/default/lib` loads: build
  `@install` (and the `.vo`s it invalidates) after a plugin change, or
  the run measures the old plugin.
- **Write a timed run's output to a file,** not through `| grep`: when
  `timeout` kills the job, the pipe's buffered lines are lost.
- **The last line printed does not say where the time went.** A message
  prints *before* its phase, and the next can come much later from another
  phase. Put timestamped markers on stderr (`Printf.eprintf ... %!`)
  before blaming a phase. Per-command times: `rocq compile -time`, or
  `Time` before a command.
- `perf` is blocked on the development machine (`perf_event_paranoid` = 4):
  time from inside the plugin (`MEBI_CERT_STATS=1`, or timestamped
  markers on stderr).

**Shell**
- zsh does not word-split a command held in a variable (`$RC file` fails
  as "no such file"); put such loops in a bash script.
- `pkill -f <pattern>` can kill the calling shell when the pattern is in
  its own command line; kill by PID from `ps`.

## Other repo-specific notes

- **The technical report** (`doc/report`) has a bibliography
  (`refs.bib`, natbib): build with `pdflatex main; bibtex main; pdflatex
  main; pdflatex main`, and cite every technique or tool it mentions.
  The built `doc/report/main.pdf` is tracked: rebuild it after changing
  the sources, so the two are committed together.
- **Terms:** CADP's semantics is `lts` over `step`, one step per
  instruction. Proc's LTS over a component LTS is *stacked*
  (`Proc.Stacked`).
- `notes/` holds local planning notes and is excluded via
  `.git/info/exclude` (not `.gitignore`) — it will not appear in a fresh
  clone's `git status` as untracked, but is not shared upstream either.
- Adding, renaming or deleting an OCaml module means updating the
  relevant dune `(modules ...)` by hand: dune silently leaves out a file no
  list claims. `python3 scripts/check_module_lists.py` checks the lists
  against the source tree (CI runs it first); run it before committing
  such a change.
- In-tree build artifacts (`.vo`, `.glob`, `.aux` in `theories/`,
  `examples/`, `test/` or `src/`) can make `dune build` silently skip a
  file; `git clean -fX theories test src examples` and `dune clean`
  restore it. Never compile in the source tree: copy to `_certify/` (as
  `bench/` does).
- Run `dune build` (capped, `-j` limited) *before* `bench/prooflog.py`, or a
  failed build costs the whole run.
