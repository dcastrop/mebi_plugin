# Decisions for @dcastrop

The repository is yours, so these choices are yours too. Each one below
gives the question, the options with what they commit us to, and a
recommendation, marked as such. Whatever you choose, every merged change
can be backed out: `ASSISTED-CHANGES.md` gives each one's revert command.

## Since you last looked

Your last commit is `d204afb` (2026-01-12, the port to Rocq 9.0.1). Since
then, 632 commits have been made, and 231 of them are already on your
`main`: 181 reached it before 2026-08-17 (the Rocq 9.2 port among them),
and 50 (2026-08-17 to 2026-09-29) were pushed to it by mistake on
2026-09-29, because an earlier session had set git up so that a push went
to your repository instead of the fork. Jonah moved on rather than undo
it. The other 401 landed on Jonah's fork, `thecathe/mebi_plugin`, as PRs
#1–#73, and are not upstream yet. The core still does what it did:
- it reads an inductive `step : term -> label -> term -> Prop`;
- it builds the LTS and decides (weak) bisimilarity;
- it proves the result with `MeBi Sim Begin`/`Solve`.

What changed is around it:

| where | what it holds |
|---|---|
| `src/` | the plugin. `proof_solver*.ml` is the proof search. `api.ml` holds the commands and settings. |
| `lib/model/` | FSMs, saturation and bisimilarity, in pure OCaml, tested by `dune exec test/tests.exe` |
| `lib/{utils,terms,showable,rocq_tools}/` | supporting libraries, split out of `src/` |
| `theories/Bisimilarity.v` | `weak_sim`, `mutual_sim`, and `weak_bisimilar`, which is new: one relation in both directions |
| `theories/{Transfer,Hiding,Traces}.v` | new: carrying results between semantics, hiding labels, and what a relation preserves |
| `theories/Test.v` | the tests, including deliberate "known limit" pins |
| `examples/` | `Bisimilarity/{Proc,CCS}`, `CADP/` (the MCS lock, the running example) and `Evaluation/`. See `examples/README.md`. |
| `bench/proofs.sh` | runs the proof suites under a memory cap and tabulates step counts |
| `doc/` | this file, `limitations.md`, `future-work.md`, `decide-vs-prove.md`, `structural-congruence.md` |
| `ASSISTED-CHANGES.md` | the history of every change, its kind, and how to revert it |
| `OUTSTANDING.md` | the index of everything still open, this file included |

The decisions below are ordered by what blocks the upstream PR first.

## 1. How should the work reach your repository?

**Question.** All 73 PRs are merged on the fork: 401 commits, 251 files,
+29,310/−3,725 against your `main` (2026-10-15). The plan is one PR to
`dcastrop/mebi_plugin` after you decide 2 and 3, because each of those
changes what every checked-in proof bound means.

**Options.**
1. **One PR of the whole fork (recommended).** You review the result
   with `ASSISTED-CHANGES.md` as the guide, and back out anything you
   reject by its revert command.
2. **A series of smaller PRs,** by area (`lib/`, the solver, `theories/`,
   `examples/`). This is easier to read, but the areas depend on each
   other, so the order has to be worked out and each step rebuilt.

## 2. Should `MeBi Config Solver MutualCofix Auto` stay the default?

**Question.** When `MeBi Sim` proves a goal it must introduce coinduction
hypotheses. Your solver does this with one nested `cofix` per new pair.
The fork adds a second way, one mutual `cofix` over every pair, and an
`Auto` setting that estimates both before the proof and picks the
cheaper (`Model.Product.estimate`). `Auto` is the default today.

**Options.**
1. **Keep `Auto` (recommended).** On the 18 cheap proofs it takes 2,565
   steps in total, against 3,794 always-nested and 2,654 always-mutual,
   and it is worse than nested on none of them. Some `weak_bisimilar`
   proofs finish only mutually: `examples/Bisimilarity/CCS/LawProofs.v`'s
   `paraA` takes 3,192 steps, and nested is killed at 5GB. The cost is
   that the choice is automatic, and it prints a notice when it goes
   mutual.
2. **Default to nested,** your original behaviour, with the others
   available by setting. Old proofs behave exactly as before, but
   `weak_bisimilar` goals then fail unless the user opts in.
3. **Default to mutual.** Close to `Auto` on these examples, with no
   estimate to compute.

**What it commits us to.** Every checked-in `MeBi Sim Solve N` bound was
measured under `Auto`. Option 2 or 3 means re-measuring them all, which
is best done before the upstream PR. If you keep `Auto`, two small
follow-ups remain:
- `Proc/Test2`'s bounds are deliberately loose, so that the file still
  compiles nested; they can be tightened.
- `Proc/Test3`'s explicit `MutualCofix True` can be dropped.

Detail: `ASSISTED-CHANGES.md`, 2026-09-29.

## 3. Should `MeBi Config Solver Answers Auto` become the default?

**Question.** The solver must choose an answer to each of the other
side's moves. `Default`, the default today, chooses move by move. `Auto`
plans every answer at `Sim Begin` and keeps the cheapest of three
policies (`Default`, `Greedy`, `Minimal`).

**Options.**
1. **Switch to `Auto` (recommended).** On all 41 checked-in proofs it was
   never slower than `Default`. It cut `weak_sim` proofs by 17% and
   `weak_bisimilar` proofs by 60% (`Proc/Test3`'s `wbis_p3`: 14,427 to
   3,579 steps).
2. **Keep `Default`.** Nothing changes. `Auto` stays one setting away.

**What it commits us to.** Switching changes every bound, as in 2. It
was not measured with `MutualCofix` forced either way, so if you choose
nested in 2, it needs measuring again. Detail: `ASSISTED-CHANGES.md`,
2026-10-02 (PR #9).

## 4. Which licence?

**Question.** There is no `LICENSE` file, and `dune-project`'s
`(license ...)` line is commented out, so the opam package has no
licence either. Nobody may legally reuse the code, and a release needs
one.

**Options.** Your choice of licence. MIT and BSD-3 are common for Rocq
plugins. LGPL-2.1 matches Rocq itself. Adding it means one file and
uncommenting one line.

## 5. What should happen to `paper/`?

**Question.** `paper/` holds 74 tracked files, mostly third-party PDFs:
~55MB, or 96% of the repository's tracked bytes. It was last changed in
February 2025.

**Options.**
1. **Delete it from `HEAD` (recommended),** and keep the references as a
   bibliography. New clones still download the history, so the
   repository only shrinks with option 3.
2. **Keep it.** Redistributing third-party papers from a public
   repository raises a copyright question.
3. **Rewrite history** (`git filter-repo`) to remove it everywhere. The
   repository then shrinks for real, but every clone must be re-cloned
   after a coordinated force-push.

Small, whichever you choose: `paper/references/to check/Affeldt.pdf` has
a ligature (U+FB00) in its name and a space in its directory, both
portability hazards.

## 6. Do you want proofs split into separately checked lemmas?

**Question.** Some systems are decided but cannot be proved. CADP's
two-process glued ≈ layered is decided in 45s, but every proof ran out of
memory at 10GB. A proof today is one `cofix`, which cannot be split. A
new back end could prove bisimilarity through an explicit relation
instead, one lemma per block of pairs, so that peak memory is about one
block. It is a new capability, and large. The design is in
`doc/future-work.md`, "Proofs split into separately checked lemmas".

**Options.**
1. **Leave it as future work** (Jonah's current choice). The paper
   reports these sizes as decided, not proved.
2. **Build it.** It reaches proofs the plugin cannot produce today. It
   is the largest item on the list.

## 7. For review: choices made while you were away

These were Jonah's calls, made as the work went. Each can be reversed.

- **The target notion is classical, divergence-insensitive weak
  bisimilarity.** A τ-loop and a stuck state are therefore equivalent.
  Since PR #1, the checker decides it correctly (rooted verdict, `=ε⇒`
  split). Before, `Run Bisim` could call `a.b.x` and `b.a.y` bisimilar.
- **`weak_bisim` is gone.** It was two `weak_sim`s, which is mutual
  similarity, strictly weaker than bisimilarity: `a.b + a` and `a.b`
  satisfy it but are not bisimilar (proved in `theories/Test.v`). Its
  honest name, `mutual_sim`, replaces it. `weak_bisimilar` is the real
  bisimulation, and it is what `MeBi Sim` proves. The theorems in
  `examples/Bisimilarity/Proc/*/ManualProofs.v` and `LtacProofs.v` were
  restated as `mutual_sim` (2026-10-13).
- **`MeBi Sim Begin` accepts `weak_sim` goals between states that are
  similar but not bisimilar,** and proves `weak_bisimilar` goals.
- **Provenance.** `rocq-sims` (N. Chappe, POPL'26) overlaps in subject,
  not in method. An audit of the assisted commits found no copying. The
  13 earliest assisted commits have no session transcript.
