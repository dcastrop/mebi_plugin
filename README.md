# MEBI: Mechanised Bisimilarities

This repository contains a Rocq (formerly Coq) plugin for automating bisimilarity proofs (which are currently taken from the methods detailed in "Advanced Topics in Bisimulation and Coinduction", Section 3.2.2).

**Work in progress**

---


- [MEBI: Mechanised Bisimilarities](#mebi-mechanised-bisimilarities)
  - [Try it](#try-it)
  - [Building the Project](#building-the-project)
    - [Toolchain](#toolchain)
    - [First-time setup on a new machine](#first-time-setup-on-a-new-machine)
    - [Everyday environment](#everyday-environment)
    - [Changing dependencies](#changing-dependencies)
    - [Documentation](#documentation)
    - [Building](#building)
    - [Faster inner loops](#faster-inner-loops)
    - [Running tests](#running-tests)
    - [Using VSCode](#using-vscode)
  - [Usage](#usage)
    - [Building an LTS or FSM](#building-an-lts-or-fsm)
    - [Bisimilarity, merging, minimizing, saturating](#bisimilarity-merging-minimizing-saturating)
    - [Which constructor shapes are supported](#which-constructor-shapes-are-supported)
    - [Benchmarking](#benchmarking)
    - [Proof search (`bisim` and `sim`)](#proof-search-bisim-and-sim)
    - [Configuration](#configuration)
    - [Diagnostics](#diagnostics)
    - [Help](#help)
  - [Status \& Remaining Work](#status--remaining-work)
  - [Licence](#licence)
  - [Acknowledgements](#acknowledgements)
  - [Other Resources](#other-resources)
    - [Templates](#templates)
    - [Tutorials](#tutorials)
    - [Other](#other)
    - [Papers](#papers)
    - [Books](#books)



## Try it

To use the plugin in your own Rocq developments, without a clone, pin it into
an opam switch that can install Rocq 9.2 (the default opam repository is
enough, and OCaml 4.14 or later):

```sh
opam switch create mebi ocaml-base-compiler.5.4.0   # skip if you have a suitable switch
opam pin add rocq-mebi "git+https://github.com/dcastrop/mebi_plugin#main"
```

Until the fork's work is merged upstream, use
`git+https://github.com/thecathe/mebi_plugin#main`. A branch pin follows the
branch: `opam update` fetches whatever is on `main` then, and `opam upgrade
rocq-mebi` (or `opam update --upgrade`) rebuilds it. To stay on one version,
pin a commit (`#<hash>`). It builds the plugin from source, which takes a few
minutes, and builds Rocq and OCaml too if the switch is new.

Then, in a `.v` file, a tiny system and a proof the plugin finds:

```coq
Require Import MEBI.loader.
Require Import MEBI.Bisimilarity.

Inductive st : nat -> option bool -> nat -> Prop :=
  | p0 : st 0 (Some true) 1 | p1 : st 1 (Some false) 0
  | q0 : st 10 (Some true) 11 | q1 : st 11 (Some false) 10.

Lemma e : weak_bisimilar st st 0 10.
Proof. bisim. Qed.
```

It prints `Solved after 29 iterations`. The tactic `bisim` proves a
`weak_bisimilar` goal, and `sim` a `weak_sim` goal, with no setup. They read
from the goal the relations, the terms, the label type (its `None` is the
silent step) and the other relations the constructors' premises use. Limits
are Rocq options, set once near the top of a file (`Set MeBi MaxStates
2000.`, `Set MeBi MaxSteps 50000.`, and so on; `MeBi Help Sim` lists them).
They are described under [Usage](#usage). `MeBi Help.` lists the commands, and
[Usage](#usage) below describes them. Larger worked examples, with the bounds
they need, are in [`examples/README.md`](examples/README.md). It is a
research prototype.

The licences are in [Licence](#licence).

## Building the Project

### Toolchain
Built against **Rocq 9.2** (`rocq-core` 9.2.0, `rocq-runtime` 9.2.0, `rocq-stdlib` 9.1.0), OCaml 5.4.0 and dune 3.23, in a **local opam switch** (`_opam/` in the project root).

Versions are pinned in two places, and both are committed:
- **`flake.nix` / `flake.lock`** — nix pins the system layer: `opam` itself, `pkg-config`, `gmp`, `zlib`, `git` and the C toolchain.
- **`rocq-mebi.opam.locked`** — opam pins the OCaml/Rocq layer: every one of the ~100 packages at an exact version, including `rocq-core.9.2.0`, `rocq-stdlib.9.1.0` and `ocaml-base-compiler.5.4.0`.

The source also compiles against **Rocq 9.3**. Code that differs between the two is under cppo's `#if ROCQ_VERSION >= (9, 3, 0)`, in the few files listed in two places: the `(preprocess (per_module ...))` in `lib/rocq_tools/dune` (or `src/dune`), and `.ocamlformat-ignore`. A file that gains an `#if` goes in both.

The ranges the source is *known* to compile in live in `dune-project`'s `(depends ...)`; the lockfile records one exact solution inside those ranges. `rocq-mebi.opam` is generated from `dune-project` by dune and is committed — it is the input to `opam switch create`, so a clone cannot bootstrap without it.

### First-time setup on a new machine
```shell
git clone https://github.com/dcastrop/mebi_plugin && cd mebi_plugin
direnv allow                     # or: nix develop
opam switch create . --locked --deps-only
opam install . --locked --deps-only --with-dev-setup
```
The dev shell notices when `_opam/` is missing and prints this command; run it under `nix develop` in a terminal and it offers to do it for you. Expect it to take a while — it builds OCaml 5.4.0 and Rocq from source.

`--with-dev-setup` is what pulls in `ocaml-lsp-server`, `ocamlformat` and `vsrocq-language-server`. Drop it if you only want to build.

> **Run these inside the dev shell.** It sets `OPAMNODEPEXTS=1`. Without it, opam probes the system package manager for `gmp` and `pkg-config`, doesn't find them (nix supplies them, not the system), and offers to run `nix-build` — which aborts the bootstrap. Outside the shell, pass `--no-depexts` by hand.

### Everyday environment
With `direnv`, entering the directory is enough. Without it:
```shell
nix develop
eval $(opam env --switch=$(pwd) --set-switch)
```
Check it took: `rocq --version` should report 9.2, and `which dune` should point inside `_opam/bin`.

### Changing dependencies
Edit the `(depends ...)` ranges in `dune-project`, install what changed (with the dev setup: `opam install . --deps-only --with-dev-setup --with-doc`), then regenerate both files:
```shell
scripts/relock.sh                # dune-project -> rocq-mebi.opam -> rocq-mebi.opam.locked
```
Commit both. Use the script rather than `opam lock .` alone: `opam lock` silently drops a filter's packages unless all of them are installed, and files packages shared by the doc and editor tooling under `with-doc` only; the script widens those to `with-dev-setup` and checks the lock matches the switch. Bumping `rocq-core`'s upper bound is a deliberate act — the plugin links against `rocq-runtime`'s OCaml API, which breaks across Rocq *minor* versions.

### Documentation
`dune build @doc` builds the API docs from the doc comments into `_build/default/_doc/_html/` (open `index.html`); `odoc` and `odig` come with the dev setup. Comments refer to other definitions with odoc references (`{!Module.f}`, `{{!Module.f}text}`), which become links.

### Building

The project builds with **dune** alone:
```shell
dune build
```
It builds the plugin (`src/`, `lib/`), `theories/`, `test/rocq/` (`Test.v`, `DevTest.v`) and the fast examples, the ones `examples/dune` lists (~2 min). The heavy proof files under `examples/` (the Alternating Bit Protocol, `Proc/Test4`, the large CADP and evaluation files, the `PluginProofs.v` suites) take minutes and gigabytes each, so they are not in the build: `bench/proofs.sh` runs them one at a time under a memory cap (see [Running tests](#running-tests)).

`dune build` runs as many jobs as the machine has cores. Each Rocq job can take a few hundred MB, so on a machine short of memory, limit them: `dune build -j 4`.

`_RocqProject` is for the IDE only: it maps the logical names (`MEBI`, `MEBI.Examples`, `MEBITests`) to dune's outputs in `_build/`, so build before stepping through a file. (Until 2026-11-05 there was also a `make` build, from `_CoqProject`, which wrote its files into the source tree; it is gone.)

### Faster inner loops
A full `dune build` takes ~1m40s, almost all of it the `MeBi Benchmark` vernaculars in `test/rocq/DevTest.v`, which is not part of the installed package, so `opam` does not run it. When you don't need them:

| Changing                                | Command                                                                                  |
| --------------------------------------- | ---------------------------------------------------------------------------------------- |
| OCaml plugin code                       | `dune build @check` — type-checks everything in `lib/` and `src/`, builds no `.vo` (~2s) |
| OCaml, and you want the loadable plugin | `dune build src/`                                                                        |
| The plugin and library, no tests        | `dune build @install` — what opam builds (~7s from clean)                                |
| A single test file                      | `dune build test/rocq/Test.vo`                                                           |
| Everything, before committing           | `dune build`                                                                             |

`dune build @check` also produces the `.cmi`/`.cmt` files merlin needs, so it doubles as the "make my editor happy" command.

### Running tests
`dune exec test/tests.exe` runs a small pure-OCaml test suite (no Rocq runtime) exercising `lib/model` directly — FSM construction, saturation, minimization, bisimilarity, JSON round-tripping. It's a fast first signal for changes to `lib/model`/`lib/terms`/`lib/utils`, but it doesn't exercise the proof solver (`src/proof_solver*`) or anything Rocq-facing.

`test/rocq/Test.v` runs the solver on small cases; the end-to-end coverage is the proof files under `examples/`: the `PluginProofs.v` files in `examples/Bisimilarity/**`, and the CADP proofs in `examples/CADP/` (`MutualExclusion.v`, `GluedLayered.v`, `Fifo.v`, ...). `dune build` builds only the fast ones (`examples/dune`): full proof search is slow, and some examples need tens of thousands of steps and gigabytes of memory.

`bench/proofs.sh` runs the rest: it compiles each proof file in its own memory-capped process, against the dune build, and tabulates the solver's step counts, which `CLAUDE.md` lists as the known-good baseline (see [`bench/README.md`](bench/README.md)). Run its suites one file at a time (`-j 1`) with a cap (`-M`) well below the memory your machine has free.



### Using VSCode
Use the **[VsRocq extension](https://github.com/rocq-prover/vsrocq)**. It reads `_RocqProject`, which points at dune's build, so run `dune build` before restarting it (**VsRocq: Restart**) after a change to the plugin.

Editor settings are per machine and are not kept in the repository (`.vscode/` is ignored; until 2026-11-05 a `settings.json` for a local switch was). Two to set, in your own `.vscode/settings.json`, depending on your opam switch:

- **A global switch** (such as `default`): `vsrocqtop` and the OCaml tools are on your `PATH`, so nothing is needed, or `"ocaml.sandbox": { "kind": "opam", "switch": "default" }`.
- **A local switch** (`_opam/` in the project root, from `opam switch create .` or the nix flake): `"ocaml.sandbox": { "kind": "opam", "switch": "${workspaceFolder}" }`. A local switch is identified by its full path, so `${workspaceFolderBasename}` does *not* work: opam reads a bare name as a *global* switch. If VSCode is launched outside the direnv environment, `_opam/bin` is not on `PATH`, so also set `"vsrocq.path": "${workspaceFolder}/_opam/bin/vsrocqtop"`.

The OCaml editor tooling goes in the switch:
```shell
opam install ocaml-lsp-server ocamlformat
```

## Usage

Every command below starts with `MeBi` and needs `Require Import MEBI.loader.` first. This is the current command surface — see `src/g_mebi.mlg` for the exact grammar, and `test/rocq/Test.v`/`test/rocq/DevTest.v` for more worked examples.

The worked examples (process calculi, CCS and the Alternating Bit Protocol, the CADP MCS lock and its properties) are described in [`examples/README.md`](examples/README.md).

### Building an LTS or FSM

```
MeBi Run LTS <term> Using <relation> [<relation>...].
MeBi Run FSM <term> Using <relation> [<relation>...].
```

`<relation>` must be an inductive relation of type `Term -> Label -> Term -> Prop` describing `<term>`'s transitions; any further `<relation>`s let mebi unfold auxiliary definitions it needs while exploring. `Run LTS` builds the labelled transition system as-is; `Run FSM` additionally deduplicates into a finite state machine. Example:

```coq
Inductive testLTS : nat -> bool -> nat -> Prop :=
  | test1 n : testLTS (S n) true n
  | test2 : testLTS (S 0) false (S 0).

MeBi Run LTS 0 Using testLTS.
```

### Bisimilarity, merging, minimizing, saturating

```
MeBi Run Bisim <term> With <relation> And <term> With <relation> [Using <relation>...].
MeBi Run Merge <term> With <relation> And <term> With <relation> [Using <relation>...].
MeBi Run Minimize <term> Using <relation> [<relation>...].
MeBi Run Saturate <term> Using <relation> [<relation>...].
```

`Run Bisim` checks (weak, if `Config Weak` is set — see below) bisimilarity between the two terms' LTSs. `Run Merge` combines two FSMs into one. `Run Minimize` partition-refines an FSM down to its bisimulation quotient. `Run Saturate` computes weak transitions across silent (tau) steps.

Each `Run` command ends with a `Notice` saying what it found: the term and its size (`(LTS of p: 3 states, 2 transitions.)`), or the verdict (`(Bisimilar: p and q are weakly bisimilar.)`). Two terms that are not bisimilar (or not similar) are an error naming both, or a warning under `MeBi Config FailIf NotBisimilar False`. `rocq compile` does not print the message of a command under `Fail`, so a test that should show why a verdict is negative can use that setting and read the warning. The full FSMs and results are printed with `MeBi Config Output "Result" True`, as plain text by default (`Output Format`, below).

### Which constructor shapes are supported

An LTS is an inductive relation `term -> label -> term -> Prop`. For each constructor, mebi matches the source term and then handles its premises:

- **Premises over an LTS listed in `Using`** (the LTS itself, or another one: layered LTSs) are explored, and a constructor may have several. The proof search replays every premise's derivation. One such shape does not terminate today: a premise over the relation being defined, on a copy of the source, as in a structural-congruence rule `cong p p' -> cong q q' -> step p l q -> step p' l q'` with `cong` reflexive (no bound stops it). Put that premise over a relation without the rule, so that it applies once, or prove through a normalised semantics (below, "When structural congruence blows up the state space").
- **Any other premise** is decided once it is closed, either immediately or after the LTS premises have fixed what it mentions (a label, say). An equation `l = r` holds when its sides are convertible and fails when they differ in a constructor. Any other inductive proposition (`<=`, `<`, `In`, `Forall`, `/\`, `\/`, your own) is decided by a bounded proof search over its constructors, after unfolding definitions and fixpoints. The bound is `Set MeBi PremiseDepth <n>` (default 16). A false premise means that transition is not in the LTS. In a proof, a true premise is closed with the proof the search found, and a false one in a hypothesis is refuted. "False" is only concluded when the search was complete: nothing opaque, no depth cut.
- **Premises that compute something** (the target in `succ_rel n m -> lts n a m`, or a value an LTS premise needs, as in `In q l -> lts q a q'`) are enumerated: each solution is a transition of its own. A value a function computes is best given as `In x (f p)`, with `f` computable. The plugin warns if the solutions found may be incomplete (depth bound, solution cap, something opaque), and if an LTS premise has to be explored from a source nothing determines; such an LTS is marked incomplete, which is an error unless `MeBi Config FailIf Incomplete False`. The latter gives only some of its steps, so put whatever fixes the source among the premises.
- **Negations** `~ P` hold exactly when `P` is refuted by a complete search; in a proof they are proved by refuting `P`.
- **Bounded universals** over `nat`, `forall k, k < n -> P k` or `forall k, k <= n -> P k` (also `n > k`), where `n` is a number once the premise is closed: they hold exactly when every `P i` does, each decided as above. Only up to `Set MeBi PremiseRange <n>` values of `k` (default 256): each value costs a search and the proof grows with the square of the range, so a wider premise is left undecided, with a warning saying why. In a proof they are assembled from the instances' proofs with the lemmas in `MEBI.Premises` (exported by `MEBI.loader`), and a false one in a hypothesis is refuted at a false instance.
- **A tactic of your own** for what the search cannot decide: `MeBi Config Premise Tactic <tactic>` (e.g. `(rewrite f_def; lia)`). It is tried on the premise and on its negation, both at extraction and in the proof. `MeBi Config Reset Premise` clears it, the depth and the range.
- **Anything else** cannot be decided: opaque functions or axioms without a user tactic, a predicate that `match`es on an argument nothing fixes yet (`barb p x := match x with ... end`, where only the conclusion mentions `x`: write it as `In x (barbs_of p)`, or match on `p`), or a search cut off by the depth bound. MeBi warns and marks the LTS incomplete, which is an error unless `MeBi Config FailIf Incomplete False` is set (the default is `True`). With it set, the constructor is applied as if the premise held, so the LTS may contain transitions that do not exist. A `Run Bisim` verdict on such an LTS may be wrong, but a proof cannot be: `Qed` still checks the premise.

**Relations with parameters**, such as one defined in a `Section` with `Context (D : env)`, are refused (`LTS_Has_Parameters`): `Using` names an inductive, not an applied term. Wrap the applied relation in one constructor:

```coq
Inductive step_D0 : term -> option act -> term -> Prop :=
| wrap t l t' : step D0 t l t' -> step_D0 t l t'.
MeBi Run LTS p Using step_D0.
```

MeBi then decides the premise `step D0 t l t'` with its proof search, which enumerates `t'`, so derivations deeper than `PremiseDepth` need it raised. A proof about `step_D0` carries to `step D0` in a few lines, with `Transfer.stutter_bisimilar` (at `eq`: the two take the same steps) and `transfer_wbis_l`; `ParameterisedLTS` in `test/rocq/Test.v` does both. The alternative is a copy of the constructors in a module with the values fixed (`examples/Evaluation/Depth.v`).

### Benchmarking

```
MeBi Benchmark LTS <min-size> <max-size> <term> Using <relation> [<relation>...].
```

Times repeated LTS construction over a range of sizes using the `benchmark` package.

### Proof search (`bisim` and `sim`)

Inside a proof whose goal is bisimilarity-shaped, the tactics `bisim` and `sim` run a proof-search state machine instead of writing the proof by hand. Two goal shapes are supported, between closed terms:

- `weak_sim r1 r2 t1 t2`: weak *simulation*, one direction.
- `weak_bisimilar r1 r2 t1 t2`: weak *bisimilarity* (one relation that is a simulation both ways), proved in one go. `mutual_sim` (two separate `weak_sim`s) is weaker; see the comment in `theories/Bisimilarity.v`. The default `Auto` strategy costs the nested and the mutual cofix on the game and takes the cheaper, and says which. For `weak_bisimilar` that is usually the mutual one: forced nested, the `weak_bisimilar` proofs of `examples/Bisimilarity/Proc` and `examples/CADP` overrun their checked-in bounds. It can still pick nested on a small game, and either can fail to close (`doc/feedback-2026-10-15.md`, item 4); then try the other with `MeBi Config Solver MutualCofix True` or `False`. (On an FSM saturated on demand, `Auto` estimates only within `Set MeBi MaxGame <n>`, and without it takes the nested cofix; it says so in a notice. Set the bound.) The `weak_bisimilar` goals in the `PluginProofs.v` files of `examples/Bisimilarity/Proc`, in `examples/Bisimilarity/CCS/ABPBisimProofs.v` and in `examples/CADP/Fifo.v` are worked examples.

What a proved relation says about behaviour is in `theories/Traces.v`. A `weak_sim` gives trace inclusion, and with it any safety property over traces. Mutual similarity gives trace equivalence. `weak_bisimilar` also matches, after any trace, what each system can do next, including deadlock. Divergence is not preserved.


```coq
Set MeBi MaxSteps 1000.                (* optional: the default is 1,000,000 *)
Example wsim_pq : weak_sim termLTS termLTS p q.
Proof. sim. Qed.
Example wbis_pq : weak_bisimilar termLTS termLTS p q.
Proof. bisim. Qed.
```

`bisim` takes a `weak_bisimilar` goal and `sim` a `weak_sim` goal; each refuses the other's, naming it. They read from the goal the two relations, the two terms, the label type (in `M -> option A -> M -> Prop`, `None` is the silent step) and the relations the constructors' premises use (layered LTSs). They build both LTSs, check them, run up to `MaxSteps + 1` steps of the search, print `Solved after <n> iterations`, and close the goal with the proof found. A goal mentioning local hypotheses, or behind a definition, is refused: `subst`, `unfold` or give the witness of an `exists` first.

**Bounded and debug runs**, as with `auto n` and `debug auto`: `bisim n` and `sim n` take at most `n` steps (`0`: only the setup) and, if they find no proof, leave the goal as it was, with a notice (plain `bisim` and `sim` fail instead). With the `debug` prefix, `debug bisim n` also prints the search's open goals where it stopped, so you can look, change `n` and run it again.

`MeBi Run Sim x With r1 And y With r2 [Using ...]` decides weak *similarity*, as `Run Bisim` decides bisimilarity: is `x` weakly simulated by `y`? Similarity is a preorder, so the order matters. It walks the pairs of states reachable from `x` and `y`; when an FSM is saturated on demand, that walk needs `Set MeBi MaxGame <n>` and is refused without it.

(Until 2026-11-05 the search was driven by commands, `MeBi Sim Begin`, `MeBi Sim Step` and `MeBi Sim Solve <n>`, and `MeBi Run Bisim ... As <name>` stated and began a proof. The tactics replaced them.)

**When structural congruence blows up the state space.** A semantics that reorders and rebrackets parallel components with silent steps makes every arrangement a state of its own (`Proc/Test4`: 9720 states, beyond any proof). [`doc/structural-congruence.md`](doc/structural-congruence.md) is a recipe for proving such systems through a normalised semantics with one canonical state per class (`Test4`: 82 states), plus a one-off transfer theorem, with no change to the plugin.

**What a proof costs.** [`doc/decide-vs-prove.md`](doc/decide-vs-prove.md) puts deciding (`Run Bisim`) and proving (`bisim`/`sim` and `Qed`) side by side on the checked-in examples. A proof takes 40-500× its verdict, and past a certain size only the verdict is affordable.

### Configuration

```
Set MeBi MaxStates <n>.                            (* default 100 *)
Set MeBi MaxTransitions <n>.                       (* instead of MaxStates *)
Set MeBi MaxSteps <n>.                             (* proof-search steps of bisim/sim; default 1000000 *)
Set MeBi MaxSaturation <n>.                        (* weak actions; default 1000000 *)
Set MeBi MaxGame <n>.                              (* most game states a walk may visit on an FSM saturated on demand *)
Set MeBi MaxMemoryMB <mb>.                         (* stop a command past <mb> MB; default 75% of RAM; 0 = off *)
Set MeBi PremiseDepth <n>.
Set MeBi PremiseRange <n>.                         (* values of k a bounded universal premise may range over *)
Unset MeBi <Name>.                                 (* back to the default *)
MeBi Config Reset.                                 (* everything below, and the bounds above *)
MeBi Config Reset [Bounds|Weak|FailIf|Output].
MeBi Config Saturation OnDemand True|False|Auto.   (* default Auto *)
MeBi Config Premise Tactic <tactic>.
MeBi Config Reset Premise.
MeBi Config Weak As Option <term>.
MeBi Config Weak As <term> Of <relation>.
MeBi Config Weak1 / Weak2 As Option <term>.        (* set only the first/second side of a Bisim/Merge/Sim check *)
MeBi Config Weak1 / Weak2 As <term> Of <relation>.
MeBi Config FailIf Empty/Incomplete/NotBisimilar/Oversaturated True/False.
MeBi Config Output "<Kind>" True/False.
MeBi Config Output Format Text/JSON.                (* how results are printed and dumped; Text (plain text) is the default *)
MeBi Config Solver MutualCofix True|False|Auto.
MeBi Config Solver Answers Default|Greedy|Minimal|Auto.
```

The limits are Rocq options (until 2026-11-05, `MeBi Config Bounds ...` and `MeBi Config Premise Depth|Range` commands): set them once near the top of a file; they follow sections and modules, and are restored when you step back in an editor.

- `MaxStates`/`MaxTransitions` cap how large an explored graph may get before mebi gives up. Extraction has measured 0.01–0.07MB of memory per state on top of a fixed ~0.1–0.3GB, so setting a state bound whose upper estimate passes 1GB (about 14,000 states) prints a notice. Logging the result with `Output "Result"`/`"DecodeResults"`/`"DumpResults"` costs far more, about 0.65MB per state, because every term is pretty-printed.
- `MaxSaturation <n>` caps how many weak actions saturation (used by `Saturate`, `Minimize`, `Bisim` and the tactics whenever a label is silent) may produce. Saturation can be orders of magnitude larger than the LTS: `examples/Bisimilarity/Proc/Test4` has 9720 states and would saturate to 74.6M weak actions. Before saturating, mebi computes that number exactly (cheaply, without saturating). Above the bound, `Run Bisim` and the tactics saturate on demand instead (see `Saturation OnDemand` below), with a warning; `Run Saturate` and `Run Minimize` refuse with `Saturation_Too_Large`, naming the figure. The default is 1,000,000. Measured memory is 450–900 bytes per weak action, the upper end for larger LTSs (each action keeps its shortest witness path). Pick a bound your machine can hold. As a guide:

  | `<n>` (weak actions) | approx. memory |
  | -------------------- | -------------- |
  | 1,000,000 (default)  | 0.45–0.9 GB    |
  | 5,000,000            | 2.3–4.5 GB     |
  | 10,000,000           | 4.5–9 GB       |
  | 20,000,000           | 9–18 GB        |

  Time grows faster than linearly in the count (a partial `Test4` LTS with 400k weak actions takes ~13s), so a large bound can also mean a long wait. The proof examples (`PluginProofs.v`) saturate to at most a few hundred weak actions. Run `MeBi Config Output "Info" True.` to see each estimate.
- `Saturation OnDemand` (default `Auto`) is how `Run Bisim` and the tactics saturate each FSM. Whole, every weak action is built up front. On demand, each state is saturated when the check or the proof first needs it, holding at most `MaxSaturation` weak actions at a time (the oldest dropped and recomputed if asked about again), and bisimilarity is decided on the quotient by silent strongly connected components (states in one silent SCC have the same weak moves; `Test4`'s 9720 states make 81), with the same verdict. `Auto` goes on demand exactly for the FSMs whose saturation would exceed `MaxSaturation`, and warns; `True` does it for every FSM; `False` refuses those FSMs (`Saturation_Too_Large`, or with `FailIf Oversaturated False` saturates them whole anyway). `Run Saturate` and `Run Minimize` always saturate whole.
- `MaxGame <n>` (unset by default) caps how many pairs of states a walk of the whole proof may visit on an FSM saturated on demand. Without it, `Solver MutualCofix True` and `Solver Answers Greedy` or `Minimal` are refused on such an FSM, `Auto` (of either) takes the nested cofix and answers move by move without estimating or planning, and `Run Sim` is refused; with it they run within `<n>` pairs and are refused past it.
- `Weak` (and the asymmetric `Weak1`/`Weak2`) mark a label constructor as the silent/tau action. Without any, an LTS of type `M -> option A -> M -> Prop` has `None` as its silent label (since 2026-11-05; before, everything was strong without the setting), and other LTSs are strong. Setting only `Weak1` or `Weak2` leaves the other side strong. `Reset Weak` goes back to the default.
- `MaxMemoryMB <mb>` stops a command with `Memory_Bound_Exceeded` once the whole Rocq process uses more than `<mb>` megabytes (resident memory on Linux), so a heavy run fails with a message instead of taking the machine down. **On by default**: 75% of the machine's RAM, or 90% of a cgroup memory cap the process runs under (`systemd-run -p MemoryMax=...`) if that is smaller; `MeBi Help Config Bounds` shows the figure. `0` turns it off; `Unset` (or `Reset Bounds`) restores the default. The error names the phase it stopped in and what to try there. It is checked during extraction, saturation, the game walks and estimates, and between proof-search steps, so one step can overshoot it, and `Qed` is not checked at all: run heavy proofs under an OS cap as well (`systemd-run --user --scope -p MemoryMax=6G -p MemorySwapMax=0 ...`).
- `FailIf` controls whether an empty LTS (default `False`), an incomplete one (default `True`: exploration cut short by a bound, or a premise the search could not decide), a negative bisimilarity result (default `True`), or a saturation above `MaxSaturation` (`Oversaturated`, default `True`) raises a hard error instead of a warning. `Oversaturated` defaults to `True` because past the bound the likely alternative is running out of memory; `False` warns and saturates anyway.
- `Solver MutualCofix` picks how the proof search introduces coinduction hypotheses (one nested cofix per new pair, or one mutual cofix over the whole relation; `Auto`, the default, measures both). `Solver Answers` picks how it chooses each answer to the other system's move: `Auto` (the default, since 2026-10-15) the plan with the lowest predicted cost, among `Default`, `Greedy` and `Minimal`; `Default` move by move, as the solver did before; `Greedy`/`Minimal` from one of those plans. On the checked-in examples `Auto` was never slower than `Default`, and cut the `weak_bisimilar` proofs by 60% (`ASSISTED-CHANGES.md`, 2026-10-02). On an FSM saturated on demand, `Auto` answers move by move unless `MaxGame` is set.
- `Output "<Kind>" <bool>` toggles one log channel. `<Kind>` is one of `Debug`, `Info`, `Notice`, `Warning`, `Error`, `Trace`, `Result`, `Show`, `DecodeResults`, `DumpResults`, `Detail`. `Info`, `Notice`, `Warning`, `Error` and `Show` are on by default, `Debug`, `Trace` and `Result` off; `Info` is the routine kind, so `MeBi Config Output "Info" False.` quiets a run and keeps the rest. Loading the plugin prints one `Notice` saying so. `DumpResults` is off by default: when on, every command writes its FSMs and results , in the format set, into `./_dumps/` in the working directory, which is useful for debugging but costly on large systems (on `Proc/Test4`, about 20s, hundreds of MB on disk and 1GB of extra memory per command).
- `Output Format Text|JSON` sets how results are printed (`Output "Result" True`) and dumped. **Plain text is the default**: the states and labels numbered once, with their terms, then each move as `from -label-> to`:

  ```
  FSM: 3 states, 3 labels, 3 edges. Initial state: 0.
  states:
    0  (choice (pre A done) (tau (pre B done)))
    1  done  (terminal)
    2  (pre B done)
  labels:
    0  (Some A)
    1  None  (silent)
    2  (Some B)
  edges:
    0 -0-> 1
    0 -1-> 2
    2 -2-> 1
  ```

  `Format JSON` prints the full structure instead, as before 2026-10-19. `Output "Detail"` adds each move's derivation tree (`0 -0-> 1  via s_l(s_pre)`) and the relations' constructors; in JSON it also keeps the constructors' bindings and the annotations of weak actions. Unless set, `Detail` is off for plain text and on for JSON; once set, it holds for both. Each printed result is tagged with its form (`[plain text]`, `[JSON, without detail]`, ...), and the first one in a session is preceded by a `Notice` saying how to change it. Benchmark samples are always JSON. `Reset Output` restores the defaults.

### Diagnostics

```
MeBi Message "<text>".
MeBi Debug "<text>".
MeBi Divider.
MeBi Divider "<text>".
```

Print a message/debug line, or a visual divider (with an optional label) — useful for finding your place in a large build log.

### Help

```
MeBi Help.
MeBi Help <topic>.
```

`MeBi Help` lists the topics: `Run`, `Sim`, `Benchmark`, `Premises`, `Config`, and `Config Bounds`, `Config Bounds Saturation`, `Config Saturation`, `Config Weak`, `Config FailIf`, `Config Solver`, `Config Output`. `MeBi Help Config Bounds Saturation` gives the memory guidance above, computed from the same measured figures the plugin's errors and notices use.





## Status & Remaining Work

The core functionality described under [Usage](#usage) — reading a `Step : Term -> Label -> Term -> Prop`-shaped relation, building an LTS/FSM from a term, deciding (weak) bisimilarity, and turning the result into a Rocq proof via `MeBi Sim` — is implemented and working. What is still open is indexed in [`OUTSTANDING.md`](OUTSTANDING.md), with the detail in [`doc/limitations.md`](doc/limitations.md) (what the tool cannot do today), [`doc/future-work.md`](doc/future-work.md) and [`doc/decisions.md`](doc/decisions.md) (decisions for the repository's owner).



## Licence

Two licences, by directory (`LICENSE`; the texts are in `LICENSES/`):
- The **plugin** (`src/`, `lib/`, `test/`, `bench/`, and the files that load
  and test it) is **GPL-3.0-or-later**, so changes to it stay free.
- The **Rocq library** other developments import (`theories/Bisimilarity.v`,
  `Premises.v`, `Transfer.v`, `Hiding.v`, `Traces.v`) and `examples/` are
  **LGPL-2.1-or-later**, as Rocq itself is. The proofs the plugin generates
  refer to nothing but `MEBI.Bisimilarity` and `MEBI.Premises`, so a
  development that imports the library, or contains such a proof, is not
  bound by the GPL.

## Acknowledgements

This work was supported by the UK Engineering and Physical Sciences
Research Council (EPSRC) grants EP/Y00339X/1, *MEBI: Mechanised
Bisimilarities and Behavioural-typed Processes* (University of Kent), and
UKRI1247, *INDIMO: Invariant Discovery and Monitoring for Message-Passing
Programs* (Royal Holloway, University of London).

## Other Resources

### Templates
- [(Community) Coq Plugin Template](https://github.com/coq-community/coq-plugin-template)
- [(Community) Coq Program Verification Template](https://github.com/coq-community/coq-program-verification-template)

### Tutorials
- [(Official) Coq Plugin Tutorial](https://github.com/coq/coq/tree/master/doc/plugin_tutorial)
- [(tlringer) Coq Plugin Tutorial](https://github.com/tlringer/plugin-tutorial) [(see also)](https://dependenttyp.es/classes/artifacts/14-mixed.html)

### Other
- [Coq Makefiles](https://coq.inria.fr/doc/V8.19.0/refman/practical-tools/utilities.html#coq-makefile)
- [Writing Coq Plugins](https://coq.inria.fr/doc/v8.19/refman/using/libraries/writing.html)

- [Dune Init](https://dune.readthedocs.io/en/stable/quick-start.html)
- [Dune Coq Plugin Project](https://dune.readthedocs.io/en/stable/coq.html#coq-plugin-project)

- [Ltac](https://coq.inria.fr/doc/V8.19.0/refman/proof-engine/ltac.html)
- [Ltac2](https://coq.inria.fr/doc/V8.19.0/refman/proof-engine/ltac2.html)

- [`evar-map` helper functions](https://github.com/uwplse/coq-plugin-lib/blob/master/src/coq/logicutils/contexts/stateutils.ml) in [coq-plugin-lib](https://github.com/uwplse/coq-plugin-lib) (recommended by [tlringer](https://github.com/tlringer/plugin-tutorial/blob/main/src/termutils.mli))

### Papers
- [Popescu, A., Gunter, E.L. (2010). Incremental Pattern-Based Coinduction for Process Algebra and Its Isabelle Formalization](https://doi.org/10.1007/978-3-642-12032-9_9)
- [Rodrigues, N., Sebe, M.O., Chen, X., Roşu, G. (2024). A Logical Treatment of Finite Automata.](https://doi.org/10.1007/978-3-031-57246-3_20)
- [Stefanescu, A., Ciobaca, S., Moore, B., Serbanuta, T.F., Rosu, G. (2013). Reachability Logic in K](http://hdl.handle.net/2142/46296)

### Books
- [Sangiorgi, D. (2011). Introduction to Bisimulation and Coinduction](https://doi.org/10.1017/CBO9780511777110)
- [Sangiorgi, D., Rutten, J. (2011). Advanced Topics in Bisimulation and Coinduction](https://doi.org/10.1017/CBO9780511792588)
<!-- - []()
- []()
- []()
- []()
- []() -->
