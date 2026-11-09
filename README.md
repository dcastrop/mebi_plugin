# MEBI: Mechanised Bisimilarities

This repository contains a Rocq (formerly Coq) plugin for strong and weak simulation and bisimilarity between terms of labelled transition systems given as inductive relations. A goal is decided symbolically, on Sylvan's decision diagrams; Sylvan logs its own computation as it decides, and that log, translated into lemma applications, is the certificate that Rocq's kernel typechecks. Nothing is recomputed in Rocq.

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
    - [How it works](#how-it-works)
    - [Command reference](#command-reference)
    - [Which constructor shapes are supported](#which-constructor-shapes-are-supported)
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

A branch pin follows the
branch: `opam update` fetches whatever is on `main` then, and `opam upgrade
rocq-mebi` (or `opam update --upgrade`) rebuilds it. To stay on one version,
pin a commit (`#<hash>`). It builds the plugin from source, which takes a few
minutes, and builds Rocq and OCaml too if the switch is new.

Then, in a `.v` file, a tiny system, decided and proved:

```coq
Require Import MEBI.loader.
Require Import MEBI.Bisimilarity.

Inductive st : nat -> option bool -> nat -> Prop :=
  | p0 : st 0 (Some true) 1 | p1 : st 1 (Some false) 0
  | q0 : st 10 (Some true) 11 | q1 : st 11 (Some false) 10
  | r0 : st 20 None 21 | r1 : st 21 (Some true) 22 | r2 : st 22 (Some false) 20.

Lemma w : weak_bisimilar st st 0 20.    Proof. weak_bisim. Qed.
```

`weak_bisim` decides the goal (`None` is the silent step); Sylvan records the decision's last checks, the certificate (in memory; with `Set MeBi Log Mode "keep"`, in `.File.v.mebi/w.log` next to the file, for a file `File.v`), and the tactic closes the goal with the proof built from that certificate, which the kernel checks at `Qed`. `bisim` and `sim` decide `strong_bisimilar` and `strong_sim` goals, `weak_bisim` and `weak_sim` the weak ones; `0` and `20` are weakly but not strongly bisimilar, so `bisim` refuses that goal. [Usage](#usage) describes the commands; larger worked examples are in [`examples/README.md`](examples/README.md). It is a research prototype.

The licence is in [Licence](#licence).

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
git clone --recursive https://github.com/dcastrop/mebi_plugin && cd mebi_plugin
# Sylvan and Lace are submodules: after a plain clone, git submodule update --init
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
It builds the plugin (`src/`, `lib/`), `theories/`, `test/rocq/` (`LogTest.v`, `CongruenceTest.v`) and the examples `examples/dune` lists, each goal decided and its certificate checked. `CADP/TwoProcesses.v` (two processes against the lock server, proved to `Qed` in about 6s, 0.83GB) and `CADP/ThreeProcesses.v` (three processes, 7.9M states, proved to `Qed` in about 5 minutes, 6.6GB) are run by `python3 bench/prooflog.py`, under memory caps (see [Running tests](#running-tests)).

`dune build` runs as many jobs as the machine has cores. Each Rocq job can take a few hundred MB, so on a machine short of memory, limit them: `dune build -j 4`.

`_RocqProject` is for the IDE only: it maps the logical names (`MEBI`, `MEBI.Examples`, `MEBITests`) to dune's outputs in `_build/`, so build before stepping through a file. 

### Faster inner loops
When you don't need the tests and examples:

| Changing                                | Command                                                                                  |
| --------------------------------------- | ---------------------------------------------------------------------------------------- |
| OCaml plugin code                       | `dune build @check` — type-checks everything in `lib/` and `src/`, builds no `.vo` (~2s) |
| OCaml, and you want the loadable plugin | `dune build src/`                                                                        |
| The plugin and library, no tests        | `dune build @install` — what opam builds (~7s from clean)                                |
| A single test file                      | `dune build test/rocq/Test.vo`                                                           |
| Everything, before committing           | `dune build`                                                                             |

`dune build @check` also produces the `.cmi`/`.cmt` files merlin needs, so it doubles as the "make my editor happy" command.

### Running tests
`dune exec test/ldd_tests.exe` tests the bindings to Sylvan's decision diagrams and the proof log. `dune build` also compiles `test/rocq/*.v` (the proof log, structural congruence) and the fast examples (`examples/dune`), each goal decided and its certificate's facts checked.

`python3 bench/prooflog.py` decides two- and three-process CADP and checks their certificates, each step in its own memory-capped process ([`bench/README.md`](bench/README.md)). Run heavy files one at a time, with a cap well below the memory your machine has free.

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

### How it works

The tactics prove a goal between two closed terms of two LTSs, each an
inductive relation `M -> option A -> M -> Prop`:

| goal | tactic | a step is answered by |
|---|---|---|
| `strong_sim lts1 lts2 s t` | `sim` | one step with the same label (`None` included) |
| `strong_bisimilar lts1 lts2 s t` | `bisim` | the same, both ways, one relation |
| `weak_sim lts1 lts2 s t` | `weak_sim` | silent steps (for `None`), or silent steps, the step, silent steps |
| `weak_bisimilar lts1 lts2 s t` | `weak_bisim` | the same, both ways, one relation |

The definitions are in `theories/Bisimilarity.v`. Each tactic:
1. **decides** the goal symbolically (`lib/learn`, [`doc/learn.md`](doc/learn.md)): the constructors are read as rules, states become vectors (containers: a shape and a value per slot), the steps of each rule are learned by an interpreter of the rules and stored as List Decision Diagrams (Sylvan), and the (strong or weak) simulation game is solved as a greatest fixpoint. A negative verdict is an error.
2. **logs the certificate**: once the fixpoint is reached, its check is made again as images with Sylvan's proof log on: every move from the relation `z` (the image of `z` by each relation of the mover's) lands among the pairs the answerer can answer (images of `z` by its relations backwards). Every result Sylvan computes (union, difference, image) is recorded with the case its algorithm took, each top-level image with the way the game's proof uses it: complete for a move, sound for an answer ([`doc/certificate.md`](doc/certificate.md)).
3. **proves the goal from it:** each record becomes a fact, a lemma of `theories/DdTree.v` applied to the record's nodes; from the facts, the game on vectors (`theories/DdGame.v`); and, generated for the goal's own LTSs, the encoding of their states as vectors, each answerer's tuple a step of its system (by the step's derivation), and each step of the mover's a tuple (by case analysis on the rules) (`theories/DdEnc.v`, `src/lts_proof.ml`). The tactic closes the goal with that proof; the kernel checks it at `Qed`. `MeBi Check Log` checks a kept log's facts on their own, a chunk at a time.

A system's **structural congruence** written as silent rules (commutation `p | q -> q | p`, associativity, a swap: rules with no premise that only reorder their pattern's variables, each with its inverse) is found in its rules, or given by hand (`MeBi Congruence <lts> <rules>`); `Print MeBi Skeleton` lists it ([`doc/upto.md`](doc/upto.md)). Proofs up to it, and the other optimisations (confluence reduction, static confluence, movers, compositional proofs), are sketched on the certificate in [`doc/optimisations.md`](doc/optimisations.md).

A tactic used on another kind of goal fails, naming the right one. A goal mentioning local hypotheses, or behind a definition, is refused: `subst`, `unfold` or give the witness of an `exists` first. `debug weak_bisim` (and so on) also prints the engine's report: states, pairs, summands, learned projections, game iterations, time.

What a proved relation says about behaviour is in `theories/Traces.v`: a `weak_sim` gives trace inclusion, and with it any safety property over traces; `weak_bisimilar` also matches what each system can do next after any trace, including deadlock. Divergence is not preserved. Strong implies weak (`strong_sim_weak`, `strong_bisimilar_weak`).

### Command reference

Every command and tactic the plugin defines (`src/g_mebi.mlg`). `MeBi Help <topic>` gives the details of each group.

**Tactics** (in a proof; `MeBi Help Sim`)

| | |
|---|---|
| `sim.` / `bisim.` | decide a closed `strong_sim` / `strong_bisimilar` goal and prove it from its certificate |
| `weak_sim.` / `weak_bisim.` | decide a closed `weak_sim` / `weak_bisimilar` goal and prove it from its certificate |
| `debug <tactic>.` | the same, and print the engine's report |

**The certificate** (`MeBi Help Debug`)

| | |
|---|---|
| `Set MeBi Log Mode "off"\|"keep".` | Sylvan's proof log, the certificate: in memory, freed once the proof is built (default), or kept in `.<File>.v.mebi/<Lemma>.log` next to `<File>.v` ([`doc/certificate.md`](doc/certificate.md)) |
| `Set MeBi Log Directory "<dir>".` | make `.<File>.v.mebi` in `<dir>` instead of next to the file |
| `MeBi Check Log "<file>".` | turn a kept log's records into facts, each chunk a constant the kernel checks (a relative path is taken from the file's directory) |

**Debugging** (`MeBi Help Debug`)

| | |
|---|---|
| `Print MeBi Rules <lts>.` | an LTS's constructors read as rules |
| `Print MeBi Skeleton <term> Using <lts>.` | a start term's shapes, slots and summands, and the rules taken as its structural congruence ([`doc/skeleton.md`](doc/skeleton.md), [`doc/upto.md`](doc/upto.md)) |
| `MeBi Congruence <lts> <rule> ...` | take these silent rules of `lts` as its structural congruence, instead of those found in its rules |
| `MeBi Message "<s>".` / `MeBi Debug "<s>".` / `MeBi Divider ["<s>"].` | print a message, a debug message, a divider |

**Output** (`MeBi Help Output`)

| | default | |
|---|---|---|
| `Set Debug "mebi"` / `"mebi-trace"` / `"mebi-info"` | off | debug output; routine progress is `mebi-info` |
| `Set Warnings "-mebi"` / `"+mebi"` | shown | hide MeBi's warnings / make them errors |

`MeBi Help.` lists the topics: `Sim`, `Debug`, `Output`.

The worked examples (process calculi, CCS and the Alternating Bit Protocol, the CADP MCS lock and its properties) are described in [`examples/README.md`](examples/README.md).

### Which constructor shapes are supported

An LTS is an inductive relation `term -> label -> term -> Prop` without parameters. Every premise of every constructor must be one of:
- **a step** of an LTS (the same one, or another: stacked LTSs), several allowed, whose source the rule's source or an earlier premise determines;
- **a decidable proposition**: one with an instance of stdlib's `Decidable` class (`Stdlib.Classes.DecidableClass`), such as `n <= m` or `x = y` over a type with decidable equality.

A step the interpreter cannot follow from the rules (a premise of another kind, something opaque in a rule, a premise stepping from its own source without end) is an error, naming the term. **Relations with parameters**, such as one defined in a `Section` with `Context (D : env)`, are refused: wrap the applied relation in one constructor, and carry a result about it to the parameterised relation with `Transfer.stutter_bisimilar` (`theories/Transfer.v`).

There are no bounds to set. Run large decisions and checks under an OS memory cap (`systemd-run --user --scope -p MemoryMax=<cap> -p MemorySwapMax=0 ...`); Sylvan exits the process when its table is full.

## Status & Remaining Work

The engine decides strong and weak simulation and bisimilarity; Sylvan logs the decision as a certificate; the tactic proves the goal from it, and the kernel checks the proof at `Qed` (every example `dune build` builds; [`doc/certificate.md`](doc/certificate.md)). Every example ends with `Qed`, three-process CADP included (about 5 minutes, 6.6GB). Next: the optimisations on the certificate. What is still open is indexed in [`OUTSTANDING.md`](OUTSTANDING.md), with the detail in [`doc/limitations.md`](doc/limitations.md) and [`doc/future-work.md`](doc/future-work.md). A technical report is in [`doc/report`](doc/report) (`main.tex`); its "Status and what comes next" lists the decisions to double-check.

## Licence

GPL-3.0-or-later (`LICENSE`; the text is in `LICENSES/`). Sylvan and Lace,
the submodules in `vendor/`, keep their own licence (Apache 2.0).

## Acknowledgements

This work was supported by the UK Engineering and Physical Sciences
Research Council (EPSRC) grants EP/Y00339X/1, *MEBI: Mechanised
Bisimilarities and Behavioural-typed Processes*, and UKRI1247, *INDIMO:
Invariant Discovery and Monitoring for Message-Passing Programs*.

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
