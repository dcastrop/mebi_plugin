# MEBI: Mechanised Bisimilarities

This repository contains a Coq plugin for automating bisimilarity proofs (which are currently taken from the methods detailed in "Advanced Topics in Bisimulation and Coinduction", Section 3.2.2).


**Work in progress**

---



## Building the Project

### Toolchain
Built against **Rocq 9.2** (`rocq-core` 9.2.0, `rocq-runtime` 9.2.0, `rocq-stdlib` 9.1.0), OCaml 5.4.0 and dune 3.23, in a **local opam switch** (`_opam/` in the project root).

Versions are pinned in two places, and both are committed:
- **`flake.nix` / `flake.lock`** — nix pins the system layer: `opam` itself, `pkg-config`, `gmp`, `zlib`, `git`, `make` and the C toolchain.
- **`rocq-mebi.opam.locked`** — opam pins the OCaml/Rocq layer: every one of the ~100 packages at an exact version, including `rocq-core.9.2.0`, `rocq-stdlib.9.1.0` and `ocaml-base-compiler.5.4.0`.

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
Edit the `(depends ...)` ranges in `dune-project`, then regenerate both files:
```shell
dune build rocq-mebi.opam        # dune-project -> rocq-mebi.opam
opam lock .                      # -> rocq-mebi.opam.locked
```
Commit both. Bumping `rocq-core`'s upper bound is a deliberate act — the plugin links against `rocq-runtime`'s OCaml API, which breaks across Rocq *minor* versions.

### Two build paths

**`dune build`** — the plugin (`src/`, `lib/`) and `theories/`. This is the everyday build.
```shell
dune build
```

**`make`** — the same, *plus* the `examples/` listed in `_CoqProject`. Use it when you want the examples checked.
```shell
make -j$(nproc)
```
`_CoqProject` deliberately enables only a subset of `examples/**/*.v`; the rest are commented out with a note on why (proof explosion, long compile). Comment lines back in to build more.

### Switching back to dune after running make
```shell
make dune
```
`make` writes generated files into the source tree (`src/g_mebi.ml`, `*.vo`, `*.glob`, `*.cm*`). Dune also claims those paths, so a following `dune build` fails with `Multiple rules generated ...`. Dune has no way to ignore them, so the artifacts have to go: `make dune` is just `make clean && dune build`.

### Faster inner loops
A full `dune build` takes ~1m40s, almost all of it the `MeBi Benchmark` vernaculars in `theories/DevTest.v`. When you don't need them:

| Changing | Command |
| --- | --- |
| OCaml plugin code | `dune build @check` — type-checks everything in `lib/` and `src/`, builds no `.vo` (~2s) |
| OCaml, and you want the loadable plugin | `dune build src/` |
| A single theory file | `dune build theories/Test.vo` |
| Everything, before committing | `dune build` |

`dune build @check` also produces the `.cmi`/`.cmt` files merlin needs, so it doubles as the "make my editor happy" command.

### Running tests
`test/tests.ml` is currently commented out in its entirety, so `_build/default/test/tests.exe` builds but does nothing. `test/saturation.ml` predates the model refactor and no longer compiles; it is excluded via `(modules tests)` in `test/dune`.



### Using VSCode
Use the **[VsRocq extension](https://github.com/rocq-prover/vsrocq)**; `vsrocqtop` is installed in the local switch. Rebuild before reloading the window, since the extension loads the compiled plugin.

`.vscode/settings.json` is set up for the local switch already. The important part:
```json
"ocaml.sandbox": { "kind": "opam", "switch": "${workspaceFolder}" }
```
A **local** switch is identified by its full path, so `${workspaceFolderBasename}` does *not* work — opam reads a bare name as a *global* switch and reports `The selected switch mebi_plugin is not installed`.

The OCaml editor tooling lives in the switch too:
```shell
opam install ocaml-lsp-server ocamlformat
```

#### Issues with `vsrocqtop`
If VSCode is launched outside the direnv environment it won't have `_opam/bin` on `PATH`. Point it at the binary explicitly:
```json
"vsrocq.path": "${workspaceFolder}/_opam/bin/vsrocqtop"
```

> Access this file by pressing **`ctrl+,`** and then clicking the file icon button in the top right corner, which will open the settings as a `json` file.





## Scratchpad

### Command that declares a relation as a "LTS-generating relation":

```
MeBi Run LTS <ident>.
```

* `<ident>` should be the identifier of a relation with type
`Term -> Action -> Term -> Prop`.





## TODO

So far, this is essentially
[`coq/doc/plugins_tutorial/tuto1`](https://github.com/coq/coq/tree/master/doc/plugin_tutorial/tuto1)
but renamed. Here is the current TODO list.

- [ ] Reading `step` relation with type
      `Step : Term -> Label -> Term -> Prop'`
      that captures state transitions in a LTS semantics.

- [ ] Reading terms `t : Term`.

- [ ] Building a state machine using `Step` for term `t`

- [ ] Implementing one of the algoriths for deciding
      bisimilarity in Sangiorgi's book.


**Questions:**
- We need to build a proof in Coq that two terms are bisimilar.
  We need the statement in terms of `Step`, and turn the result
  of our algorithm into sequences of Coq tactics.
- Tau transitions/weak bisimilarity?
- Open terms/use of existing lemmas?



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
