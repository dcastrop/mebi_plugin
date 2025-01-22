# MEBI: Mechanised Bisimilarities

This repository contains a Coq plugin for automating bisimilarity proofs.

**Work in progress**

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

## Scratchpad

### Command that declares a relation as a "LTS-generating relation":

```
MeBi LTS <ident>.
```

* `<ident>` should be the identifier of a relation with type
`Term -> Action -> Term -> Prop`.

## To run the test of bisimilarity
> currently taken from "Advanced Topics in Bisimulation and Coinduction", Section 3.2.2.

### First build the project:
```shell
make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile
```

### Next, run the tests:
```shell
_build/default/test/tests.exe
```

### To build and run together:
```shell
make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile; _build/default/test/tests.exe
```

## Setup
*using `coq 8.20.0`*

### using `make`
Run the following commands
```
coq_makefile -f _CoqProject -o CoqMakeFile
```
```
make -f CoqMakeFile
```

### using **vscode** (with `vscoq`)
Use `make` method (above).
Assuming you are using `vscoq` extension, you will not be able to use `MEBI.loader` unless you add the following line to your `settings.json`:
```json
"vscoq.args": ["-R theories/ MEBI -w all -I src/ "]
```
(access by `ctrl+,` and find the button in the top right for opening the settings as `json` file.)

E.g., to add this for a specific workspace in vscode it may look like this:
```json
{
  "settings": {
    "vscoq.args": ["-R theories/ MEBI -w all -I src/ "]
  }
}
```

#### to build in vscode (whole plugin)
Run the following commands
```
make .merlin clean
```
```
dune build
```
```
coq_makefile -f _CoqProject -o CoqMakeFile
```
```
make -f CoqMakeFile
```

##### all-in-one:
```
make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile
```
then afterwards, you have to reload vscode. [this extension is helpful for this](https://marketplace.visualstudio.com/items?itemName=natqe.reload)

##### all-in-one (suppress warnings)
```
make .merlin clean; dune build --profile release; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile
```

#### build in vscode (ocaml tools only)

```
make .merlin clean; dune build
```

### ~~using `dune`~~  (*`dune` currently not supported*)
*issue (in vscode) with `theories/loder.v` where `mebi_plugin.cmxs` appears inside `_build/default/src` instead of under `src/`.*
```
dune build
```





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
- [Sangiorgi, D. (2011). Introduction to Bisimulation and Coinduction](https://doi.org/10.1017/CBO9780511777110)
- [Sangiorgi, D., Rutten, J. (2011). Advanced Topics in Bisimulation and Coinduction](https://doi.org/10.1017/CBO9780511792588)
<!-- - []()
- []()
- []()
- []()
- []() -->
