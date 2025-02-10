# MEBI: Mechanised Bisimilarities

This repository contains a Coq plugin for automating bisimilarity proofs (which are currently taken from the methods detailed in "Advanced Topics in Bisimulation and Coinduction", Section 3.2.2).


**Work in progress**

---



## Building the Project
Using **`coq 8.20.0`**, below is the "fool-poof" method:
```shell
make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile
```

### Running tests
Compiling using the above will also generate a `tests.exe` which can be run:
```shell
_build/default/test/tests.exe
```

### Altogether
```shell
make .merlin clean; dune build; coq_makefile -f _CoqProject -o CoqMakeFile; make -f CoqMakeFile; _build/default/test/tests.exe
```



### Using VSCode
Use the **[`vscoq` extension](https://github.com/coq/vscoq)** and build as shown above. Afterwards, you have to reload vscode. (**[This extension is helpful for this](https://marketplace.visualstudio.com/items?itemName=natqe.reload).**)



#### Issues with `vscoqtop`
Try adding this to your workspace settings `.json` file, under the `"settings"` field:
```json
"vscoq.path": "/home/user/.opam/mebi/bin/vscoqtop"
```
E.g.:
```json
{ "settings": {
    "vscoq.path": "/home/user/.opam/mebi/bin/vscoqtop"
} }
```

> Access this file by pressing **`ctrl+,`** and then clicking the file icon button in the top right corner, which will open the settings as a `json` file.





## Scratchpad

### Command that declares a relation as a "LTS-generating relation":

```
MeBi LTS <ident>.
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
