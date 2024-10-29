# MEBI: Mechanised Bisimilarities

This repository contains a Coq plugin for automating bisimilarity proofs. 

**Work in progress**

## Setup
(currently using `coq 8.19.0` ?)
### using `dune`
```
dune build
```

### using `make`
```
coq_makefile -f _CoqProject -o CoqMakeFile
```
```
make -f CoqMakeFile
```

## TODO

So far, this is essentially `coq/doc/plugins_tutorial/tuto1` but renamed.  Here
is the current TODO list.

[] Reading `step` relation with type `Step : Term -> Label -> Term -> Prop'`
   that captures state transitions in a LTS semantics.
[] Reading terms `t : Term`. 
[] Building a state machine using `Step` for term `t`
[] Implementing one of the algoriths for deciding bisimilarity in Sangiorgi's
   book.

**Questions:**
  - We need to build a proof in Coq that two terms are bisimilar. We need the
    statement in terms of `Step`, and turn the result of our algorithm into
    sequences of Coq tactics.
  - Tau transitions/weak bisimilarity?
  - Open terms/use of existing lemmas?

## Scratchpad

### Command that declares a relation as a "LTS-generating relation": 

```
MeBi LTS <ident>.
```

* `<ident>` should be the identifier of a relation with type `Term -> Action ->
  Term -> Prop`.


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
- [Dune Coq Plugin Project](https://dune.readthedocs.io/en/stable/coq.html#coq-plugin-project)

- [Ltac](https://coq.inria.fr/doc/V8.19.0/refman/proof-engine/ltac.html)
- [Ltac2](https://coq.inria.fr/doc/V8.19.0/refman/proof-engine/ltac2.html)

