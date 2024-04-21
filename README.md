# MEBI: Mechanised Bisimilarities

This repository contains a Coq plugin for automating bisimilarity proofs. 

**Work in progress**

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

