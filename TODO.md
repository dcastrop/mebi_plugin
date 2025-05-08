# todo

## bug list

- `Proc.v, Example proc0_send1` is missing `true` transitions.
  LTS shows that the actions occur since `tpar (tend tend)` is present, but
  the transitions themselves do not occur. Additionally, the number of states
  reported is consistent with the states being reached, but for some reason the
  transitions are not added.

  Appears that the same state is somehow able to be added to set of states S,
  i.e., two states that produce the same string. Issue may lie in the comparison
  function in `mebi_monad`:
  ```ocaml
  let compare_constr st () t1 t2 =
    if EConstr.eq_constr !st.coq_ctx t1 t2 then 0 else 1
  ;;
  ```

  Changing it to the below appears to have fixed the issue.
  ```ocaml
  Int.compare (econstr_hash st () t1) (econstr_hash st () t2)
  ```

  Now the issue still remains that the LTS is missing any `true` transitions.



---

## Refactor `Command.ml`
- [ ] `check_updated_ctx`
- [ ] `check_valid_constructor`
- [ ] `build_lts_graph`
- [ ] `build_graph`
- [ ] `lts_graph_to_lts`

### Optimizations
- [ ] Add configuration option to use hashes of coq-terms rather than strings. (E.g., only use strings in `debug` mode.)
- [ ] Streamline the pure-LTS (of non-coq terms) by building the coq-translation to a pure-LTS on-the-fly.

## Add Plugin Support Structural Equivalence
If provided, when building the LTS of a coq-term the plugin should only consider exploring new states that are not structurally equivalent to an already-explored state.

## Overaul `Utils.params`
Overhaul the `params` used to determine the level-of-detail for logging and formatting. The current approach ended up being rather combersome and, unfortunately difficult to extend and use.
- Function `Utils.log` handles all printing and ensures the correct output is used (i.e., if in Coq then using `Feedback`).
- However, there is currently some overlap with the idea of "show details": some messages are classified as a "detail", whilst others will provide "further details" (e.g., `Fsm.PStr` will include additional meta-info).

## Bugs
- [ ] The plugin will crash if provided a term with a type that does not *exactly* match the type expected by the semantics. E.g.:
```coq
Inductive lts : tm * state -> action -> tm * state -> Prop := ...

Definition process : type := tm * state.
Example p1 : process := ...

MeBi Show LTS Of p1 Using lts. (* <- error not found *)
```
