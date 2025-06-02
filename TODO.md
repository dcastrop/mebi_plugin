# todo

- [ ] fix the printouts/dumps for FSMs, generalize the dumps using a functor to handle either LTS or FSM
- [ ] overhaul utils.params and utils.logging


- [ ] "glue" the `CADP.v` example (`p0`) together so that only the *external*
      (non `SILENT`) actions are performed, and the "internal" ones are all
      performed as one *big step*.
- [ ] test the performance of the plugin to build LTS for this glued version.
      (The current version would require `2^37 = 137438953472` states which is
      infeasible.)
- [ ] Check if this "glued" version is *bisimilar* to the original protocol.
- [ ] Check how this glued version compares to the minimized version too.


---

- [x] overhaul mebi monad into a wrapper that allows us to seamlessly just work
      with the encodings
- [x] since we support multi-layered lts, change `Constr_tree.t` from `int` to
      `(int * int)` since we need to know which layer the constructor index
      applies to. I.e, indexing from `0` the coq-lts in the order provided in
      the command.

---

## bug list

- [x] Issue of retriving correct `raw_lts` from hashtbl using term.

      Fixed this issue by normalizing the term before adding it to the hashtbl,
      rather than trying to normalise either the term before obtaining the type,
      or the type when fetching from the hashtbl.

- [ ] Issue where terms are not found in the hashtbl when they should be.

      This issue appeared to be (at least partially) resolved when using the
      `hash` of the terms to compare equality -- however, upon closer inspection
      I do believe I observed some collisions (false positive matches), even
      though this did also successfully match terms.

      Need to investigate the behaviour of `EConstr.eq_constr` to see why this
      does not appear to work in certain cases. (Specifically, it does not match
      some terms that do match when turned into strings.)

      > Lifting this restriction has caused duplicate states to be added.

- [ ] Investigating why duplicate states appear in set.

      After some printouts, it appears that `S.union` is the culprit (in this
      particular case). After looking into the definition, I wonder if the
      comparison function defined in `Mebi_monad` is causing the issue, as since
      we only return `0` or `1` the set is not actually ordered and therefore,
      the balancing of the tree that prepresents the set could cause arbitrary
      pockets of terms to not be found when perfoming the union. The crux of
      this seems to be the ordering of the elements in the set.

      We need a way or ordering the set of terms
      (states, which are currently `EConstr.t`).

      For now I have ***temporarily*** changed the comparison function to use
      the comparison of the string representations. This is definitely slower,
      with some of the more complicated examples taking far too long to finish.

      Could we somehow encode the terms as unique integers?

      I have now tried several different ways of encoding them as integers.
      From tests, these are equivalent and provide the same output and
      crucially, they do appear to have additional states that were missing, and
      appear to be without duplicates.
      These two attempts extend the monad with some collection we use to
      determine the terms encoding, either:
      - A `(EConstr.t * int) Hashtbl.t` that simply maps a term to an integer.
        The integer is simply the length of the tbl when it is added. However,
        out of concerns that this hashtbl would potentially encounter the issue
        of hash collisions, I also tried a more rudimentary approach:
      - A `EConstr.t list` containing each term in the order it is added to the
        set. Terms are prepended to the list if they are not already present. To
        retrieve the encoding of a term, find its position in the list and minus
        from the total length of the list -- this yields the same as just taking
        the index if they were *appended* to the list.

      The main notable thing here is that the "sanity check" functions now take
      a very long time on the more complicated examples, and therefore I have
      currently commented them out. (This is due to the currently inefficient
      methods that they iterate repeatidly over the same structures, and do lots
      of string comparisons.)

- [ ] Duplicates appear in `proc3_recbra1` in `Proc.v`.

      Realised this was caused by the comparison function for destination pairs
      not being updated in a similar way.

      ***Need to finish implementing `Constr_tree.compare`***
      <!-- Specifically, this starts occuring from exactly bound `135`, for
      transitions from:
      ```
      (tpar (tfix (tbra (tpar (tact ASend trec)
                              (tact ARecv trec))
                        (tpar (tact BSend tend)
                              (tact BRecv tend))))
            (tpar (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))
                  (tpar (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend))))
                        (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend)))))))
      ```

      Which itself is reached from one of the following:
      ```
      (tpar (tfix (tbra (tpar (tact ASend trec)
                              (tact ARecv trec))
                        (tpar (tact BSend tend)
                              (tact BRecv tend))))
            (tpar (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))
                  (tpar (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend))))
                        (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend)))))))

      (6) [(6) [(8) []]]
      ```

      ```
      (tpar (tpar (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))
                  (tpar (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend))))
                        (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend))))))
            (tfix (tbra (tpar (tact ASend trec)
                              (tact ARecv trec))
                        (tpar (tact BSend tend)
                              (tact BRecv tend)))))

      (8) []
      ```

      ```
      (tpar (tpar (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))
                  (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend)))))
            (tpar (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))
                  (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))))

      (10) []
      ```

      ```
      (tpar (tfix (tbra (tpar (tact ASend trec)
                              (tact ARecv trec))
                        (tpar (tact BSend tend)
                              (tact BRecv tend))))
            (tpar (tpar (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend))))
                        (tfix (tbra (tpar (tact ASend trec)
                                          (tact ARecv trec))
                                    (tpar (tact BSend tend)
                                          (tact BRecv tend)))))
                  (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))))

      (6) [(8) []]
      (6) [(10) []]
      ```

      ---

      Or, with the addition of the `do_clean` group of rules, the duplicates
      begin upwards and including a bound of exactly `125`, from:
      ```
      (tpar (tfix (tbra (tpar (tact ASend trec)
                              (tact ARecv trec))
                        (tpar (tact BSend tend)
                              (tact BRecv tend))))
            (tpar tend
                  (tfix (tbra (tpar (tact ASend trec)
                                    (tact ARecv trec))
                              (tpar (tact BSend tend)
                                    (tact BRecv tend))))))
      ``` -->

### archive

- [x] `Proc.v, Example proc0_send1` is missing `true` transitions.
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

  After investigating the contents of `g.to_visit` it appears that the issue may
  occur at the beginning of each iteration, when exploring the constructors from
  the current state.

  This has now been fixed by removing the following, which was misplaced and
  unused in `insert_constr_transition`:
  ```ocaml
  let* sigma = get_sigma in
  ```


- [ ] Investigating the `mebi_errors` of `unknown_term_type` and
  `unknown_tref_type` which appear to be occurring for even the most simple
  examples in `CADP.v`. Below is the current output message:
  (reformatted for legibility)
  ```
    None of the constructors provided matched type of term to visit.

    Term: (REC_DEF 0
            (SEQ (ACT NCS)
                 (SEQ (SEQ (ACT (WRITE_NEXT THE_PID NIL))
                           (SEQ (ACT FETCH_AND_STORE)
                                <{ if (EQ (VAR PREDECESSOR) (VAL NIL))
                                   then OK
                                   else (SEQ (ACT [...])
                                             (SEQ [...] [...])) }>))
                      (SEQ (ACT ENTER)
                           (SEQ (ACT LEAVE)
                                (SEQ (SEQ (ACT READ_NEXT)
                                          <{ if [...]
                                             then [...]
                                             else [...] }>)
                                     (REC_CALL 0)))))),
          ( 0, {| var_predecessor := None;
                  var_locked := false;
                  var_next := None;
                  var_swap := false        |},
            None,
            ([{| next := None; locked := false |}],
            (None, None))))

    Type:   (tm * (nat
                  * local_vars
                  * option (list error)
                  * (list qnode * (option nat * option nat))))
  ```

  I then proceeded to pass the keys of the `Hashtabl` that causes the error
  (following `Hashtbl.opt` returning `None`) and this is the error's output:
  ```
    Keys: [ (tm * (nat
                  * local_vars
                  * option (list error)
                  * (list qnode * (option nat * option nat)))) ]
  ```

  Clearly, this appears to be a match. However, after adding more checks to
  compare both the Stringified version and the `EConstr.t` themselves, there
  is still no match:
  ```
  Does Type match EConstr of any Key? = false
  Does Type match String of any Key? = false
  ```

  I briefly attempted to compare the `int hash` of the `Type` and `Key`, but
  quickly realised that in order to do it correctly, I would need to move it
  into the `mebi_monad`.

  My thoughts at this stage:
  - The string comparison above may be flawed, since it is hard to tell if the
    formatting added by `Pp` interferes with it. They certainly do *look* to be
    the same. (I have tried some basic attempts to "clean" the strings provided
    but this still returned `false`, and it didn't feel worthwhile to spend more
    time cleaning the strings since we won't be using strings anyway.)
  - Perhaps it is worth moving this map into the `MkGraph` module so that they
    are contained within the `monad` (which may fix the issue, as it is hard to
    tell how the `Hashtbl` is returning None given that they appear to be the
    same when printed out).
  - Additionally, putting it all in the `monad` would allow us to use the hash
    of the terms instead, which may be worth looking into.

  I am considering of whether it is worthwhile extending the `monad` itself such
  that it contains the `Hashtbl` mapping `Types` of terms to their `raw_lts`.
  But first I will try the `MkGraph` approach.

  The main justification for moving this into `MkGraph` is because we need to
  pass around a `Hashtbl` defined using the `monad` so that we can reference
  this as a `Type`. Otherwise, if we just use the `monad` to define the
  `Hashtbl`, we won't be able to pass this around.

  I have just finished updating `MkGraph` such that instead of `term_type_map`
  we now use `raw_lets H.t`. However, I have run into an issue being that if I
  continue down this path, I will need to pull both `check_valid_constructor`
  and `check_updated_ctx` into `MkGraph` -- but this is not desirable.
  As a small work-around, I currently just convert (via `seq`) to a normal
  `Hashtbl`. However, this does not appear to fix the issue.
  (I have also just merged the two maps, as it doesn't make much sense passing
  two around all the time.)

  Taking a step back, I am now going to reassues whether the issue originally
  lay with `build_rlts_map`. I have now refactored this (outside of `MkGraph`)
  to use `iterate` from `mebi_monad` (and again, have switch to just using a
  single `Hashtbl`). I do not notice any difference between these two approaches
  -- i.e., building the mapping from coq term types/names to `raw_lts` either
  before `MkGraph` or inside of it -- and therefore feel I should focus on other things. For now, my thoughts on this are:
  > It is clear the standard `Hashtbl` does not recognise a coq term type that
    matches one of the keys. To address this I have attempted to use the table
    defined within `mebi_monad`, by utilising `H` within `MkGraph` and building
    the mapping within. However, I subsequently ran into issues when needing to
    pass this information outside of `MkGraph` to `check_valid_constructor` and
    `check_updated_ctx`. Therefore, I think our options are to consider:
  > - Why are the terms not being matched? Does using `H` actually fix this?
      Would using the `hash` function in the monad fix this?
  > - Might it be worth extending the `monad` itself, so that we can just ask
      the monad "what is the `raw_lts` of `x`?" and it would return it for us.
      However, this feels like the wrong way to approach this, as the `monad`
      should just be a wrapper that handles the coq environment and context
      (`Environ.env` and `Evd.evar_map`).

      In the context of `check_valid_constructor` and `check_updated_ctx`,
      `raw_lts` (given below) is only required to obtain the
      `constructor_transitions` of the given `lts`. Therefore, it is worth
      noting that we would only need to extend the `monad` to store the mapping
      from `EConstr.t` to `(Constr.rel_context * Constr.types) array` (i.e., the
      constructors).
    ```ocaml
    type raw_lts =
      { coq_lts : EConstr.t
      ; trm_type : EConstr.types
      ; lbl_type : EConstr.types
      ; coq_ctor_names : Names.Id.t array
      ; constructor_transitions : (Constr.rel_context * Constr.types) array
      }
    ```

  ***This bug is still open***

  ---

  Returning to this bug. So far, this only occurs in `CADP.v` in examples with
  multiple layers, specifically where the semantics are over tuples. After
  fixing the error message printout, it is given below (reformatted for
  legibility):
  ```
  Type: (tm * ( nat * local_vars * option (list error) *
                (list qnode * (option nat * option nat)) ) )

  Keys: [ 'step' '(tm * env)' ]
  ```

  The initiala idea is that perhaps the plugin is unable to recognise that the
  lhs is actually `env`, which is defined as:
  ```coq
  Definition env : Type := state * resource.
  ```

  where:
  ```coq
  Definition state : Type := pid * local_vars * (option (list error)).

  Record local_vars :=
    { var_predecessor : index
    ; var_locked      : bool
    ; var_next        : index
    ; var_swap        : bool  }.
  ```

  and:
  ```coq

  Definition resource : Type := mem * lock.

  Definition mem : Type := list qnode.

  Record qnode := { next : index; locked : bool }.

  Definition lock : Type := index * index.
  ```

  in total:
  ```coq
  Definition env : Type :=
    (pid * local_vars * (option (list error))) * (* state *)
    (list qnode * (index * index)).              (* resource *)
  ```

  which compared to the type of the term provided (ignoring `tm`):
  ```coq
    nat * local_vars * option (list error) *  (* state* )
    (list qnode * (option nat * option nat)). (* resource *)
  ```

  does appear to be correct. (The only differences appear to be the names of,
  e.g., `index` and `pid`, and the parentheses used.)

  Notably, it gets stuck on the very first term, which `coq` successfully checks
  to be the correct type (i.e., `tm * env`). Additionally, this did work in the
  past when each of the constructors was applied exhaustatively.

  - Could it be that since it is the first term, then both
    `check_valid_constructor` and `check_updated_ctx` have not yet been called,
    meaning that we haven't started to build/update the context/universe of
    coq terms in the monad and therefore, are unable to unify the original term
    with anything?

    > The first idea from this would be to try to allow the "exhaustative"
      checking for only the first initial term. However, after inspecting the
      code, I see in `build_graph` that we do in fact update the environment
      for the initial state, and the error does not occur there. Therefore,
      it may be that the monad is somehow not staying updated, and I can see
      that we use a standard `Queue` for our terms `to_visit`, and I wonder if
      because this does not interact with the monad, that somehow the
      information that we have already typed this (successfully) is lost.

    I am going to now try to add a `Queue` to the monad.





  <!-- I think it is worth looking further "downstream" to check what is actually
  needed by `check_valid_constructor` and `check_updated_ctx`, and see if this
  can be optimized. (If this is not possible, then we may have to consider
  moving these functions inside of `MkGraph`, but I am hoping that this is not
  going to be necessary, and that we can just pass what is needed to the
  outside.) -->






- [x] Following explorations with the previous bug, I have encountered instances
  where the `coq_translation` yields `UNKNOWN_DEST_STATE` when convertin from
  `raw_lts` to `Lts.lts`. So far this is only occuring in `proc2` of `Proc.v`:
  ```coq
  tpar (tpar (tfix (tact ASend trec)) (tfix (tact ARecv trec)))
       (tpar (tfix (tact ASend trec)) (tfix (tact ARecv trec))).
  ```

  Upon closer inspection of the `_dumps`, it does indeed appear that certain
  states in particular are consistently yielding this issue. E.g.:
  ```json
  {
    "id": "1",
    "from": "(tpar (tfix (tact ASend trec))
                   (tpar (tpar (tact ARecv (tfix (tact ARecv trec)))
                               (tact ASend (tfix (tact ASend trec))))
                         (tfix (tact ARecv trec))))",
    "label": "false",
    "destination": "UNKNOWN_DEST_STATE:
             (tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))))",
    "info": "(5) [(9) []]"
  }, ...,
  {
    "id": "248",
    "from": "(tpar (tfix (tact ASend trec))
                   (tpar (tfix (tact ARecv trec))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                         (tfix (tact ARecv trec)))))",
    "label": "false",
    "destination": "UNKNOWN_DEST_STATE:
             (tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))))",
    "info": "(5) [(4) [(6) []]]"
  }, ...,
  {
    "id": "829",
    "from": "(tpar (tpar (tfix (tact ASend trec))
                         (tact ARecv (tfix (tact ARecv trec))))
                   (tpar (tact ASend (tfix (tact ASend trec)))
                         (tfix (tact ARecv trec))))",
    "label": "false",
    "destination": "UNKNOWN_DEST_STATE:
             (tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))))",
    "info": "(9) []"
  }, ...,
  {
    "id": "856",
    "from": "(tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tfix (tact ASend trec))
                               (tfix (tact ARecv trec)))))",
    "label": "false",
    "destination": "UNKNOWN_DEST_STATE:
             (tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))))",
    "info": "(5) [(5) [(4) [(6) []]]]"
  }, ...,
  {
    "id": "924",
    "from": "(tpar (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec))))
                   (tfix (tact ASend trec)))",
    "label": "false",
    "destination": "UNKNOWN_DEST_STATE:
             (tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))))",
    "info": "(7) []"
  }, ...,
  {
    "id": "1004",
    "from": "(tpar (tfix (tact ASend trec))
                   (tpar (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))
                         (tact ARecv (tfix (tact ARecv trec)))))",
    "label": "false",
    "destination": "UNKNOWN_DEST_STATE:
             (tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))))",
    "info": "(5) [(7) []]"
  }, ...,
  {
    "id": "1292",
    "from": "(tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tfix (tact ARecv trec))
                               (tact ASend (tfix (tact ASend trec))))))",
    "label": "false",
    "destination": "UNKNOWN_DEST_STATE:
             (tpar (tfix (tact ASend trec))
                   (tpar (tact ARecv (tfix (tact ARecv trec)))
                         (tpar (tact ASend (tfix (tact ASend trec)))
                               (tfix (tact ARecv trec)))))",
    "info": "(5) [(5) [(7) []]]"
  },
  ```

  After adding a `_check_states` that checks that each state in the transitions
  is present in the set of states, it appears this this is not the source of the
  issue. (I.e., the error must occur after `lts_graph` has been constructed.)

  I have been trying to remove any margin for error when interacting with
  `EConstr.t` terms, as I believe that some of the issues may arise from the
  monad somehow, e.g., if it is not correctly used, then the comparison of two
  coq terms may not behave correctly. This has led me to try and
  replace/refactor many of the loops to use the `Mebi_monad.iterate` function.
  (This is underway.)

  Refactoring parts of `DeCoq` to better utilize the `Mebi_monad.iterate` have
  not fixed the issue. Following this, the dumps produced have not been updated
  so that the LTS also include a list of states. From this, it is clear that
  the states that are `UNKNOWN` so not appear to be in the list of states, which
  is odd since I have added checks whenever an `UNKNOWN` is encountered that
  confirm that the `EConstr.t` term of the state is within `g.states`.
  I will now investigate the translation of states.

  After some tests, I can confirm that all the terms in `g.states` are indeed
  present in the translation table. Therefore, there must be some disparity
  between the terms stored in the transitions (i.e., `from` and `destination`)
  and those that were translated. Puzzlingly, checks reveal that such terms are
  in fact present in `g.states`. My thought at this stage is to check the
  comparison function defined in `Mebi_monad` that would be used to fetch values
  from the table `H`. Clearly, the one for set `S` is performing more desirably.

  Indeed, changing the comparison function from:
  ```ocaml
  let eq_constr st () = EConstr.eq_constr !st.coq_ctx
  ```

  to:
  ```ocaml
  let compare_constr st () t1 t2 = Int.compare (econstr_hash st () t1) (econstr_hash st () t2)

  let eq_constr st () t1 t2 = Int.equal (compare_constr st () t1 t2) 0
  ```

  does appear to have fixed the issued of `UNKNOWN` terms in the translation.

  Additionally, whilst debugging this issue I extended `lts_graph` to also
  contain the `init` state. This is because the original state is actually a
  `Constrexpr.constr_expr` rather than an `EConstr.t`, and we already do this
  convertion in `build_graph` when we begin exploring. Therefore, it felt
  appropriate to just store this state for later.

---

## Refactor `Command.ml`
- [ ] `check_updated_ctx`
- [ ] `check_valid_constructor`
- [ ] `build_lts_graph`
- [ ] `build_graph`
- [ ] `lts_graph_to_lts`

### Optimizations
- [ ] Add configuration option to use hashes of coq-terms rather than strings.
      (E.g., only use strings in `debug` mode.)
- [ ] Streamline the pure-LTS (of non-coq terms) by building the coq-translation
      to a pure-LTS on-the-fly.

## Add Plugin Support Structural Equivalence
If provided, when building the LTS of a coq-term the plugin should only consider
exploring new states that are not structurally equivalent to an already-explored
state.

## Overaul `Utils.params`
Overhaul the `params` used to determine the level-of-detail for logging and
formatting. The current approach ended up being rather combersome and,
unfortunately difficult to extend and use.
- Function `Utils.log` handles all printing and ensures the correct output is
  used (i.e., if in Coq then using `Feedback`).
- However, there is currently some overlap with the idea of "show details": some
  messages are classified as a "detail", whilst others will provide "further
  details" (e.g., `Fsm.PStr` will include additional meta-info).

## Bugs
- [ ] The plugin will crash if provided a term with a type that does not
  *exactly* match the type expected by the semantics. E.g.:
```coq
Inductive lts : tm * state -> action -> tm * state -> Prop := ...

Definition process : type := tm * state.
Example p1 : process := ...

MeBi Show LTS Of p1 Using lts. (* <- error not found *)
```
