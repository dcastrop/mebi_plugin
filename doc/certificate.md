# The certificate: Sylvan's computation, typechecked by Rocq

The plugin decides a goal with its engine (`lib/learn`) on Sylvan's list
decision diagrams, and proves it from **Sylvan's own computation**:
Sylvan logs each result it computes, and the plugin loads the log as
lemma applications that Rocq's kernel typechecks. Nothing is recomputed
in Rocq (CLAUDE.md, hard rule 1; `scripts/check_no_computation.py`,
first in CI, fails on any `vm_compute`, VM cast or the like). The
technical report's section "The certificate" (`doc/report`) is the guide
for newcomers; this note is the plan and its status. Optimisations:
[`optimisations.md`](optimisations.md).

## Terms

- **Engine:** the OCaml decision procedure (`lib/learn`) over Sylvan's
  list decision diagrams (LDDs, `lib/ldd`). It learns each system's steps
  as relations on vectors, explores both systems, then solves the
  simulation game.
- **The game's fixpoint** (`gfp`, `lib/learn/learn.ml`): start from `z` =
  all explored pairs; each iteration computes `bad(z)`, the pairs with a
  move the other side cannot answer into `z` (from preimages and silent
  closures); then `z := z \ bad(z)`; stop when `z` no longer changes. A
  proof needs the fact the last iteration establishes: every move from a
  pair of `z` has an answer into `z`.
- **Image, inverse image:** the image of a set of vectors by a relation is
  the set of vectors one step after them; by the relation with each
  tuple's read and written values swapped (its *inverse*), one step
  before them.
- **Proof log:** the record of every result Sylvan computes during one
  extra run of that check, made as images (below), with the case of the
  recursion it took.
- **Fact:** a lemma of `theories/DdTree.v` applied to a record's nodes:
  that result is what that case computes. A **chunk** is a set of facts
  declared as one constant, which the kernel checks when it is declared.

## A. Sylvan logs the certificate

- **The check, made again as images, logged.** Which iteration is the
  last is known only after it has run: it is the one where `z` does not
  change. So, once the fixpoint has finished, the engine clears Sylvan's
  operation cache and checks `z` once more with logging on
  (`lib/learn/learn.ml`, after `gfp`), as images: for each relation `r`
  of the mover (label `l`), the image of `z` by `r` (the pairs one move
  later) is within `ok_l`, the pairs the answerer can answer; `ok_l` is
  the image of `z`, closed under the answerer's silent steps backwards,
  by the inverses of the answerer's `l`-relations, closed again (for a
  silent `l`, the silent closure alone). Each mover's image is used
  *complete* (every move from a pair of `z` lands in it), each
  answerer's image *sound* (each pair in it has a step into the pairs it
  came from); the inclusion is a difference that comes out empty. For a
  bisimilarity, both sides in turn. Example: `z = {(p, s)}`, `p -a-> p'`,
  `s -a-> s'` and `(p', s') ∈ z`: the mover's image is `{(p', s)}`, the
  inverse image of `z` by the answerer's `a`-relation is `{(p', s)}`
  (`s` steps to `s'`), and `{(p', s)} \ {(p', s)}` is empty.
- **What the check needs, and what it does not.** It is the definition
  of a simulation, read on `z`: no universe (the pairs the game ranges
  over), no closure of the exploration, no intersection, no product of
  the explored sets, no preimage within a set. An inverse relation is
  built from the relation's tuples (`inverse`); a relation that only
  writes a position (`WO`) has no finite inverse, and the check is then
  not logged (Proc's `termLTS`).
- **The pairs' order.** A pair is one vector, the answering side's
  positions first (for a bisimilarity, the narrower side's), so that its
  relations, which the check applies most, start at the first position;
  and every meta stops at the relation's last touched position
  (`meta_levels`): past a meta's end, Sylvan carries the rest of the
  vector unchanged, with no step per node. Three-process CADP: the
  answerer's images are about 100 facts.
- **In the Sylvan fork** (`vendor/sylvan`, local branch `proof-log`):
  `lddmc_log_open`/`lddmc_log_close`; union, minus, intersect, relprod
  and relprev each record the result they compute (operation, case,
  arguments as the cache sees them, result), per worker, without a lock;
  at close, one file with the records and every node they reach.
  `lddmc_log_top` records a call the caller made at its top level, with
  its role in the caller's proof (op 6; `Ldd.log_top_relprod`: 1 sound,
  2 complete). Collection is off while the log is open (`Ldd.log_open`
  grows the tables to their maximum first), so Sylvan's node indices name
  the nodes.
- **Where the log goes:** in memory (`lddmc_log_open(NULL)`,
  `Ldd.log_open_mem`): each worker keeps its full buffers of records; no
  file. After close, `lddmc_log_take` hands the records out in order,
  freeing each buffer once taken, and the nodes are read from Sylvan's
  own table (`Ldd.node_fields`), collection staying off until the facts
  are written (`Ldd.log_close_mem`, then `Ldd.log_free`). With
  `Set MeBi Log Mode "keep"`, the log is a file instead: for a file `F.v`,
  `.F.v.mebi/L.log` next to it, for a lemma `L` (`Set MeBi Log
  Directory`), which `MeBi Check Log` reads. Every file the plugin writes
  goes there.
- **The translation** (`lib/certify/dd_cert.ml`, `write_log`): each record
  becomes the fact of its case, its result checked against the case.
  Unions and differences: `U`, `Mi` (`u_lt`, `u_gt`, `u_eq`, terminal
  cases; each union in the orientation its use asks for, so no swap
  fact). Images, complete: `PostC` (`post_*`, `WlC` for a write level),
  each skip with the comparison Sylvan made, as a proof from the numbers'
  binary digits (`plt_*`, `nlt_*`), shared within a chunk, under `Srt`
  (the sets sorted). Images, sound: `PostS` (`posts_*`, `WlS`), no
  comparison and no order (a skip only drops candidates). A relation's
  codes stop at its last touched position: `rstep` carries the rest
  unchanged, so an image at a meta's end is one fact (`post_end`,
  `posts_end`). The last step of a chain is folded into its parent's
  fact (`wl_last`, `post_copy_last`, `post_rw_last`, `u_lt0`, `u_gt0`,
  `u_eq_d`, `u_eq_r`, `mi_eq0d`, `mi_eq0r`): about 30% fewer facts. The
  roots go into chunks of about 20,000 facts (`MEBI_CHUNK_FACTS`; a root
  alone may give more), with chunk-local nodes, statement `True`.
- **Reading** (`lib/certify/dd_read.ml`): for a proof, the facts it asks
  for (`Dd_cert.request`: images, unions, differences, sortedness,
  memberships, nodes, codes), in one chunk, as `let`s around the proof,
  each line handed from the writer to the reader as it is written
  (`Dd_cert.To_lines`, `proof_reader`: no file). Alone (`MeBi Check
  Log`, `read`): each chunk one closed
  term, its nodes and its shared facts as `let`s, declared as an opaque
  constant, the largest chunk first (the kernel's memory for checking a
  chunk is about twice its term, so the largest meets the least held),
  with a tight GC.
- **Measured** (`bench/prooflog.py`, 10 November 2026): two-process CADP,
  15,200 facts checked in 0.7s, 0.34GB; three-process CADP (7.9M states),
  decided in 23s (1.33GB), 488,067 records, 668,460 facts in 8 chunks
  checked in 30s, 0.83GB at most. The translation alone
  (`test/cert_tool.exe --log`): 1.8s. Where the kernel's time goes
  (three processes): about 14µs per node `let` and 28µs per fact,
  typing and hash-consing about equally; neither binding the lists of
  codes and the numbers as `let`s nor one chunk instead of eight changes
  the total.

## The proof of a goal (`src/lts_proof.ml`, `theories/DdGame.v`, `theories/DdEnc.v`)

The tactic closes the goal with one term; the kernel checks it at `Qed`
(`test/rocq/LogTest.v`: closed under the global context). Built in this
order (`Lts_proof.qed_term`):

1. **The encoding of each system's states as vectors**, from the
   engine's tables. A vector `s :: rest` encodes a state when the shape
   numbered `s` says so: `enc Pt` looks the shape's predicate up in a
   trie `Pt` (`DdEnc.enc`), and each shape's predicate reads `exists u0
   .., rest = [u0; ..; 0 ..] /\ x = fill (val u0) ..`: the shape's
   constructors around its slots' values, each value looked up by its
   number in a trie per slot type (`DdEnc.val`; the kernel follows one
   branch per binary digit). `LenL`: every encoded vector of the left
   system has its width.
2. **The game on vectors**, from the log's facts (`src/game_proof.ml`):
   `VWSim rs inv z` (`VSSim` for strong), every move from a pair of `z`
   by a mover's relation answered into `z` by the answerer's relations
   logged backwards; each union, difference and image of the logged check
   a fact of `theories/DdTree.v`, each sub-proof used twice a `let`.
3. **The answerer's relations are sound** (`InvOkL`, `InvOkR`): every
   tuple of a relation, read backwards, is a step of the system
   (`TSound`). One lemma per relation and pair of source and target
   shapes, over the tuple's read and written numbers, takes the step's
   derivation as a hypothesis; each tuple applies it to its numbers and
   to its derivation (the engine's: the rule's constructor applied to its
   variables' values and its premises' derivations,
   `Learn.derive_chain`). `AllT` walks the relation's diagram, one
   tuple per leaf.
4. **The mover's relations are complete** (`CompleteTabR`,
   `CompleteTabL`): every step of an encoded state of `z` is a tuple of a
   mover's relation. Per shape, the step is taken apart by the LTS's
   case lemma (one per LTS: a disjunct per rule, the premises' labels and
   targets as variables, proved by a match on the step), each disjunct
   by small inversion: a match on an equation between the state and the
   rule's source, the remaining hypotheses abstracted again, a branch
   refuted where the constructors differ. A premise holding part of the
   shape is taken apart in turn; a premise that needs a slot's value
   waits for the **coverage** of `z` by the summand's learned projections
   (logged: the image of `z` by the shape's filter, complete, within its
   image by the projections, sound; `DdEnc.cov_of`), then one case per
   projection (`AllT` over the projections' diagram). The projections are
   grouped by the values of the slots the remaining hypotheses inspect
   (for CADP, a process's program point); per group one lemma, over the
   other slots' numbers, takes each step's witness as a hypothesis, and
   each projection is that lemma applied to its numbers and to witnesses
   built as terms (the tuple's membership, its reads by `eq_refl`, the
   target's encoding by a lemma per shape), with no elaboration. A
   premise from a
   closed term is taken apart by that term's **step lemma** (`forall
   holes Q, Q l1 t1 -> .. -> forall l t, L v l t -> Q l t`, proved the
   same way), one per LTS and term *skeleton*: the positions no rule
   inspects (every rule with that head has a variable there, used once,
   reaching premises only at such positions, and no side condition) are
   holes (`abstractable`), so CADP's process term is one lemma for every
   state and resource; a premise's target matched by pairs only is
   matched by projections (`fst`, `snd`) of a computed target such as
   `do_act a e`. At the end, the step's label and target
   pick the relation's tuple, its membership a fact of the log.
   Impossible cases are refuted from the encoding: a projection of
   another shape (by the shape's number), a rule that needs a slot to
   start with a structural constructor (no value of the slot's type does:
   `DdEnc.val_all`), a shape no summand has (`z` holds none of it: an
   empty image, logged).
5. **The theorem** (`DdGame.cert_strong_sim`, `cert_weak_sim`,
   `cert_strong_bisim`, `cert_weak_bisim`; a bisimilarity whose narrower
   system is the goal's second: `strong_bisimilar_sym`,
   `weak_bisimilar_sym`), applied to the start pair's encoding and its
   membership in `z`.

The texts name every global by its qualified name and every closed term
by a local definition (no term is printed); they are elaborated
(`Constrintern`), their sort variables settled. The proof's universe
constraints (CADP's types lie above `Set`) are the elaborations' and a
probe's (each constant built in OCaml applied to the goal's types, typed
once), added to the proof's state.

**Each definition is a constant** (11 November 2026), declared as the
tactic `abstract` declares a sub-proof: a side effect of the proof,
exported with it at `Qed` (named `<lemma>_mb_...`). The kernel checks
each when it is declared, once; the proof's term only names them. A proof
(a case lemma, a step lemma, a coverage lemma, a relation's soundness) is
opaque, and the elaboration sees only its statement, never its proof; a
datum (a value, an encoding, a predicate, a diagram) is transparent, or
written in place when its body is small. A definition over the
certificate's facts is abstracted over the facts it uses and named
applied to them. A diagram that a proof walks (the projections a coverage
covers, the tuples of a relation) is a datum built from its tuples, the
same tree as the log's node, which the term identifies with it at `Qed`
(by conversion). What is left in the term: the facts, the game, the
coverage proofs from the facts, the theorem. `MEBI_QED_LETS=1` makes the
definitions `let`s of the term instead.

**A constant takes few facts.** The kernel types an application in time
quadratic in its number of arguments (each argument's type is rebuilt
under the substitution of all those before: `Typeops.type_of_apply`), and
a constant over the facts is applied to every fact it uses. A coverage
of three processes uses thousands of memberships (a tuple in a
relation), one fact each: its application took minutes. So a coverage's
memberships are one fact, `TAll` of the trie of their statements (a datum
over the relations' diagrams), built in the term from the log's
membership facts; each leaf takes its membership by its key
(`DdEnc.tall_get`). The pack is checked apart from the term: the
translator writes a chunk per relation diagram the packs name (nodes
only: the diagram's constant, `RR`, its nodes as `let`s) and a chunk per
pack (its membership facts, its own nodes); the pack is an opaque
constant over those `let`s, given the statement over the `RR` constants
(the two statements compared once, diagram against diagram). Three
processes: the main term 1.12M facts down to 829K, and `Qed` under 6GB.

**A membership is one fact per level** (`DdTree.in_sib_d`): a vector of a
diagram, by the sibling holding its next value, reached along the list
of siblings (`Sib`, `sib_refl`, `sib_step`). A sibling list's steps do
not name the vector, so every vector scanning that list shares them; and
a membership's steps from a node depend on the node and the rest of the
vector only, so vectors meeting at a node (Sylvan shares subtrees) share
the rest. Three processes: one fact per sibling skipped, each naming its
vector, made 9.4M of the proof's 11.1M facts.

**What Rocq computes, beyond typing:** conversions only. A value's
number to its term (`val`, a trie lookup); a coverage's membership of a
tuple in a relation, by its key in the trie of the coverage's
memberships (`tall_get`, the same lookup: the kernel follows one branch
per binary digit of the key, no `eq_refl`); a vector after a tuple
(`post`, `Reads`) and a projection (`projv`) on lists of known length;
the user's own functions in a rule's label, target or premise source
(CADP's `do_act`, CCS's `def`); a decidable side condition's boolean
(`Decidable_sound`, `Decidable_complete_alt`, with `eq_refl`; CCS's
`allowed`). No boolean of the certificate's data is proved by
`eq_refl`: code lists' properties are inductive predicates proved by
their constructors (`NoWO`, `Within`, `Shifted`, `RoOnly`).

**The engine's side, for the proof:** a relation that writes a position
it does not read cannot be read backwards, so the engine reads every slot
a step overwrites, and reads a position that is no slot of the source
shape as its 0. The plan (`Learn.Plan`) records, per relation, its label,
codes, diagram, source shape and rule chain (keyed by label, meta and
node: relations with the same tuples share a node).

**Cost** (examples, 2026-11-11): LogTest and Proc under a second per
goal; CADP, one process, about a second per goal; two processes (12,027
pairs), the file in about 6s (the proof built in 3.3s, `Qed` 1.7s),
0.83GB at most; three processes (7.9M pairs), the proof built in 124s
(the packs' 280K membership facts checked then, 59s), `Qed` in 144s on
the main term's 829K facts, the file in 293s, 6.6GB at most.

## Decided (10 November 2026)

- **Evidence, not re-checking:** where Sylvan has decided a fact, the log
  records enough to write it in Gallina directly. A comparison of two
  node values becomes a proof built from the two numerals' binary digits,
  one lemma per digit. A lookup is avoided: lemmas take the element
  itself. Equal nodes are the same term.
- **Steps from the rules only:** the engine finds a term's steps by
  interpreting the LTS's rules; a step it cannot follow is an error
  (`Learn.Unfollowable`).
- **One extra logged check**, not a log of every iteration: the cache
  survives between iterations, so logging every one would keep every
  fact of every iteration, many times what the proof uses.
- **The logged check as images** (the mover's complete, the answerer's
  sound by inverse relations), not the game's preimages within the
  explored pairs: it needs no universe, no closure of the exploration, no
  intersection and no complete preimage, and the answerer's images, its
  positions first, are small.
- **The mover's image is built, then its difference with `ok_l`**, not
  checked within `ok_l` without building it: measured (three processes),
  a subset image in the Sylvan fork made 266K records instead of 488K but
  1.15M facts instead of 668K and 33s instead of 30s, because each image
  value must be found in the target by walking its right chain (one fact
  per node walked), while a built image's difference stops at the nodes it
  shares with `ok_l` (Sylvan's nodes are canonical).
- **`Decidable` premises are proved by the instance**
  (`Decidable_sound`, refuted by `Decidable_complete_alt`), with the
  kernel reducing the instance's boolean on the concrete values: one
  reduction per such premise per step. This is the one computation of
  the user's side conditions the design keeps; it is listed here because
  `scripts/check_no_computation.py` does not see it.
- **Inversion by terms the plugin builds** (11 November 2026): the case
  lemma per LTS is a match on the step, and every inversion a small
  inversion by match predicates in the generated text, so no tactic runs
  to prove anything about a goal.
- **The proof is Gallina text over named local definitions**, elaborated
  piece by piece (11 November 2026): the elaborator infers the motives of
  the generated matches and the arguments left implicit; no engine term
  is printed (every closed term is a local definition, every global a
  qualified name).
