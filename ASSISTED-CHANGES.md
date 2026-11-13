# Assisted changes log

A record of work done on this codebase with Claude (Claude Code), so that
the direction of travel and the *kind* of each change stay legible to
anyone reviewing what was contributed and why. It records technical work
only, and names no people. Git history has every earlier state.

| tag | meaning |
| --- | --- |
| **Refactor** | Same behaviour, different structure. |
| **Bug fix** | Behaviour was wrong; now it isn't. |
| **Optimization** | Same behaviour, measurably less work. |
| **New feature** | Capability the *plugin* did not previously have. |
| **Tooling** | Build, dev environment or editor config. No plugin behaviour change. |
| **Docs** | Documentation only. |

**Standing policy on "New feature":** anything that would add a *plugin
capability* is raised explicitly and up front, before it is written.

---

## The engine on Sylvan's decision diagrams

The decision procedure (`lib/learn`) on Sylvan's list decision diagrams
(`lib/ldd`, Sylvan and Lace as git submodules, forks): an LTS's
constructors read as rules (`lib/rules`), states as containers (a shape
and a value per slot, `lib/skeleton`), each rule's steps learned on the
positions it reads by an interpreter of the rules, chained exploration,
and the weak and strong games as greatest fixpoints by preimages within
the product of the explored sets. `Print MeBi Rules`, `Print MeBi
Skeleton`; reorderings (commutation, associativity, swaps) detected in the
rules, or named by `MeBi Congruence`. Tests: `test/ldd_tests.ml`,
`test/rocq/CongruenceTest.v`.

## 2026-11-10 — The certificate: Sylvan's computation, typechecked

- **New feature (Sylvan's proof log;** the fork's local branch
  `proof-log`, commits `94f8584`, `61c926d`, `367f4d5`): union, minus,
  intersect, relprod and relprev record each result they compute, with
  the case taken, per worker; at close one file with the records and the
  nodes they reach; `lddmc_log_top` records a top-level call with its
  role. The plugin's side: `Ldd.log_open`/`log_close` (tables grown to
  their maximum first, collection off), `Ldd.log_top_relprev`, the reader
  `Proof_log`. The engine repeats the fixpoint's last check and each
  side's last exploration round with the log open, recording each
  top-level preimage with the way the game's proof uses it (soundness for
  the answerer's, completeness for the mover's).
- **New feature (the translation, `Dd_cert.write_log`):** each record as
  the fact of its case, a lemma of `theories/DdTree.v` (unions,
  differences, images; sound and complete preimages, each skip with the
  comparison Sylvan made, as a proof from binary digits); preimages proved
  the one way their role needs; a run of skipped sorted nodes in one fact
  (`innerc_ylt2/4/8/16`, with `Srt` ordering right siblings, `Lead`);
  comparisons shared within a chunk; compact tables (open addressing,
  packed integer keys; the log streamed).
- **New feature (`MeBi Check Log`, `Dd_read.read`):** a facts file read
  back a chunk at a time, each chunk a constant the kernel checks when
  declared, the largest first (the kernel hash-conses a chunk's term while
  checking it, so the largest chunk meets the least held memory), with a
  tight GC.
- **New feature (files next to the file being checked):** for `F.v`,
  `.F.v.mebi/` holds each lemma's log (`Set MeBi Log Mode`,
  `Set MeBi Log Directory`) and a log's facts while they are checked.
- **Refactor (the tactics):** a tactic decides, logs the certificate, and,
  on a positive verdict, leaves the goal open with a notice until the
  theorem from the certificate exists. Examples and tests decide each goal
  and check its certificate (`Proof. weak_sim. Abort.`, then
  `MeBi Check Log`); a negative verdict is an error.
- **Tooling:** `scripts/check_no_computation.py` (first in CI) fails on any
  computation as proof; `scripts/hook_no_computation.py`, a local
  PreToolUse hook, refuses an edit that writes one; `bench/prooflog.py`
  decides and checks the CADP examples, each step under a memory cap sized
  from free memory; `test/cert_tool.exe --log` translates alone, with the
  facts per lemma; `MEBI_LOG_FACTS` reads a facts file instead.
- **Docs:** `doc/certificate.md` (the design and its status),
  `doc/optimisations.md` (confluence reduction, static confluence, movers,
  compositional proofs, up to structural congruence, the state encoding,
  large certificates in pieces, each sketched on the certificate),
  `doc/learn.md`, `doc/limitations.md`, `doc/future-work.md`, `README.md`,
  `examples/README.md`, `bench/README.md`, `CLAUDE.md`, `OUTSTANDING.md`;
  the technical report rewritten around the certificate (its PDF to be
  rebuilt).
- **Measured** (`bench/prooflog.py`): two-process CADP, 59,819 facts
  checked in 2.0s, 0.43GB; three-process CADP (7.9M states), decided in
  27s, 2,461,163 facts in 30 chunks checked in 121s, 1.67GB at most.
  `LogTest.v`'s chunk is closed under the global context.
- **Mistakes, for the record:** the plugin first wrote its temporary files
  (certificates, facts) to a cache in the home directory, and experiments
  ran in a scratch directory on a tmpfs, where files are memory; every
  file now goes next to the file being checked.
- **Open:** only-write levels (Sylvan's meta code 4, Proc's `termLTS`) are
  not translated yet; the certificate's top level, each relation's tuples
  as the LTS's steps and the encoding of states come next
  (`OUTSTANDING.md`).

**Session tally:** New feature 4 · Refactor 1 · Tooling 1 · Docs 1 ·
Bug fix 0 · Optimization 0.

## 2026-11-10 — The check as images: the certificate a quarter of the size

- **Optimization (the logged check as images,** `lib/learn/learn.ml`):
  once the fixpoint is reached, its check is made again as images with
  Sylvan's proof log on: for each relation of the mover, the image of `z`
  (used complete) lies within the pairs the answerer can answer, images
  of `z` by its relations backwards (`inverse`: each tuple's read and
  written values swapped; used sound), closed under its silent steps; the
  inclusion is a difference that comes out empty. It needs no universe,
  no closure of the exploration, no intersection and no complete
  preimage. Each top-level image is logged with its role
  (`Ldd.log_top_relprod`, in place of `log_top_relprev`). A relation that
  only writes a position has no finite inverse: its check is not logged
  (Proc's `termLTS`, as before not translated).
- **Optimization (the pairs' order and the metas):** a pair's vector
  holds the answering side's positions first (for a bisimilarity, the
  narrower side's), so that its relations need no copied prefix; every
  meta stops at its relation's last touched position (`meta_levels`),
  past which Sylvan carries the rest of the vector unchanged. The
  answerer's images are about 100 facts on three-process CADP.
- **Refactor (`theories/DdTree.v`):** `rstep` carries the positions after
  the last code unchanged, so an image at a meta's end is one fact
  (`post_end`, `posts_end`); sound images (`PostS`, `WlS`, `posts_*`,
  `wls_*`: no comparison, no order); the preimage families (`PreS`,
  `InnerS`, `PreC`, `InnerC`, their 60 lemmas) and the padded codes
  (`copies`) deleted, with their translation in `lib/certify/dd_cert.ml`.
- **Optimization (folded chains):** a chain's last step and its terminal
  case in one fact (`wl_last`, `wl_last0`, `post_copy_last`,
  `post_copy0_last`, `post_rw_last`, `u_lt0`, `u_gt0`, `u_eq_d`, `u_eq_r`,
  `mi_eq0d`, `mi_eq0r`, each proved once from the two), and each union in
  the orientation its use asks for (no `u_comm` fact): 963,941 facts down
  to 668,460 on three processes, 10% less time (the folded facts were
  small).
- **Measured** (`bench/prooflog.py`): three-process CADP, 1.38M records
  down to 488K (log 86MB to 31MB), 2,461,163 facts down to 668,460,
  checked in 30s instead of 121s, 0.83GB at most instead of 1.67GB,
  decided in 23s instead of 27s; two-process CADP, 59,819 facts down to
  15,200, 0.7s, 0.34GB. Where the kernel's time goes: about 14µs per node
  `let` and 28µs per fact, typing and hash-consing about equally.
- **Mistakes, for the record:** binding each chunk's lists of codes and
  numbers as `let`s was built and measured (30.0s against 29.7s: no
  gain) and removed; one chunk instead of eight takes the same time and
  1.42GB.
- **Tooling:** `MEBI_CHUNK_FACTS` sets the facts per chunk;
  `test/ldd_tests.ml` logs a top-level image backwards instead of a
  preimage; `dune fmt` applied.
- **Docs:** `doc/certificate.md` (A rewritten: the check as images, the
  pairs' order, the translation, the numbers; "Next in A": a subset image
  in Sylvan, only-write levels, the top level), `OUTSTANDING.md` (the
  intersections and the product are no longer needed: removed; the
  report's PDF rebuilt: removed), `doc/limitations.md`, `doc/learn.md`,
  `README.md`, `CLAUDE.md`, `bench/README.md`, `examples/README.md`, the
  CADP examples' comments; the technical report's certificate, status,
  tests, overview and engine sections, its PDF rebuilt; comments that no
  longer matched the code corrected (a timing tip in `CLAUDE.md`, the
  lists of tests in the CI workflow and `test/rocq/dune`, a comment in
  `lib/learn/learn.ml`, `examples/Bisimilarity/Proc/Test4/TermTests.v`'s
  notes, references to a deleted test file in `theories/Bisimilarity.v`
  and `theories/Traces.v`, the Sylvan update steps in
  `lib/ldd/README.md`).

- **Measured and not kept: a subset image in the Sylvan fork**
  (`lddmc_relprod_sub`, the mover's image checked within `ok_l` without
  building it, with its lemmas and translation): three processes, 266K
  records instead of 488K, but 1.15M facts instead of 668K and 33s
  instead of 30s, because each image value is found in the target by a
  walk of one fact per node, where a built image's difference stops at the
  nodes it shares with the target. Removed, from the fork too.

- **New feature (the theorems from the certificate, `theories/DdGame.v`,
  proved once, closed under the global context):** the game on vectors
  (`VWSim`, `VSSim`), a toolkit from the logged facts to it (`all_u`,
  `all_mi`, `all_back`, the silent closure, `move_ok`, `vwsim_of`), and
  the transfer to `weak_sim`, `strong_sim`, `weak_bisimilar` and
  `strong_bisimilar` (`cert_weak_sim` and the others) given encodings
  complete for the mover and sound for the answerer. Not yet used by the
  tactics: the assembly for a goal, and completeness and soundness, come
  next.
- **Docs:** the technical report's section "Testing from VSCode" (a
  step-by-step guide); `doc/certificate.md` ("The theorem from the
  certificate").

**Session tally:** Optimization 3 · Refactor 1 · Tooling 1 · Docs 2 ·
New feature 1 · Bug fix 0.

## 2026-11-11 — Goals proved: `Qed` from the certificate

- **New feature (the proof of a goal,** `src/lts_proof.ml`,
  `theories/DdEnc.v` rewritten): the tactics `sim`, `bisim`, `weak_sim`
  and `weak_bisim` close their goal with a term built from Sylvan's
  certificate, checked by the kernel at `Qed`, closed under the global
  context. Every example `dune build` builds ends with `Qed` (LogTest,
  CCS with the Alternating Bit Protocol, Proc Test1 to Test3, CADP with
  one process). The term: the log's facts (`Dd_cert.write_log
  ~requests`, `Dd_read.read_proof`); the game on vectors from them
  (`src/game_proof.ml`, sub-proofs used twice shared as `let`s); each
  system's encoding (a trie of shape predicates over a trie of values);
  the answerer's relations sound (a lemma per relation and pair of
  shapes, a derivation per tuple from `Learn.derive_chain`); the mover's
  complete (a case lemma per LTS as a match on the step, small inversion
  by match predicates, coverage of the slots by the learned projections,
  a step lemma per closed term); `DdGame`'s theorem applied. The proof is
  Gallina text over named local definitions, elaborated, no engine term
  printed. No boolean of the certificate is proved by `eq_refl`: code
  lists' properties are inductive predicates (`NoWO`, `Within`,
  `Shifted`, `RoOnly`).
- **New feature (the engine, for the proof):** the plan of the logged
  check (`Learn.Plan`: the sets by node index, each relation's label,
  codes, diagram, source shape and rule chain; per mover summand its
  coverage of `z`, logged; per shape without a summand an empty image);
  the derivation of a step of a partial term along a summand's chain
  (`Learn.derive_chain`). A step that overwrites a slot it does not read
  now reads it, and a position that is no slot of the source is read as
  its 0, so that every answerer's relation can be read backwards: Proc's
  `termLTS` (Test1), whose certificate could not be logged, is proved.
- **Refactor (the tactics):** the log is always written while a tactic
  proves its goal and deleted after, unless `Set MeBi Log Mode "keep"`;
  the mode `"temporary"` (now the same as `"off"`) deleted. `MeBi Check
  Game` and `MeBi Check Qed`, test commands of the work in progress,
  deleted; the translator's `Enum` request (lists of a diagram's tuples)
  deleted, `AllT` walks diagrams instead; a duplicated copy of
  `Dd_read.read_proof` deleted.
- **Optimization (building the proof):** only the hypotheses a rewriting
  changes are abstracted again; vectors and the goal's head abbreviated by
  local definitions; each text elaborated in an environment with only the
  definitions it names (elaborating among all of them slowed every hole,
  growing with the proof); the step lemmas found by hash, not a scan;
  definitions closed in one pass. The Alternating Bit Protocol's proof:
  24s down to 5s (2.85M characters of text down to 1.36M, a 5M-node term
  down to 1.4M); its `Qed` 1.5s, 1GB.
- **Optimization (step lemmas per skeleton):** a closed term's step lemma
  is over holes at the positions no rule inspects (every rule with that
  head has a variable there, used once, reaching premises only at such
  positions, in no side condition), its steps collected from its own
  proof; a premise's target matched by pairs only binds its variables to
  projections of a computed target. CADP's process term: one lemma per
  program point instead of one per program point, state and resource
  (two processes: 4,200 lemmas before). The proof's universe constraints
  come from the elaborations and a probe of the constants built in
  OCaml, instead of typing the whole term again (which ran out of 5GB on
  two processes). Two processes then: the proof built in 104s, `Qed` in
  10s, 2.1GB at most.
- **Optimization (coverage cases per group):** the projections of a
  summand that agree on the slots the remaining hypotheses inspect share
  one elaborated lemma over the other slots' numbers, taking each step's
  witness as a hypothesis; each projection is the lemma applied to its
  numbers and to witness terms built in OCaml (membership, reads by
  `eq_refl`, the encoding by a lemma per shape). One-process CADP: 1.2s
  down to 0.55s per goal. Three processes (one summand alone learned
  21,903 projections) ran out of 7GB with one elaborated case per
  projection.
- **Bug fixes found on the way** (each made a goal fail to build, none a
  wrong proof): relations with the same tuples share a node, so relations
  are keyed by label, meta and node; a shape that is a bare slot; a
  refuted side condition needs its instance named; the order of
  inversions (a premise's label and target are defined by its step only;
  coverage last, its summand by the exact rule path; a premise built from
  slots alone is one closed step); a pattern variable bound twice; a
  pattern of a one-constructor type with a redundant default; targets
  compared in normal form; universe constraints, inferred by typing the
  term once when a type lies above `Set` (CADP).
- **Mistakes, for the record:** the first assembly abstracted each of
  about 300 local definitions by a pass over the whole term, which with
  the game's closures written out (not shared) exhausted 6GB on the
  Alternating Bit Protocol's bisimilarity; the first criterion for taking
  a premise apart by the rules (a term holding part of the shape) broke
  one-process CADP, whose summand takes such a premise as one closed
  step: the summands' chains decide now.
- **Docs:** `doc/certificate.md` (the proof of a goal, what Rocq
  computes, decisions), the technical report's new section "The proof of
  a goal" (a worked example) and its overview, commands, tests, VSCode
  and status sections; `README.md`, `CLAUDE.md`, `examples/README.md`,
  the examples' files (`Abort` to `Qed`, `MeBi Check Log` lines
  removed), `test/rocq/LogTest.v`, the help text.

**Session tally:** New feature 2 · Refactor 1 · Optimization 1 ·
Bug fix 1 · Docs 1 · Tooling 0.

## 2026-11-11 — The proof's pieces as constants; the log in memory

- **Optimization (the proof's pieces as constants,** `src/lts_proof.ml`,
  `src/user_tactics.ml`): every definition the proof makes is a constant,
  declared the way `abstract` declares a sub-proof (`Evd.push_side_effects`,
  emitted into the proof's state, exported at `Qed` as
  `<lemma>_mb_...`). Proofs (case lemmas, step lemmas, coverage groups
  and coverages, soundness per relation, encodings' lemmas) are opaque, and
  the elaborator sees only their statements (local assumptions, never
  unfolded); data are transparent, or written in place when a dozen nodes
  or fewer. A definition over the certificate's facts is abstracted over
  the facts it uses (in one pass) and named applied to them. A diagram a
  proof walks (a coverage's projections, a relation's tuples) is a datum
  built from its tuples, the same tree as the log's node. The kernel
  checks each constant once, when declared; the proof's term is the facts,
  the game, the coverage of each summand and the theorem.
  `MEBI_QED_LETS=1` keeps the former `let`s, for comparison. One-process
  CADP: `Qed` 0.11s instead of 0.29s.
- **Optimization (`AllT` proofs):** each subtree of a walked diagram is a
  `let` of the proof, named instead of repeated in its ancestors'
  arguments, and the proof is given its statement (by an identity function
  over `AllT D Q`, `D` the diagram's constant) inside those `let`s.
- **Optimization (building the proof):** registered globals and numerals
  made once and shared; labels' normal forms remembered; a target compared
  with a tuple's by conversion (lazy) instead of two normal forms; the
  facts' list arguments in an array (a list scanned by `List.nth` made
  reading the facts quadratic: 8.6s of a two-process proof, now 0.13s).
  Two processes: the file in 9.8s (the proof built in 6.1s, `Qed` 2.7s),
  0.95GB at most; it was 114s and 2.1GB.
- **Optimization (the log in memory,** the fork's `proof-log` branch:
  `lddmc_log_open(NULL)`, `lddmc_log_take`, `lddmc_log_free`;
  `Ldd.log_open_mem`, `log_close_mem`, `log_take`, `log_free`,
  `node_fields`; `Proof_log.source`, `Proof_log.iter`): unless
  `Set MeBi Log Mode "keep"`, Sylvan's records stay in its workers'
  buffers, taken a buffer at a time (each freed once read), and the nodes
  are read from Sylvan's own table, collection staying off until the
  facts are written (no table of the log's nodes in OCaml). The facts go
  from `Dd_cert.write_log` to `Dd_read.proof_reader` a line at a time
  (`Dd_cert.To_lines`): no log file, no facts file. "keep" writes the
  log's file as before, for `MeBi Check Log`.
- **Mistakes, for the record:** the first abstraction over the facts
  used `Term.mkNamedLambda` once per fact, each a pass over the whole
  proof: 15s per coverage on two processes, the hundreds of facts of a
  coverage times its size. The first `let`s for a diagram's subtrees left
  the proof's own type naming them: the kernel types a `let` by
  substituting into its body's type, which grew a subtree at each `let`
  (448s for one coverage of three processes); the type given inside the
  `let`s fixed it.
- **Optimization (a constant takes few facts):** the kernel types an
  application in time quadratic in its arguments (each argument's type
  rebuilt under the substitution of all those before:
  `Typeops.type_of_apply`, found by sampling the stuck process's stack
  under gdb), and a constant over the facts is applied to every fact it
  uses: a coverage of three processes, over thousands of memberships,
  took minutes. A coverage's memberships are now one fact, `TAll` of the
  trie of their statements (`DdEnc.tall_get`, a lookup by key as `val`
  is), built in the term from the log's membership facts, as a `let`
  between the facts and the body (made at its own depth: substituting or
  lifting it would copy each shared subtrie). Two processes: the shapes'
  proof 2.5s to under 0.01s.
- **Optimization (memberships, `DdTree.Sib`, `in_sib_d`):** a membership
  is one fact per level: the sibling holding the next value, reached along
  its list by `Sib` facts that do not name the vector, so every vector
  scanning a list shares them; and a membership's steps from a node are
  shared by the vectors meeting there (Sylvan shares subtrees). Three
  processes: 11.1M facts in the proof, 9.4M of them steps along sibling
  lists, down to 1.1M.
- **Optimization (building the proof):** a witness's tuple and codes
  written in place (they named thousands of constants); a tuple's
  membership found by hash (a list scanned at each witness: 107s of a
  three-process coverage); a statement's diagram given by its fact once
  per statement; lists of numbers on shared suffixes, one term per de
  Bruijn index (two processes: the facts' `let`s 18MB to 4MB); the heap
  kept closer to what is live while the proof is built (`space_overhead`
  80, a compaction after a large coverage); after a large decision
  (over a million pairs), Sylvan's tables shrunk and the heap compacted
  before `Qed`, and `space_overhead` 60 until the next tactic.
- **Optimization (memberships checked apart):** the translator writes,
  after the proof's main chunk, a chunk per relation diagram the packs
  name (its nodes only: a transparent constant `RR`, its nodes as
  `let`s) and a chunk per pack (its membership facts, its own nodes);
  each pack is an opaque constant over its chunk, given the statement
  over the `RR` constants (compared with its own once, by the trie's
  arguments), checked while the proof is built. The main chunk asks only
  for the memberships a constant still uses. Three processes: the main
  term 1.12M facts down to 829K.
- **Resolved (`OUTSTANDING.md`, "the proof's cost on large goals"):**
  every example ends with `Qed`, `examples/CADP/ThreeProcesses.v`
  included (from `Abort` and `MeBi Check Log`). Measured (11 November
  2026): two processes, the file in about 6s (the proof built in 3.3s,
  `Qed` 1.7s), 0.83GB at most (114s and 2.1GB before the session); three
  processes, the proof built in 124s (59s of it the packs), `Qed` 144s,
  the file in 293s, 6.6GB at most, the compiled file 113MB.
  `bench/prooflog.py` leaves 4GB free (was 1GB), at most 7GB.
- **Mistakes, for the record:** a guess (the long application's cost in
  the substitution of dependent types) was wrong until a stack sample
  showed the kernel's own; a compaction after every coverage, then a
  collection after every goal, made the small examples twice as slow
  until limited to large ones; the diagnostics left on (heap walks per
  goal) did the same. A run capped at 3GB with 5.5GB available crashed
  the machine when a browser was opened meanwhile: heavy runs now leave
  4 to 5GB free, under a watcher that stops them below 2GB.
- **Tooling:** the Sylvan fork's in-memory log committed on its local
  branch `proof-log` (not pushed), the submodule pointer moved;
  `test/ldd_tests.exe` checks the log in memory (30 tests);
  `test/cert_tool.exe` and `MeBi Check Log` read a file source; under
  `MEBI_CERT_STATS`, the kernel's time per kind of constant, a slow
  constant's declaration, a coverage's parts, the facts per lemma.
- **Docs:** `CLAUDE.md`, `README.md`, `doc/certificate.md`,
  `doc/limitations.md`, `lib/ldd/README.md`, `bench/README.md`,
  `examples/README.md`, the help text; the report's section "The proof
  of a goal" rewritten to be read without context (what it needs from
  Rocq and from earlier sections defined first; how the proof is put
  together, step by step; each measure with a small example and its
  measured effect), and its overview, commands, proof log, tests and
  status sections.

**Session tally:** Optimization 8 · Tooling 1 · Docs 1.

## 2026-11-12 — How low three processes can go; a relation read the wrong way

- **Measured (three-process CADP, memory sampled every 2s):** the proof
  is built with 2.9GB at most; 1.6GB when `Qed` starts; the kernel's
  check of the term grows it steadily to 6GB over 145s (about 5KB per
  fact of the term's 829K: the term, its hash-consed copy, the typing of
  1.16M nested `let`s); writing the compiled file takes 2s more, 6.6GB at
  most.
- **Mistake, corrected:** a first conclusion that 3GB was out of reach
  assumed that a piece's statement must name the pairs' diagram by a
  constant per node; nodes can be named by number instead.
- **New feature (`MeBi Prove id : goal.`, a command):** decides and
  proves a goal given as a statement, as the tactics do, and declares the
  lemma `id`. Its proof is in pieces, each a global constant the kernel
  checks as it is declared (`Dd_read.finish_pieces`): the main chunk's
  nodes in a trie per height (a node named `val D0 G_h p`, looked up by
  number: accepted as the same kind of conversion as `val`), its facts in
  segments of 20,000, each a trie of the statements others need and an
  opaque `TAll` of it; the lemma's term names only the pieces. Three
  processes (`examples/CADP/ThreeProcesses.v` now uses it): decided and
  proved in about 280s, 2.6GB at most, the lemma itself checked in 0.4s,
  closed under the global context; two processes: 5.8s, 0.72GB.
  `LogTest.v` proves a goal with it, prints its assumptions, and checks
  that it refuses a goal that does not hold.
- **The same from a tactic: not possible, measured.** Declared as global
  constants, the pieces were gone at `Qed` (Rocq discards a tactic's
  global declarations: "Constant ... does not appear in the
  environment"); declared as side effects (as `abstract`), each keeps
  its typed form (`HConstr`, about three times the term) until `Qed`:
  three processes went from 1.65GB to 4.1GB while declaring them, and
  past 7GB at `Qed`. A command to run inside a proof (`MeBi Certify`,
  tried, removed) declared them, but Rocq replays a side-effecting
  command once the proof ends, so it did the work twice. The tactics
  keep the proof as one term with its constants as side effects (6.6GB
  for three processes). **Checked in Rocq 9.3's source, when the
  conclusion was questioned** (the first explanation was inferred from
  the failures): `Qed` declares the lemma in the state from before the
  proof (`stm/stm.ml`); a private constant keeps its `HConstr` until
  export (`kernel/safe_typing.ml`, `seff_body`); a command inside a
  proof is run in it and again on the main branch, outside it, at `Qed`
  (the docs had said "replayed once the proof ends", corrected);
  Program's obligations live in the commands' program state, out of a
  tactic's reach.
- **Optimization:** a large certificate's log and Sylvan's tables are
  given back as soon as the facts are written (not after the proof is
  built): three processes, 3.2GB to 2.9GB while the proof is built.
- **Bug fix (`Learn`, the plan's `rinfo`):** the mover's relations and
  the answerer's inverse relations were recorded under one key (label,
  meta, node), and Sylvan stores equal sets once: the inverse of a
  reassociation's relation is the other reassociation's relation. In a
  bisimilarity, where each system is both mover and answerer, an inverse
  entry could be replaced by a forward one, with the other rule's chain:
  its tuples were then read the wrong way ("a tuple with no
  derivation"). `rinfo` now holds the answerer's relations only, the
  only ones the proof reads it for.
- **Proc Test4 (`TermTests.v`, four processes):** with that fix, its
  proof gets past soundness, but makes over 2.3M facts (85 answerer
  relations, 94 coverages of up to 1,992 projections) for 9,720 states:
  the reorderings multiply each class by its 120 bracketings and orders.
  It stays `Abort` until proofs up to the congruence (`doc/upto.md`).
- **Tooling (work files):** the watcher logs the job's memory every 2s;
  a tuple with no derivation names its chain, source, label, expected
  target and what was derived.
- **Docs:** `MeBi Prove` in the README's commands, CLAUDE.md (the
  pipeline, verification), the examples' and bench's READMEs,
  `doc/limitations.md` and `doc/certificate.md` ("Large goals", and what
  Rocq computes: a node by its number); the report gains "Large goals:
  the command `MeBi Prove`" (why, use, the nodes by height with an
  example, the segments, cost, why a tactic cannot), a cost row, the
  command, the test, and a decision to double-check; its status ("Done",
  "Next") rewritten to the current state (it still described the
  certificate before goals reached `Qed`). The report's PDF is
  gitignored, kept so: CLAUDE.md and the `.gitignore` comment said it was
  tracked, now corrected.

**Session tally:** Bug fix 1 · Optimization 1 · New feature 1 · Tooling 1 ·
Docs 1.

## 2026-11-13 — Proofs up to reordering (route B): theory and plan

- **Docs:** route B chosen for proofs up to the structural congruence
  (`doc/upto.md`): the rules shown to commute with the reorderings, so
  that the game runs on one representative per class; representatives
  by saturating each shape under the reorderings, value-free (each item
  a shape and a slot map, with its path), the least of a state's items
  its representative, and sorting for larger classes (not to be pushed
  far: larger systems from compositionality). The staged plan is in
  `doc/upto.md`, "Route B: implementation plan".
- **Mistake, corrected before any code:** a first answer said Test4's
  120 states per class were 120 skeletons, so a representative skeleton
  would merge them; Proc's skeletons are its bracketings only (9 in
  all), and the 24 orders are values permuted within a skeleton, so a
  representative needs a canonical placement of the values too.
- **Found while designing:** Proc's `do_par_end` (`cprc tend | cprc tend
  -> cprc tend`) does not commute with associativity, so the
  commutation lemmas range over the slot values that can occur (closed
  under the slots' steps), which excludes `tend` in Test4.
- **New feature, part 1 (theory): `theories/DdUpto.v`.** `Cong`,
  `cong_equiv` (a one-step relation with inverse paths and commuting
  with the steps gives a strong bisimulation); the game answered into a
  property (`VWSimP`, `NormZ`: the vectors that normalise into `z`)
  with its toolkit; `cert_weak_sim_upto` and `cert_weak_bisim_upto`
  (DdGame's completeness and soundness of the encodings unchanged, plus
  each normalisation step sound). Both closed under the global context.

- **Theory, revised the same day:** an answer may interleave
  normalisation steps with the answerer's silent steps (`VWSimN`,
  `TausN`), because the answerer's relations are learned from
  representatives only, so an answer must come back to representatives
  as it goes; a real answer is carried along the congruence at each
  normalisation (`taus_left_upto`, `taus_right_upto`). Closed under the
  global context.
- **New feature, part 2 (engine, behind `MEBI_UPTO=1`, built, not yet
  run):** reordering summands (`Learn.is_reordering`); per-shape
  saturation under them, value-free, with paths and a bound
  (`Learn.saturate`, `MEBI_UPTO_BOUND`); a vector's representative, the
  least of its items applied to its values (`Learn.represent`);
  exploration from representatives, each target normalised, the
  one-step targets kept (`explored.ext`); the game from the
  representatives' pairs, over the wider universe, the normalisation
  relations among the answer's silent steps, logged backwards (label
  -1, chain `<norm>`); the plan gains each side's normalisation tuples
  with their items, and the start's exact vector (`Plan.pnorm`,
  `pstart_v`). A dynamic summand turns it off for that system.

- **Engine checked (development runs, allowed until route B's stage 4):**
  the same verdicts with and without up-to on four goals (two
  bisimilar, one not, one simulation); states 18 -> 9 (two processes),
  324 -> 27 (three), 9,720 -> 81 (Test4). A first comparison was wrong:
  `MEBI_UPTO=` (empty) still sets the variable, so both runs had up-to.
- **New feature, part 3 (proof, the congruence): `upto_cong`
  (`src/lts_proof.ml`).** `Ro` (a disjunction over the slots' numbers),
  its eliminator, inverse paths, commutation lemmas by the
  small-inversion engine, and `Equiv`, all checked by the kernel for
  Test4 (31 reorderings, 18s, 1.1GB). The first version quantified over
  values with a set `V` and split it where a rule needed a slot's
  constructor: 40-85K characters per lemma for three processes and 68s
  for one side of Test4; over numbers, the existing refutation
  (`ns_def`) and a closure lemma (`Vn`) replace it.

- **New feature, part 4 (proof, the rest): Test4 proved up to
  reordering.** Item lemmas (each by its path, `item_lemma`);
  normalisation soundness by `soundness_rel` with the relation `Cong
  Ro` and the items as the tuples' proofs (`norm_pf`, `forall_ok`,
  `soundNL_of`); the game with the normalisation lists
  (`game_proof.ml`, `~upto`); the start's normalisation steps
  (`normz_l`, `normz_r`); `cert_weak_sim_upto` / `cert_weak_bisim_upto`
  applied. With `MEBI_UPTO=1`: two processes, `Qed` in 1.3s; three,
  6.2s (86K facts); Test4's `p` and `q` (`weak_bisimilar`), `Qed` in
  52.5s, 3.85GB at most (593K facts), each closed under the global
  context. Before: over 2.3M facts, `Abort`.

- **New feature, part 5: up to reordering on by default** where it
  applies (weak goals, an LTS with reorderings, no dynamic summand;
  `MEBI_UPTO=off`); a proof up to it that cannot be built is replaced by
  one without, with a notice (`Learn.upto_off`, the tactic's retry). The
  full build passes with it: Proc Test3's 14 goals use it; Test1, Test2
  and CCS have dynamic summands and CADP no reorderings, so they do
  not. Two-process CADP unchanged (5.8s, 0.83GB).
- **Examples:** Test4's `TermTests.v` now proves `p ≈ q`, `q ≈ r` and
  `p ≈ r` by `MeBi Prove` up to reordering (the file in 493s, 3.3GB;
  by the tactic, one goal 52s, 3.9GB); its six `Abort`s and log checks
  removed (bisimilarity is symmetric). Not in the default build.
- **Docs:** `doc/upto.md` (status, stages), `doc/limitations.md`,
  `OUTSTANDING.md` (up to reordering resolved; what is left of it),
  CLAUDE.md, README, the examples' README and `dune` comment; the report
  gains "Proofs up to reordering" (`sections/18-upto.tex`: the terms,
  why representatives alone are unsound, the engine, a worked example,
  the theorem, the proof, cost, limits), and its status, options,
  tests and optimisations sections are brought up to date. `loader.v`
  requires `DdUpto`.
- **Mistake:** a rebuild of `@install` alone after changing `loader.v`
  left the examples' compiled files stale, and the bench failed on
  "inconsistent assumptions" until a full build.

- **Bug fix (CI, Rocq 9.2):** CI builds with Rocq 9.2.0 (the lock), and
  only 9.3 had been built here: `Environ.push_named` takes a variable's
  kind in 9.3 only, and `Evd.collapse_sort_variables` its
  `~only_above_prop` flag in 9.3 only (`src/lts_proof.ml`). Both now go
  through `Rocq_utils.push_var` and `Rocq_utils.collapse_sorts`, under
  cppo. Every Rocq function the plugin calls was then compared between
  9.2.0's interfaces (fetched from its tag) and 9.3's: no other
  difference that a call depends on (`Evd.from_env` and
  `Evd.minimize_universes` differ only in optional arguments not given).
  Still not compiled with 9.2 here: CI is the check.

**Session tally:** New feature 5 · Bug fix 1 · Docs 2.
