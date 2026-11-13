# Limitations

What the tool cannot do today. The plan for each is in
[`certificate.md`](certificate.md) or [`future-work.md`](future-work.md).

- **The proof's cost grows with the certificate:** its facts are checked
  in one term at `Qed` (the memberships apart, as constants), about 5KB
  of the kernel's memory each. Two-process CADP (12,027 pairs): the file
  in about 6s, 0.83GB at most. Three-process CADP (7.9M pairs, 829K
  facts in the term): the proof built in 124s (2.9GB at most), `Qed`
  144s, during which memory grows from 1.6GB to 6.6GB. The command
  `MeBi Prove id : goal.` checks the proof in pieces, each a global
  constant the kernel checks as it is declared: three processes in
  about 280s, 2.6GB at most. A tactic cannot: its global declarations
  are discarded, and the constants it makes as side effects keep their
  typed form (about three times the term) until `Qed`
  (`doc/certificate.md`, "Large goals").
- **Proofs up to reordering** (`doc/upto.md`) apply to weak goals only,
  for an LTS whose rules hold reorderings and whose summands are not
  dynamic, with classes of at most 50,000 items (`MEBI_UPTO_BOUND`;
  sorting for larger ones is not built). The commutation proofs follow
  rules with one premise; a rule that needs a slot to start with a
  constructor one of its values starts with, or a rule that does not
  commute with a reordering, has no proof up to reordering: the goal is
  then proved without it, with a notice. Test4 (four processes, 81
  states up to reordering) still makes 593K facts, mostly from the
  normalisation relations, which read and write every position.
- **Summands with escaped projections** (learned per whole state, when
  the interpreter needed more than the summand reads) and **premises the
  rules cannot evaluate** (`Opaque`): the proof refuses them, with the
  summand or the premise named.
- **LTSs** must be inductives `M -> option A -> M -> Prop` without
  parameters; a goal must be between closed terms.
- **A step the rules do not determine** (a premise the interpreter cannot
  follow: a source that is not a pattern it can match, something opaque, a
  premise stepping from its own source without end) is an error.
- **Infinitely many reachable states** cannot be decided: exploration
  enumerates both systems from the start states, symbolically.
- **Divergence:** `weak_sim` and `weak_bisimilar` ignore it, as their
  definitions do.
- **Memory:** a chunk's check needs about twice its term (Rocq's kernel
  hash-conses it); one root of the certificate can give 300,000 facts
  (three-process CADP: 0.83GB at most). Heavy runs need an OS memory cap
  (CLAUDE.md). Sylvan exits the process when its table is full.
