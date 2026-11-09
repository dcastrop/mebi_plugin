# Future work

Work that is understood well enough to describe, but not under way.
`OUTSTANDING.md` lists each item in one line. The certificate's own next
steps are in [`certificate.md`](certificate.md); the optimisations, each
sketched on the certificate, in [`optimisations.md`](optimisations.md).

## Any number of processes

**Kind:** *capability* (agreed 2026-11-10, to design next). From
user-annotated facts `P <~ I` and `I || P <~ I` (or `P || I <~ I`), each
decided and certified by the plugin as a finite check, conclude
`P || ... || P <~ I` for all n, by a generic theorem (precongruence of
`||` for weak simulation, and transitivity) and a short proof or a tactic
(network invariants: Wolper and Lovinfosse 1989; Kurshan and McMillan
1989; in general undecidable, Apt and Kozen 1986, hence the user's `I`).
- **Without shared state** the precongruence follows from the rules'
  format (de Simone 1985; Bloom 1995; van Glabbeek 2011), which the plugin
  could detect, as it detects context rules.
- **With shared state (CADP)** a group of processes cannot be replaced by
  the ENTER/LEAVE specification: the others read and write the group's
  part of the memory. A specification `I_k` for "k processes and their
  part of the memory, seen by one more process" (the abstract queue, plus
  exactly the memory the outside process reads or writes), with
  `P ≤ I_1` and `I_k ∥ P ≤ I_{k+1} ≅ I_k` as finite checks, needs the
  theorem that `∥` over a shared memory preserves weak simulation when
  each side tolerates the other's writes (rely-guarantee simulation,
  Liang, Feng and Fu 2012).

## Saturation

Sylvan's saturation works on fixed relations; the engine learns its
relations as it explores, so it uses chained exploration instead (each
summand applied to the round's frontier grown by the summands before it).
To revisit for a model whose frontier diagrams grow large.

## Smaller possible work

- **Compare with CADP and mCRL2**: verdicts and cost on the same models.
  Neither tool is installed; CADP needs a licence.
- **Rocq 9.3 in CI.** The source compiles against 9.2 and 9.3 (cppo's
  `#if ROCQ_VERSION`); CI builds only the locked 9.2 switch; a 9.3 job
  would need its own, unlocked switch.

## Set aside (deliberately)

- **Divergence-sensitive equivalences**: out of scope for the current
  definitions.
- **Module lists** (dune's `(modules ...)`) are still maintained by
  hand. A drift is caught by `scripts/check_module_lists.py` (CI) rather
  than prevented.
- **odoc warnings** naming stdlib or Rocq exceptions in `@raise` tags,
  which odoc cannot resolve. Accepted.
