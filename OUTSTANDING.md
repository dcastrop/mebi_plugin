# Outstanding

The index of what is still open about `mebi_plugin`: one line per item,
saying where its detail is.
- **Detail** lives in `doc/` (or in the example's own files), never here.
- **History** lives in `ASSISTED-CHANGES.md`.

This file should **only shrink**. When an item is resolved, delete its
line, and record the resolution in `ASSISTED-CHANGES.md`. When something
new is found, add one line, and put its detail in the right `doc/` file.

## The certificate (in order)

- **Escaped summands and premises the rules cannot evaluate:** the proof
  refuses them ([`doc/limitations.md`](doc/limitations.md)).
- **The optimisations on the certificate:** confluence reduction, static
  confluence, movers, compositional proofs
  ([`doc/optimisations.md`](doc/optimisations.md)).
- **Up to reordering, what is left:** sorting for classes past the
  bound, and fewer facts for the normalisation closure
  ([`doc/upto.md`](doc/upto.md), stage 4).

## Decisions to double-check

- **Technical decisions taken as the work went**, each with its files and
  the alternative: the technical report, "Status and what comes next"
  (`doc/report/sections/10-status.tex`).

## CADP, the running example

- **Any number of processes**, from user-annotated `P <~ I` and
  `I || P <~ I`: to design ([`doc/future-work.md`](doc/future-work.md)).

## Limits of the tool

[`doc/limitations.md`](doc/limitations.md): the proof's cost; escaped
summands; LTS shapes; steps the rules do not determine; memory.
