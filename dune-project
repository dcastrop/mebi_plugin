(lang dune 3.21)

(using rocq 0.11)

; https://dune.readthedocs.io/en/stable/rocq.html#rocq-plugin-project

(name rocq-mebi)

; REFERENCE: https://github.com/rocq-prover/rocq/blob/4dc426ced0bcfe372784e5a936802430c18593bf/dune-project
; ALSO: https://dune.readthedocs.io/en/stable/rocq.html

(formatting
 (enabled_for ocaml))

(generate_opam_files true)

(source
 (github dcastrop/mebi_plugin))

(authors "David Castro-Perez" "Jonah Pears")

(maintainers "David Castro-Perez")

(license "GPL-3.0-or-later")

; (documentation https://url/to/documentation)

(version dev)

(package
 (name rocq-mebi)
 (synopsis "Automating bisimilarity proofs in Rocq")
 (description
  "MeBi (Mechanised Bisimilarities) proves strong and weak simulation and bisimilarity goals between labelled transition systems given as Rocq inductive relations. It decides each goal symbolically, on decision diagrams (Sylvan), and closes it with a certificate that a Gallina checker, proved sound once, checks by computation.")
 ; Bounds matter here: the plugin links against rocq-runtime's OCaml API, which
 ; breaks across Rocq *minor* versions. Exact versions are pinned separately in
 ; rocq-mebi.opam.locked; these are the ranges the source is known to compile in.
 (depends
  (ocaml
   (>= 4.14))
  ; 9.2 and 9.3 differ in a few functions, under cppo's #if ROCQ_VERSION.
  (rocq-core
   (and
    (>= 9.2)
    (< 9.4)))
  ; theories/dune declares (theories Stdlib) and the .v files `From Stdlib Require`
  (rocq-stdlib
   (>= 9.1))
  ; Preprocesses the modules whose code differs by Rocq version: the
  ; (preprocess ...) in lib/rocq_tools/dune and lib/reify/dune.
  (cppo
   (and
    :build
    (>= 1.6)))
  ; Documentation tooling (`dune build @doc`; `odig` browses it locally).
  (odoc
   (or :with-doc :with-dev-setup))
  (odig :with-dev-setup)
  ; Editor/formatting tooling: installed by `opam install . --with-dev-setup`,
  ; kept out of the build closure. ocamlformat is exact because its output
  ; changes between releases (see the `version` field in .ocamlformat).
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (vsrocq-language-server :with-dev-setup))
 ; Optional: Rocq built with its native compiler (no proof needs it).
 ; rocq-native.2 goes with Rocq 9.2, rocq-native.3 with 9.3.
 (depopts rocq-native)
 (tags
  (bisimulation coinduction "process algebra" "proof automation" rocq)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
