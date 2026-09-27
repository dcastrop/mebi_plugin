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

; (license LICENSE)

; (documentation https://url/to/documentation)

(version dev)

(package
 (name rocq-mebi)
 (synopsis "Automating bisimilarity proofs in Rocq")
 (description
  "MeBi (Mechanised Bisimilarities) builds labelled transition systems and finite state machines from Rocq inductive relations, checks (weak) bisimilarity between them, and drives interactive tactic-based proof search for bisimilarity goals, following the methods in Sangiorgi and Rutten's \"Advanced Topics in Bisimulation and Coinduction\", Section 3.2.2.")
 ; Bounds matter here: the plugin links against rocq-runtime's OCaml API, which
 ; breaks across Rocq *minor* versions. Exact versions are pinned separately in
 ; rocq-mebi.opam.locked; these are the ranges the source is known to compile in.
 (depends
  (ocaml
   (>= 4.14))
  (rocq-core
   (and
    (>= 9.2)
    (< 9.3)))
  ; theories/dune declares (theories Stdlib) and the .v files `From Stdlib Require`
  (rocq-stdlib
   (>= 9.1))
  ; 3.x exposes Yojson.t; 2.x requires Yojson.Safe.t
  (yojson
   (>= 3.0))
  (benchmark
   (>= 1.6))
  ; Editor/formatting tooling: installed by `opam install . --with-dev-setup`,
  ; kept out of the build closure. ocamlformat is exact because its output
  ; changes between releases (see the `version` field in .ocamlformat).
  (ocaml-lsp-server
   :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (vsrocq-language-server
   :with-dev-setup))
 (tags
  (bisimulation coinduction "process algebra" "proof automation" rocq)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
