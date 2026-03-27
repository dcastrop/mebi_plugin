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
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml rocq-core yojson benchmark)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
