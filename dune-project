(lang dune 3.20)

;  FIXME: whenevr dune 3.21 and rocq updte ??
; (lang dune 3.21)

; (using rocq 0.11)
; (using rocq-core 0.8)

; (lang dune 3.20)

(using coq 0.10)

; https://dune.readthedocs.io/en/stable/rocq.html#rocq-plugin-project

; FIXME: for updating to rocq-9.1

(name rocq-mebi)

; REFERENCE: https://github.com/rocq-prover/rocq/blob/4dc426ced0bcfe372784e5a936802430c18593bf/dune-project
; ALSO: https://dune.readthedocs.io/en/stable/rocq.html

; (using coq 0.8)

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
 (depends ocaml rocq-core bisim)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
