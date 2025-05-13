(lang dune 3.16)

(using coq 0.9)

(name coq-mebi)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name coq-mebi)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml base coq-core bisim)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
