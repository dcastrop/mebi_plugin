# COQBIN is deliberately left unset: `rocq` comes from the local opam switch on PATH
# (direnv / `opam env`). Override with `make COQBIN=/path/to/bin` for a specific
# installation. Makefile.rocq.conf appends the trailing slash itself, so COQBIN must
# NOT end in one.

ROCQ = $(if $(COQBIN),$(COQBIN)/,)rocq

%: Makefile.rocq

Makefile.rocq: _CoqProject
	$(ROCQ) makefile -f _CoqProject -o Makefile.rocq

# Switching back to the dune build requires removing the in-tree artifacts this
# rocq-makefile build leaves behind (src/g_mebi.ml, *.vo, *.glob, *.cm*); dune
# refuses to build over files it also generates.
.PHONY: dune
dune: clean
	dune build

tests: all
	@$(MAKE) -C tests -s clean
	@$(MAKE) -C tests -s all

-include Makefile.rocq
