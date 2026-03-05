# FIXME: for updating to rocq-9.1

ifeq "$(COQBIN)" ""
#   COQBIN=$(dir $(shell which rocq top))/
  COQBIN=$(dir $(shell which coqtop))/
endif

%: Makefile.rocq
# %: Makefile.coq

Makefile.rocq: _CoqProject
	$(COQBIN)rocq makefile -f _CoqProject -o Makefile.rocq

# Makefile.coq: _CoqProject
# 	$(COQBIN)coq_makefile -f _CoqProject -o Makefile.coq

tests: all
	@$(MAKE) -C tests -s clean
	@$(MAKE) -C tests -s all

-include Makefile.rocq
# -include Makefile.coq
