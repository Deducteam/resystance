DUNE ?= dune

.PHONY: bin install doc uninstall clean test
bin:
	$(DUNE) build

install:
	$(DUNE) install

doc:
	$(DUNE) build @doc

tests: bin
	_build/install/default/bin/dkritic tests/critical_pairs.lp

uninstall:
	$(DUNE) uninstall

clean:
	-rm -r _build/
