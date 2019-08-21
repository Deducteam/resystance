DUNE ?= dune

.PHONY: bin install uninstall clean
bin:
	dune build

install:
	dune install

doc:
	dune build @doc

uninstall:
	dune uninstall

clean:
	-rm -r _build/
