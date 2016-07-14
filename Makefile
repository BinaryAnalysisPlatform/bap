SETUP = ocaml setup.ml -quiet

build: setup.ml
	$(SETUP) -build $(BAPBUILDFLAGS)

.PHONY: doc
doc:
	ocaml bapdoc.ml

test: build
	$(SETUP) -test $(BAPTESTFLAGS)

all:
	$(SETUP) -all $(BAPALLFLAGS)

install:
	$(SETUP) -install $(BAPINSTALLFLAGS)
	sh tools/build_plugins.sh

uninstall:
	$(SETUP) -uninstall $(BAPUNINSTALLFLAGS)

reinstall:
	make uninstall
	make install

clean:
	$(SETUP) -clean $(BAPCLEANFLAGS)

distclean:
	$(SETUP) -distclean $(BAPDISTCLEANFLAGS)

.PHONY: clean disclean reinstall

.PHONY: check
check:
	if [ -d .git ]; then git submodule init; git submodule update; fi
	make -C testsuite
