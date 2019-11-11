ifeq (is$(BAP_DEBUG), $(filter is$(BAP_DEBUG),is is0 isno isdisable isfalse))
SETUP = ./setup.exe -quiet
else
SETUP = ./setup.exe
endif

build: setup.ml
	$(SETUP) -build $(BAPBUILDFLAGS)

.PHONY: doc
doc:
	@ocamlbuild -pkg bap bapdoc.native
	make -C doc

all:
	$(SETUP) -all $(BAPALLFLAGS)

install:
	$(SETUP) -install $(BAPINSTALLFLAGS)
	sh tools/build_plugins.sh
	[ -f ./postinstall.native ] && ./postinstall.native

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

.PHONY: test

.PHONY: check

.PHONY: veri

test: build
	$(SETUP) -test $(BAPTESTFLAGS)

check:
	if [ -d .git ]; then git submodule init; git submodule update; 	fi
	make -C testsuite

veri:
	if [ -d .git ]; then git submodule init; git submodule update; 	fi
	make -C testsuite veri
