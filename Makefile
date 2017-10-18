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

test: build
ifeq ("$(BAP_RUN_TEST)","true")
	$(SETUP) -test $(BAPTESTFLAGS)
endif

check:
ifeq ("$(BAP_RUN_CHECK)","true")
	if [ -d .git ]; then git submodule init; git submodule update; 	fi
	make -C testsuite
endif
