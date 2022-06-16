ifeq (is$(BAP_DEBUG), $(filter is$(BAP_DEBUG),is is0 isno isdisable isfalse))
SETUP = ./setup.exe -quiet
else
SETUP = ./setup.exe
endif

build: setup.ml
	$(SETUP) -build $(BAPBUILDFLAGS)

.PHONY: doc
doc:
	@ocamlfind ocamlopt -linkpkg -package bap-plugins,core_kernel,core_kernel.caml_unix tools/bapdoc.ml -o bapdoc.native
	make -C doc

all:
	$(SETUP) -all $(BAPALLFLAGS)

.PHONY: plugins
install-plugins:
	sh tools/build_plugins.sh
	if [ -f ./postinstall.native ]; then ./postinstall.native; fi
	if [ -f ./postinstall.byte ]; then ./postinstall.byte; fi
	if [ -f ./postinstall ]; then ./postinstall; fi

install-libs:
	$(SETUP) -install $(BAPINSTALLFLAGS)

reinstall-libs:
	$(SETUP) -reinstall $(BAPINSTALLFLAGS)

uninstall:
	$(SETUP) -uninstall $(BAPUNINSTALLFLAGS)

install: install-libs install-plugins
reinstall: reinstall-libs install-plugins

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

testsuite:
	git clone https://github.com/BinaryAnalysisPlatform/bap-testsuite.git testsuite

check: testsuite
	make REVISION=ddb5e33ce1c -C testsuite

.PHONY: indent check-style status-clean

indent:
	sh tools/ocp-indent-all.sh

status-clean:
	git diff --quiet --exit-code

check-style: status-clean indent
	git diff --exit-code
