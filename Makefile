SETUP = ocaml setup.ml -quiet
CONFIGURE = ./configure
PIQI=piqi
OCI=ocp-indent

build: setup.data setup.ml
	$(SETUP) -build $(BAPBUILDFLAGS)

.PHONY: doc
doc:
	ocaml bapdoc.ml

test: setup.data build
	$(SETUP) -test $(BAPTESTFLAGS)

all: setup.data
	$(SETUP) -all $(BAPALLFLAGS)

install: setup.data
	$(SETUP) -install $(BAPINSTALLFLAGS)
	sh tools/build_plugins.sh

uninstall: setup.data
	$(SETUP) -uninstall $(BAPUNINSTALLFLAGS)

reinstall: setup.data
	make uninstall
	make install

clean: setup.data
	$(SETUP) -clean $(BAPCLEANFLAGS)

distclean: setup.data
	$(SETUP) -distclean $(BAPDISTCLEANFLAGS)

setup.data: *.in _oasis
	$(CONFIGURE) $(BAPCONFIGUREFLAGS)

configure:
	$(CONFIGURE) $(BAPCONFIGUREFLAGS)

.PHONY: configure
.PHONY: clean disclean reinstall

.PHONY: check
check: check-piqi check-ocp-indent

.PHONY: check-piqi
check-piqi: *.piqi
	for piqifile in $^; do $(PIQI) check --strict $$piqifile; done

.PHONY: check-ocp-indent
check-ocp-indent: *.ml
	for mlfile in $^; do $(OCI) $$mlfile | diff - $$mlfile; done

.PHONY: auto-ocp-indent
auto-ocp-indent: *.ml
	for mlfile in $^; do $(OCI) -i $$mlfile; done
