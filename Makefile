SETUP = ocaml setup.ml -quiet
PIQI=piqi
OCI=ocp-indent

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
