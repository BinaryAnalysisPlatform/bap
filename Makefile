SETUP = ocaml setup.ml
CONFIGURE = ./configure
PIQI=piqi
OCI=ocp-indent

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: setup.data
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: setup.data
	$(SETUP) -clean $(CLEANFLAGS)

distclean: setup.data
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(CONFIGURE) $(CONFIGUREFLAGS)

configure:
	$(CONFIGURE) $(CONFIGUREFLAGS)


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
