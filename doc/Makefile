.PHONY: doc argot


doc: argot
	../bapdoc.native 2>bapdoc.log


argot:
	git clone https://github.com/ivg/argot.git
	cd argot; git checkout 1.2-devel
	cd argot && sh configure
	make -C argot all
	make -C argot install
	rm -rf argot
