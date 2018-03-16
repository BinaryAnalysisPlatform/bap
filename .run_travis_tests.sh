TASK=$1

eval `opam config env`

comp=`opam config var switch`

cp -r $HOME/save_opam/bin/* $HOME/.opam/$comp/bin/
cp -r $HOME/save_opam/share/* $HOME/.opam/$comp/share/
cp -r $HOME/save_opam/lib/* $HOME/.opam/$comp/lib/

bap --version

if [ "$TASK" == "checks" ]; then
    bash -exc 'make check'
fi

if [ "$TASK" == "unit_tests" ]; then
    bap_run_tests
fi

if [ "$TASK" == "veri" ]; then
    opam install pcre textutils -y
    git clone https://github.com/BinaryAnalysisPlatform/bap-veri.git
    cd bap-veri
    oasis setup
    ./configure --prefix=`opam config var prefix`
    make
    make reinstall
    cd ../
    bash -exc 'make veri'
fi
