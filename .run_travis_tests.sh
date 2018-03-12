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
