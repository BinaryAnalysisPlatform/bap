TASK=$1

eval `opam config env`

comp=`opam config var switch`

cp -alu $HOME/save_opam/bin/* $HOME/.opam/$comp/bin/
cp -alu $HOME/save_opam/share/* $HOME/.opam/$comp/share/
cp -alu $HOME/save_opam/lib/* $HOME/.opam/$comp/lib/

bap --version
bap-byteweight update

if [ "$TASK" == "checks" ]; then
    bash -exc 'make check'
fi

if [ "$TASK" == "unit_tests" ]; then
    bap_run_tests
fi
