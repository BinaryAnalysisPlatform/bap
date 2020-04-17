TASK=$1

mkdir -p $HOME/.opam/

comp=`ls $HOME/save_opam`

cp -r $HOME/save_opam/$comp $HOME/.opam/
cp $HOME/save_opam/$comp/config $HOME/.opam/

export PATH=$HOME/.opam/$comp/bin:$PATH

bap --version

if [ "$TASK" == "checks" ]; then
    bash -exc 'make check'
fi

if [ "$TASK" == "unit_tests" ]; then
    bap_run_tests
    bap_future_tests
    bap_x86_tests
    bap_powerpc_tests
    bap_piqi_tests
    bap_traces_tests
    bap_stub_resolver_tests
fi

if [ "$TASK" == "veri" ]; then
    bash -exc 'make veri'
fi
