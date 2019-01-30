TASK=$1

mkdir -p $HOME/.opam/

comp=`ls $HOME/save_opam`

cp -r $HOME/save_opam/$comp $HOME/.opam/
export PATH=$HOME/.opam/$comp/bin:$PATH

bap --version

if [ "$TASK" == "checks" ]; then
    bash -exc 'make check'
fi

if [ "$TASK" == "unit_tests" ]; then
    bap_run_tests
fi

if [ "$TASK" == "veri" ]; then
    bash -exc 'make veri'
fi
