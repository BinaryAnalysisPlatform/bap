export OPAMYES=true
export PACKAGE=bap
export TESTS=false

if [ "$WITH_BUILD_CACHE" == "true" ]; then
    export POST_INSTALL_HOOK='
rm -rf bap-veri
git submodule update --init --recursive
opam pin add bap-veri testsuite/veri/bap-veri/ -y
OPAM_SWITCH=`opam config var switch`
mkdir -p $HOME/save_opam/$OPAM_SWITCH/lib/bap
cp -r $HOME/.opam/$OPAM_SWITCH/bin/ $HOME/save_opam/$OPAM_SWITCH/
cp -r $HOME/.opam/$OPAM_SWITCH/share $HOME/save_opam/$OPAM_SWITCH/
cp -r $HOME/.opam/$OPAM_SWITCH/lib/bap/*.plugin $HOME/save_opam/$OPAM_SWITCH/lib/bap
cp $HOME/.opam/config $HOME/save_opam/$OPAM_SWITCH/

opam remove bap-veri -y
'

fi

bash -ex .travis-opam.sh
