OPAM_DEPENDS="core_kernel oasis piqi zarith bitstring utop cmdliner faillib ezjsonm lwt-zmq uri re cohttp"
SYS_DEPENDS="libgmp-dev time llvm-3.4-dev libzmq3-dev aspcud"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.02.0,1.2.0) ppa=avsm/ocaml42+opam12 ;;
4.02.0,1.1.0) ppa=avsm/ocaml42+opam11 ;;
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac


install_on_linux () {
  echo 'yes' | sudo add-apt-repository ppa:$ppa
  echo 'yes' | sudo apt-add-repository ppa:chris-lea/zeromq
  sudo apt-get update -qq
  sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam $SYS_DEPENDS
  opam init
}

install_on_osx () {
  brew install gmp
  brew install opam
  opam init
  opam switch $OCAML_VERSION
  eval `opam config env`
}

export OPAMYES=1
export OPAMVERBOSE=1
export OPAMJOBS=4

echo $TRAVIS_OS_NAME
case $TRAVIS_OS_NAME in
  osx) install_on_osx ;;
  linux) install_on_linux ;;
esac

echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam install ${OPAM_DEPENDS}
eval `opam config env`

oasis setup
./configure --prefix=$(opam config var prefix) --enable-tests --with-cxx=`which $CXX`
make
make install
make test
./test.sh
make uninstall
