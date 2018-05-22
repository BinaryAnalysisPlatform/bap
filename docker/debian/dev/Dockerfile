FROM ocaml/opam:debian
MAINTAINER Ivan Gotovchits <ivg@ieee.org>
RUN sudo apt-get -y update && sudo apt-get -y install \
    llvm-3.8-dev \
    git \
    libcurl4-gnutls-dev \
    libgmp-dev \
    zlib1g-dev \
    binutils-multiarch \
    clang \
    time
RUN opam init --auto-setup --comp=4.05.0 --yes
RUN eval `opam config env`
RUN LLVM_CONFIG=llvm-config-3.8 opam install conf-bap-llvm --yes
RUN opam install conf-binutils
RUN git clone https://github.com/binaryanalysisplatform/bap
RUN opam pin add bap ./bap --yes
RUN sudo apt-get install python-pip --yes
RUN sudo pip install bap
RUN opam install merlin ocp-indent --yes
