FROM ocaml/opam2:alpine as bap-testing

ENV HOME /home/opam
WORKDIR $HOME

RUN sudo apk update \
 && sudo apk add \
    binutils \
    clang \
    gmp-dev \
    libzip-dev \
    llvm-dev \
    llvm-static \
    m4 \
    perl \
    zlib-dev

RUN opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing \
 && opam update \
 && opam depext --install bap --yes \
 && opam clean -acrs \
 && rm -rf .opam/4.0[2-6] \
 && rm -rf .opam/4.07/.opam-switch/sources/* \
 && rm -rf opam-repository

ENTRYPOINT ["opam", "config", "exec", "--"]
