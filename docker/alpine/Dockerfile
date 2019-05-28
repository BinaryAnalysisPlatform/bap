FROM ocaml/opam2:alpine

ENV HOME /home/opam
WORKDIR $HOME

RUN opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing \
 && opam update \
 && opam depext --install bap --yes \
 && opam clean -acrs \
 && rm -rf .opam/4.0[2-6] \
 && rm -rf .opam/4.07/.opam-switch/sources/* \
 && rm -rf opam-repository

ENTRYPOINT ["opam", "config", "exec", "--"]
