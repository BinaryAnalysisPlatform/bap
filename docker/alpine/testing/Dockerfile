FROM ocaml/opam2:alpine

WORKDIR /home/opam

RUN opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing \
 && opam update \
 && opam depext --install bap --yes \
 && opam clean -acrs \
 && rm -rf /home/opam/.opam/4.0[2-6] \
 && rm -rf /home/opam/.opam/4.07/.opam-switch/sources/* \
 && rm -rf /home/opam/opam-repository

ENTRYPOINT ["opam", "config", "exec", "--"]
