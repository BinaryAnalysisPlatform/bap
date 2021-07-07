FROM ocaml/opam2:ubuntu-18.04

WORKDIR /home/opam

RUN sudo add-apt-repository ppa:ivg/ghidra -y \
 && sudo apt-get install libghidra-dev -y \
 && sudo apt-get install libghidra-data -y \
 && sudo apt-get update  \
 && opam switch 4.09 \
 && eval "$(opam env)" \
 && opam remote set-url default https://opam.ocaml.org \
 && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository --all \
 && opam update \
 && opam depext --install bap-extra --yes -j 1 \
 && opam clean -acrs \
 && rm -rf /home/opam/.opam/4.0[2-8,10] \
 && rm -rf /home/opam/.opam/4.09/.opam-switch/sources/* \
 && rm -rf /home/opam/opam-repository

ENTRYPOINT ["opam", "config", "exec", "--"]
