FROM ocaml/opam:ubuntu-18.04-ocaml-4.14

WORKDIR /home/opam

RUN sudo apt-get update  \
 && sudo apt-get install software-properties-common -y \
 && sudo add-apt-repository ppa:ivg/ghidra -y \
 && sudo apt-get install libghidra-dev -y \
 && sudo apt-get install libghidra-data -y \
 && opam remote set-url default https://opam.ocaml.org \
 && opam repo add bap git+https://github.com/BinaryAnalysisPlatform/opam-repository --all \
 && opam update \
 && opam depext --install bap-extra "z3=4.8.11" --yes -j 1 \
 && opam clean -acrs

ENTRYPOINT ["opam", "config", "exec", "--"]
