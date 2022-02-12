FROM ocaml/opam:debian

WORKDIR /home/opam

RUN opam remote set-url default https://opam.ocaml.org \
 && opam repo add  bap-testing git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing --all \
 && opam update \
 && opam depext --install bap --yes -j 1 \
 && opam clean -acrs

ENTRYPOINT ["opam", "config", "exec", "--"]
