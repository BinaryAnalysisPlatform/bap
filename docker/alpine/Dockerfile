FROM ocaml/opam:alpine-ocaml-4.14

WORKDIR /home/opam

RUN opam remote set-url default https://opam.ocaml.org \
 && opam repo add bap git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing --all \
 && opam update \
 && opam depext --install bap --yes -j 1


ENTRYPOINT ["opam", "config", "exec", "--"]
