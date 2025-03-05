FROM ocaml/opam:ubuntu-24.04-ocaml-4.14

WORKDIR /home/opam

RUN opam remote set-url default https://opam.ocaml.org \
 && opam repo add bap git+https://github.com/BinaryAnalysisPlatform/opam-repository --all \
 && opam update \
 && opam depext --install bap --yes -j 1 \
 && opam clean -acrs

ENTRYPOINT ["opam", "config", "exec", "--" ]
