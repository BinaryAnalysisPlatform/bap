FROM ocaml/opam:debian-ocaml-4.14

WORKDIR /home/opam

RUN opam remote set-url default https://opam.ocaml.org \
 && opam update \
 && opam depext --update --install bap.2.5.0 --yes -j 1 \
 && opam clean -acrs

ENTRYPOINT ["opam", "config", "exec", "--"]
