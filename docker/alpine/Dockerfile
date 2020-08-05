FROM ocaml/opam2:alpine

WORKDIR /home/opam

RUN opam switch 4.09 \
 && eval "$(opam env)" \
 && opam remote set-url default https://opam.ocaml.org \
 && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing --all \
 && opam update \
 && opam depext --install bap --yes -j 1 \
 && opam clean -acrs \
 && rm -rf /home/opam/.opam/4.0[2-8] \
 && rm -rf /home/opam/.opam/4.09/.opam-switch/sources/* \
 && rm -rf /home/opam/opam-repository \
 && sudo apk add py-pip \
 && sudo pip install bap

ENTRYPOINT ["opam", "config", "exec", "--"]
