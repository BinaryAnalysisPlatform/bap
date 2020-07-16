FROM ocaml/opam2:debian-testing

WORKDIR /home/opam

RUN sudo apt-get update --yes && sudo apt-get install wget \
 && wget https://radare.mikelloc.com/get/4.5.0-git/radare2_4.5.0-git_amd64.deb \
 && sudo dpkg -i radare2_4.5.0-git_amd64.deb \
 && opam switch 4.09 \
 && eval "$(opam env)" \
 && opam remote set-url default https://opam.ocaml.org \
 && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing --all \
 && opam update \
 && opam depext --install bap --yes -j 1 \
 && opam clean -acrs \
 && rm -rf /home/opam/.opam/4.0[2-8,10] \
 && rm -rf /home/opam/.opam/4.09/.opam-switch/sources/* \
 && rm -rf /home/opam/opam-repository

ENTRYPOINT ["opam", "config", "exec", "--"]
