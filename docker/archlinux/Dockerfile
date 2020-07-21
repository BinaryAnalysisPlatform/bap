FROM archlinux:latest

RUN pacman -Syu --noconfirm && pacman -S sudo --noconfirm \
 && useradd -m bap \
 && echo 'bap ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/bap \
 && su bap

USER bap
WORKDIR /home/bap

RUN sudo pacman -S --noconfirm m4 git unzip make curl wget \
         diffutils patch make gcc pkgconfig llvm python2 \
         which clang \
  && wget https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh \
  && echo "" | sudo sh install.sh \
  && opam init --comp=4.09.1 --disable-sandboxing --yes \
  && eval "$(opam env)" \
  && opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing --all \
  && opam update \
  && opam install bap --yes -j 1 \
  && opam clean -acrs \
  && rm -rf /home/opam/.opam/4.09.1/.opam-switch/sources/*

ENTRYPOINT ["opam", "config", "exec", "--"]
