FROM ocaml/opam2:debian-stable

MAINTAINER Ivan Gotovchits <ivg@ieee.org>

RUN sudo apt-get -y update && sudo apt-get -y install \
    build-essential \
    curl \
    git \
    libx11-dev \
    m4 \
    pkg-config \
    python-pip \
    software-properties-common \
    sudo \
    unzip \
    wget \
    libcap-dev \
    gcc \
    make \
    libncurses5-dev

RUN opam switch 4.05
RUN eval "$(opam env)"
RUN opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing
RUN opam update
RUN opam install depext --yes
RUN OPAMJOBS=1 opam depext --install bap --yes
RUN sudo pip install bap

WORKDIR /home/opam

ENTRYPOINT ["opam", "config", "exec", "--"]
