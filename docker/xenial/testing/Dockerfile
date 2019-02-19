FROM ubuntu:xenial
MAINTAINER Ivan Gotovchits <ivg@ieee.org>

RUN apt-get -y update
RUN apt-get -y install \
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

RUN useradd -m bap && echo "bap:bap" | chpasswd && adduser bap sudo
RUN sed -i.bkp -e \
      's/%sudo\s\+ALL=(ALL\(:ALL\)\?)\s\+ALL/%sudo ALL=NOPASSWD:ALL/g' \
      /etc/sudoers
USER bap
WORKDIR /home/bap

RUN wget https://github.com/projectatomic/bubblewrap/releases/download/v0.3.1/bubblewrap-0.3.1.tar.xz
RUN tar xvf bubblewrap-0.3.1.tar.xz
WORKDIR /home/bap/bubblewrap-0.3.1/
RUN ./configure
RUN make && sudo make install
RUN bwrap --version
WORKDIR /home/bap
RUN rm -r bubblewrap-0.3.1 bubblewrap-0.3.1.tar.xz

RUN wget https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
RUN echo "" | sh ./install.sh
RUN opam --version

RUN opam init --yes --compiler=4.05.0 --disable-sandboxing
RUN eval $(opam env)
RUN opam repo add bap git://github.com/BinaryAnalysisPlatform/opam-repository#testing
RUN opam update
RUN opam install depext --yes
RUN OPAMJOBS=1 opam depext --install bap --yes
RUN sudo pip install bap

RUN sudo apt-get remove libncurses5 --yes

ENTRYPOINT ["opam", "config", "exec", "--"]
