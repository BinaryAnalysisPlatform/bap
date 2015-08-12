FROM avsm/docker-opam:ubuntu-trusty-4.02.1
MAINTAINER maurer@matthewmaurer.org
RUN sudo apt-get update
ADD . /home/opam/bap
RUN opam pin add bap ~/bap -n
RUN opam install depext
RUN opam config exec opam depext bap
RUN opam install bap utop
# add python bindings
RUN sudo apt-get install python-pip -y
RUN sudo pip install ~/bap
RUN /bin/sh -c 'echo "eval `opam config env`" >> /home/opam/.bashrc'
WORKDIR /home/opam/
