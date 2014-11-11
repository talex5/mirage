FROM ubuntu:utopic
MAINTAINER Anil Madhavapeddy <anil@recoil.org>
RUN apt-get update
RUN apt-get -y install sudo pkg-config git build-essential m4 software-properties-common ocaml camlp4-extra ocaml-native-compilers 0install-core --no-install-recommends
RUN apt-get -y install curl
RUN git config --global user.email "docker@example.com"
RUN git config --global user.name "Docker CI"
RUN adduser --disabled-password --gecos "" mirage
RUN passwd -l mirage
USER mirage
ENV HOME /home/mirage
ENV OPAMVERBOSE 1
ENV OPAMYES 1
ENV PATH /home/mirage/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN 0install add opam http://tools.ocaml.org/opam.xml --not-before=1.2
RUN opam init -a
ENTRYPOINT /bin/bash
