ARG TAG=latest
ARG IMAGE=ocaml/opam2:$TAG
FROM $IMAGE

MAINTAINER Nicolas Jeannerod

ARG SWITCH=
RUN [ -z "$SWITCH" ] || opam switch create "$SWITCH"
RUN opam update && opam upgrade

RUN [ -n "$HTTP_PROXY" ] && sudo sh -c "echo 'Acquire::http::Proxy \"$HTTP_PROXY\";' >> /etc/apt/apt.conf"
RUN sudo apt-get update && sudo apt-get install -yy -qq libgmp-dev zlib1g-dev m4 automake

WORKDIR /home/opam/workdir

COPY *.opam .
RUN sudo chown -R opam .
RUN opam install . --deps-only

COPY . .
RUN sudo chown -R opam .
RUN eval $(opam env) && make && make install
