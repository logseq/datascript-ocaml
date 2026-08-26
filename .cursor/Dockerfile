FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV OPAMYES=1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        build-essential \
        pkg-config \
        libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Node.js 24 for js_of_ocaml smoke tests and cross-runtime helpers.
RUN curl -fsSL https://deb.nodesource.com/setup_24.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m -s /bin/bash ubuntu 2>/dev/null || true

USER ubuntu
WORKDIR /home/ubuntu

RUN curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh \
      | bash -s -- --disable-sandboxing \
    && opam init --disable-sandboxing -y \
    && opam switch create 5.5 ocaml-base-compiler.5.5.0 -y \
    && eval "$(opam env --switch=5.5)" \
    && opam update -a

RUN echo 'test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true' >> ~/.bashrc \
    && echo 'eval $(opam env --switch=5.5 2>/dev/null)' >> ~/.bashrc
