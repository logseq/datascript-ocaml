FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive
ENV OPAMYES=1

# System packages + opam as root so /usr/bin/opam is available to every shell.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        build-essential \
        pkg-config \
        bubblewrap \
        opam \
        libsqlite3-dev \
        liblmdb-dev \
    && rm -rf /var/lib/apt/lists/*

# Node.js 24 for js_of_ocaml / Melange smoke tests.
RUN curl -fsSL https://deb.nodesource.com/setup_24.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m -s /bin/bash ubuntu 2>/dev/null || true

USER ubuntu
WORKDIR /home/ubuntu

# OCaml 5.5 toolchain lives in the image; project opam deps are installed by
# .cursor/cloud-agent-install.sh after the repository checkout is available.
RUN opam init --disable-sandboxing -a -y \
    && opam update -a \
    && opam switch create 5.5 ocaml-base-compiler.5.5.0 -y \
    && eval "$(opam env --switch=5.5)" \
    && opam install dune ocamlfind -y

RUN echo 'test -r ~/.opam/opam-init/init.sh && . ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true' >> ~/.bashrc \
    && echo 'eval $(opam env --switch=5.5 2>/dev/null)' >> ~/.bashrc
