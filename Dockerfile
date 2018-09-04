FROM ubuntu:16.04

MAINTAINER James Dabbs <james.dabbs@gmail.com>

ARG STACK_VERSION=1.7.1

RUN apt-get update && \
    apt-get install -y build-essential wget libgmp3-dev libpq-dev libicu-dev git

RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'

COPY package.yaml package.yaml
COPY stack.yaml stack.yaml

RUN stack build --only-dependencies
