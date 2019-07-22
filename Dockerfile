FROM ubuntu:18.04

ENV LANG en_US.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get install -y \
    curl

RUN curl -sSL https://get.haskellstack.org/ | sh

ENV PATH="/root/.local/bin:${PATH}"
RUN stack upgrade

RUN mkdir -p /haskeme

COPY stack.yaml /haskeme
COPY *.cabal /haskeme

WORKDIR /haskeme

COPY . .

RUN stack setup && \
    stack build --test && \
    stack install

CMD ["haskeme"]