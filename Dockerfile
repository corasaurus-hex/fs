FROM clojure:openjdk-11-tools-deps-slim-buster

ARG DEBIAN_FRONTEND=noninteractive

ARG babashka_version=0.0.66
ARG babashka_file=babashka-${babashka_version}-linux-amd64.zip
ARG babashka_url=https://github.com/borkdude/babashka/releases/download/v${babashka_version}/$babashka_file

RUN apt-get -y update && apt-get install -y apt-utils && apt-get -y upgrade

RUN apt-get install -y bash git curl zip

RUN curl -OL $babashka_url && unzip $babashka_file && mv bb /usr/local/bin

RUN curl -LSfs https://japaric.github.io/trust/install.sh | \
  sh -s -- --git casey/just --target x86_64-unknown-linux-musl --to /usr/local/bin
RUN curl -O https://raw.githubusercontent.com/borkdude/deps.clj/master/install && bash install /usr/local/bin

RUN mkdir /docker-scripts
COPY entrypoint.sh /docker-scripts

RUN adduser --disabled-password --gecos "" user1
RUN adduser --disabled-password --gecos "" user2
USER user1

WORKDIR /code

ENTRYPOINT ["/docker-scripts/entrypoint.sh"]
