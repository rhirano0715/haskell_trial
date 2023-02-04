FROM haskell:latest

WORKDIR /workspace

RUN apt-get update && \
    apt-get -y install git && \
    apt-get clean

RUN cabal clean && \
    cabal update

RUN cabal install doctest
