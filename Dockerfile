FROM haskell:latest

WORKDIR /workspace

RUN apt-get update && \
    apt-get -y install git && \
    apt-get clean

RUN cabal clean && \
    cabal update

RUN cabal install doctest

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
      BOOTSTRAP_HASKELL_GHC_VERSION=latest \
      BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
      BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
      BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
      BOOTSTRAP_HASKELL_ADJUST_BASHRC=P \
      sh
