#!/bin/sh

set -e

case "$@" in
  "dev ki-indef")
    exec ghcid -c 'cabal repl ki:lib:ki-indef' --restart ki/ki.cabal
    ;;
  "build experiments/fork.hs")
    exec cabal exec -- ghc --make -O2 -threaded -rtsopts -package ki experiments/fork.hs
    ;;
  "build ki")
    exec cabal build ki --constraint "ki +build-tests" --disable-optimization --enable-tests
    ;;
  "test ki")
    exec cabal run ki:test:tests --constraint "ki +build-tests" --disable-optimization --enable-tests
    ;;
  "upload ki candidate")
    cabal sdist ki
    cabal haddock ki --haddock-for-hackage
    cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD dist-newstyle/sdist/ki-0.tar.gz
    cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD -d dist-newstyle/ki-0-docs.tar.gz
    ;;
esac
