#!/bin/sh

set -e

case "$@" in
  "build ki")
    cabal build ki --constraint "ki +build-tests" --disable-optimization --enable-tests
    ;;
  "test ki")
    cabal run ki:test:tests --constraint "ki +build-tests" --disable-optimization --enable-tests
    ;;
  "build ki-mtl")
    cabal build ki-mtl --constraint "ki +build-tests" --disable-optimization --enable-tests
    ;;
  "upload ki candidate")
    cabal sdist ki
    cabal haddock ki --haddock-for-hackage
    cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD dist-newstyle/sdist/ki-0.tar.gz
    cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD -d dist-newstyle/ki-0-docs.tar.gz
esac
