#!/bin/sh

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
esac
