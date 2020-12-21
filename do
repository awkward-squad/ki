#!/bin/sh

set -e

case "$@" in
  "build")
    exec cabal build --disable-optimization
    ;;
  "build experiments/fork.hs")
    exec cabal exec -- ghc --make -O2 -threaded -rtsopts -package ki experiments/fork.hs
    ;;
  "clean")
    exec cabal clean
    ;;
  "dev")
    exec ghcid -c 'cabal repl ki:lib:ki --disable-optimization' --restart ki.cabal
    ;;
  "dev tests")
    exec \
      ghcid \
        -c 'cabal repl ki:test:dejafu-tests --constraint "ki +dejafu-tests" --disable-optimization' \
        -T ':main' \
        -W \
        --restart ki.cabal
    ;;
  "docs")
    exec cabal haddock --disable-optimization --haddock-hyperlink-source --haddock-quickjump
    ;;
  "format")
    exec ormolu -i $(ls src/**/*.hs test/**/*.hs)
    ;;
  "repl")
    exec cabal repl ki:lib:ki --disable-optimization
    ;;
  "test")
    cabal run ki:test:unit-tests --disable-optimization
    cabal run ki:test:dejafu-tests --constraint "ki +dejafu-tests" --disable-optimization
    ;;
  "upload candidate")
    cabal sdist
    cabal haddock --haddock-for-hackage
    cabal upload dist-newstyle/sdist/ki-0.2.0.tar.gz
    cabal upload -d dist-newstyle/ki-0.2.0-docs.tar.gz
    ;;
esac
