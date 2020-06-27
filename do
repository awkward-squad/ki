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
    exec ghcid -c 'cabal repl ki:lib:ki' --restart ki.cabal
    ;;
  "dev tests")
    exec \
      ghcid \
        -c 'cabal repl ki:test:tests --constraint "ki +test" --disable-optimization' \
        -T ':main' \
        -W \
        --restart ki.cabal
    ;;
  "docs")
    exec cabal haddock --haddock-hyperlink-source --haddock-quickjump
    ;;
  "freeze")
    exec cabal freeze
    ;;
  "test")
    cabal run ki:test:tests --constraint "ki +test" --disable-optimization --enable-tests
    # cabal exec -- ghci -package ki -pgmL markdown-unlit tutorial/*.lhs < /dev/null
    ;;
  "upload candidate")
    cabal sdist
    cabal haddock --haddock-for-hackage
    cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD dist-newstyle/sdist/ki-0.tar.gz
    cabal upload -u $HACKAGE_USERNAME -p $HACKAGE_PASSWORD -d dist-newstyle/ki-0-docs.tar.gz
    ;;
esac
