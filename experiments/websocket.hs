-- cabal exec -- ghc -O -package ki experiments/pusher.hs

{-# LANGUAGE BlockArguments #-}

module Main where

import Ki

main :: IO ()
main =
  global do
    pure ()

data Websocket

openWebsocket :: Context => String -> (Websocket -> IO a) -> IO a
openWebsocket host callback =
  scoped \scope -> do
    undefined
