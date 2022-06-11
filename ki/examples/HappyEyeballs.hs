-- A work-in-progress implementation of Happy Eyeballs

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module HappyEyeballs where

import Control.Applicative
import Control.Exception (MaskingState (..), SomeException, bracket)
import Control.Monad
import Data.Either (fromRight)
import Data.Foldable (asum)
import Data.Functor
import GHC.Conc
import qualified Ki

main :: IO ()
main = do
  let hostname = "www.google.com"
  Ki.scoped \scope -> do
    thread4 <- (fmap . fmap) (fromRight []) (Ki.forkTry @SomeException scope (resolve4 hostname))
    thread6 <- (fmap . fmap) (fromRight []) (Ki.forkTry @SomeException scope (resolve6 hostname))
    result <-
      selectIO
        [ do
            addrs4 <- Ki.awaitSTM thread4
            pure do
              timeout0
                50_000
                ((\addrs6 -> FullyResolved (prioritize (addrs6 ++ addrs4))) <$> Ki.awaitSTM thread6)
                (PartiallyResolved (prioritize addrs4) (Ki.awaitSTM thread6)),
          do
            addrs6 <- Ki.awaitSTM thread6
            pure (pure (PartiallyResolved addrs6 (Ki.awaitSTM thread4)))
        ]
    bracket (connectAny result) (onJust close) print

data ResolveResult
  = FullyResolved [Address]
  | PartiallyResolved [Address] (STM [Address])

resolve4 :: Hostname -> IO [Address]
resolve4 = undefined

resolve6 :: Hostname -> IO [Address]
resolve6 = undefined

prioritize :: [Address] -> [Address]
prioritize = undefined

connectAny :: ResolveResult -> IO (Maybe Socket)
connectAny result = do
  -- Create a TVar to hold the first socket we connect to.
  socketVar <- newTVarIO Nothing
  doneVar <- newTVarIO False

  let put :: Socket -> IO PutResult
      put socket =
        atomically do
          done <- readTVar doneVar
          maybeSocket <- readTVar socketVar
          case (done, maybeSocket) of
            (False, Nothing) -> do
              writeTVar socketVar (Just socket)
              pure Success
            _ -> pure Failure

  Ki.scoped \scope -> do
    let loop :: Maybe (STM [Address]) -> [Address] -> IO (Maybe Socket)
        loop maybeMore = \case
          [] -> do
            atomically (writeTVar doneVar True)
            readTVarIO socketVar
          addr : addrs -> do
            thread <- Ki.forkTryWith @SomeException scope workerOpts (worker addr put)
            timeout
              250_000
              ( selectSTM
                  [ do
                      readTVar socketVar >>= \case
                        Nothing -> retry
                        Just socket -> pure (pure (Just socket)),
                    case maybeMore of
                      Nothing -> retry
                      Just more -> more <&> \addrs0 -> loop Nothing (prioritize (addrs0 ++ addrs)),
                    do
                      _ <- Ki.awaitSTM thread
                      pure (loop maybeMore addrs)
                  ]
              )
              (loop maybeMore addrs)

    case result of
      FullyResolved addrs -> loop Nothing addrs
      PartiallyResolved addrs more -> loop (Just more) addrs
  where
    workerOpts :: Ki.ThreadOptions
    workerOpts =
      Ki.defaultThreadOptions
        { Ki.maskingState = MaskedInterruptible
        }

worker :: Address -> (Socket -> IO PutResult) -> IO ()
worker addr put = do
  putStrLn ("Trying " ++ addr)
  socket <- connect addr
  result <- put socket
  case result of
    Success -> pure ()
    Failure -> close socket

data PutResult
  = Success
  | Failure

type Address = String

type Socket = String

type Hostname = String

connect :: String -> IO String
connect s = do
  threadDelay 600000
  pure s

close :: Socket -> IO ()
close _ = pure ()

timeout :: Int -> STM (IO a) -> IO a -> IO a
timeout micros action fallback = do
  done <- registerDelay micros
  selectIO
    [ action,
      readTVar done >>= \case
        False -> retry
        True -> pure fallback
    ]

timeout0 :: Int -> STM a -> a -> IO a
timeout0 micros action fallback =
  timeout micros (pure <$> action) (pure fallback)

selectSTM :: [STM a] -> STM a
selectSTM =
  asum

select :: [STM a] -> IO a
select =
  atomically . selectSTM

selectIO :: [STM (IO a)] -> IO a
selectIO =
  join . select

onJust :: Applicative m => (a -> m ()) -> Maybe a -> m ()
onJust =
  maybe (pure ())
