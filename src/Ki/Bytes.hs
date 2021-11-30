module Ki.Bytes
  ( Bytes,
    kilobytes,
    megabytes,
    bytesToInt64,
  )
where

import Ki.Prelude

-- | A number of bytes.
newtype Bytes = Bytes Int64
  deriving newtype (Eq, Ord)

instance Show Bytes where
  show (Bytes b)
    | (mb, 0) <- quotRem b 1048576, mb > 0 = "megabytes " ++ show mb
    | (kb, 0) <- quotRem b 1024 = "kilobytes " ++ show kb
    | otherwise = undefined

-- | A number of kilobytes.
kilobytes :: Natural -> Bytes
kilobytes n =
  Bytes (snip (n * 1024))

-- | A number of megabytes.
megabytes :: Natural -> Bytes
megabytes n =
  Bytes (snip (n * 1048576))

bytesToInt64 :: Bytes -> Int64
bytesToInt64 =
  coerce

snip :: Natural -> Int64
snip n =
  fromIntegral (min (fromIntegral (maxBound :: Int64)) n)
