module Ki.Internal.ByteCount
  ( ByteCount,
    kilobytes,
    megabytes,
    byteCountToInt64,
  )
where

import Ki.Internal.Prelude

-- | A number of bytes.
newtype ByteCount = ByteCount Int64
  deriving newtype (Eq, Ord)

instance Show ByteCount where
  show (ByteCount b)
    | (mb, 0) <- quotRem b 1048576, mb > 0 = "megabytes " ++ show mb
    | (kb, 0) <- quotRem b 1024 = "kilobytes " ++ show kb
    | otherwise = undefined

-- | A number of kilobytes.
kilobytes :: Natural -> ByteCount
kilobytes n =
  ByteCount (snip (n * 1024))

-- | A number of megabytes.
megabytes :: Natural -> ByteCount
megabytes n =
  ByteCount (snip (n * 1048576))

byteCountToInt64 :: ByteCount -> Int64
byteCountToInt64 =
  coerce

snip :: Natural -> Int64
snip n =
  fromIntegral (min (fromIntegral (maxBound :: Int64)) n)
