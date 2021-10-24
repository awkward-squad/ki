module Ki.Duration
  ( Duration (..),
    toMicroseconds,
    microseconds,
    milliseconds,
    seconds,
    minutes,
    hours,
  )
where

import Data.Fixed
import Ki.Prelude

-- | A length of time with microsecond precision.
newtype Duration = Duration (Fixed E6)
  deriving newtype (Eq, Ord)

instance Show Duration where
  show (Duration (MkFixed us))
    | n <- fromIntegral us / (3600000000 :: Double), n >= 1 = "hours " ++ show n
    | n <- fromIntegral us / (60000000 :: Double), n >= 1 = "minutes " ++ show n
    | n <- fromIntegral us / (1000000 :: Double), n >= 1 = "seconds " ++ show n
    | (n, 0) <- quotRem us 1000, n > 0 = "milliseconds " ++ show n
    | otherwise = "microseconds " ++ show us


toMicroseconds :: Duration -> Int
toMicroseconds (Duration (MkFixed us)) =
  fromIntegral us

-- | A number of microseconds.
microseconds :: Natural -> Duration
microseconds n =
  Duration (MkFixed (fromIntegral n))

-- | A number of milliseconds.
milliseconds :: Natural -> Duration
milliseconds n =
  Duration (MkFixed (1000 * fromIntegral n))

-- | A number of seconds.
seconds :: Double -> Duration
seconds n =
  Duration (MkFixed (floor (1000000 * n)))

-- | A number of minutes.
minutes :: Double -> Duration
minutes n =
  Duration (MkFixed (floor (60000000 * n)))

-- | A number of hours.
hours :: Double -> Duration
hours n =
  Duration (MkFixed (floor (3600000000 * n)))
