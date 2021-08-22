module Ki.Duration
  ( Duration (..),
    toMicroseconds,
    microseconds,
    milliseconds,
    seconds,
  )
where

import Data.Data (Data)
import Data.Fixed
import Ki.Prelude

-- | A length of time with microsecond precision. Numeric literals are treated as seconds.
newtype Duration = Duration (Fixed E6)
  deriving stock (Data, Generic)
  deriving newtype (Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac, Show)

toMicroseconds :: Duration -> Int
toMicroseconds (Duration (MkFixed us)) =
  fromIntegral us

-- | One microsecond.
microseconds :: Duration
microseconds =
  Duration (MkFixed 1)

-- | One millisecond.
milliseconds :: Duration
milliseconds =
  Duration (MkFixed 1000)

-- | One second.
seconds :: Duration
seconds =
  Duration (MkFixed 1000000)
