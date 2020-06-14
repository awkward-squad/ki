module Ki.Indef.Seconds
  ( Seconds (..),
    toMicros,
  )
where

import Data.Data (Data)
import Data.Fixed
import GHC.Generics (Generic)

newtype Seconds = Seconds (Fixed E3)
  deriving stock (Data, Generic)
  deriving newtype (Enum, Eq, Fractional, Num, Ord, Read, Real, RealFrac, Show)

toMicros :: Seconds -> Int
toMicros (Seconds (MkFixed milliseconds)) =
  fromIntegral milliseconds * 1000
