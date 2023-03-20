-- Some code modified from the atomic-primops library; license included below.
-- Copyright (c)2012-2013, Ryan R. Newton
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Ryan R. Newton nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Ki.Internal.Counter
  ( Counter,
    newCounter,
    incrCounter,
  )
where

import Data.Bits
import GHC.Base

-- | A thread-safe counter implemented with atomic fetch-and-add.
data Counter
  = Counter (MutableByteArray# RealWorld)

-- | Create a new counter initialized to 0.
newCounter :: IO Counter
newCounter =
  IO \s0# ->
    case newByteArray# size s0# of
      (# s1#, arr# #) ->
        case writeIntArray# arr# 0# 0# s1# of
          s2# -> (# s2#, Counter arr# #)
  where
    !(I# size) =
      finiteBitSize (undefined :: Int) `div` 8
{-# INLINE newCounter #-}

-- | Increment a counter and return the value prior to incrementing.
incrCounter :: Counter -> IO Int
incrCounter (Counter arr#) =
  IO \s0# ->
    case fetchAddIntArray# arr# 0# 1# s0# of
      (# s1#, n# #) -> (# s1#, I# n# #)
{-# INLINE incrCounter #-}
