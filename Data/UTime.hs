-- | 'UTime' is a minimalistic efficient alternative to 'UTCTime'.
--
-- The 'UTime' representation is intentionally not opaque to allow
-- efficient low-level access. But, you should consider this module
-- \"somewhat internal\".

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.UTime (
  UTime(..),
  fromUTime,
  toUTime,
  toUTimeUp, toUTimeUp',
  toUTimeDown, toUTimeDown',
  utime,
  -- * For testing only:
  dtPico,
  ) where

import Control.Lens
import Data.Time
import Data.Int
import Foreign.Storable
import Unsafe.Coerce

newtype UTime = UTime Int64
                deriving (Eq, Ord, Enum, Bounded, Storable)

-- 'DiffTime' is currently implemented as a newtype around
-- 'Integer'. As long as it stays so, this function is safe; and if it
-- changes it will blow up noticeably.
--
-- Because the appropriate accessors/constructors are not exported,
-- all other conversion mechanisms involve unnecessary 'Rational'
-- arithmetics, which is probably the biggest source of inefficiency
-- in 'UTCTime'.
dtPico :: Iso' DiffTime Integer
dtPico = iso unsafeCoerce unsafeCoerce
{-# INLINE dtPico #-}

fromUTime :: UTime -> UTCTime
fromUTime (UTime micros) = UTCTime (ModifiedJulianDay day) dayTime
  where
    (d, dt) = micros `divMod` 86400000000
    day = fromIntegral $ 40587 + d
    dayTime = (fromIntegral $ 1000000 * dt) ^. from dtPico
{-# INLINE fromUTime #-}

toUTimeGeneric :: Int64 -> UTCTime -> UTime
toUTimeGeneric shift (UTCTime (ModifiedJulianDay day) dayTime) = UTime micros
  where
    micros = d * 86400000000 + dt
    d = fromIntegral day - 40587
    dt = (fromIntegral (dayTime ^. dtPico) + shift) `div` 1000000
{-# INLINE toUTimeGeneric #-}

-- | Converts 'UTCTime' to 'UTime' by rounding.
toUTime :: UTCTime -> UTime
toUTime = toUTimeGeneric 500000
-- TODO(klao): rounding is probably the right thing to do
-- here. But, truncating can make things cleaner and easier to
-- reason about...
{-# INLINE toUTime #-}

-- | Returns the smallest 'UTime' value representing a time not
-- earlier than the input 'UTCTime'.
toUTimeUp :: UTCTime -> UTime
toUTimeUp = toUTimeGeneric 999999
{-# INLINE toUTimeUp #-}

-- | Returns the smallest 'UTime' value representing a time strictly
-- later than the input 'UTCTime'.
toUTimeUp' :: UTCTime -> UTime
toUTimeUp' = toUTimeGeneric 1000000
{-# INLINE toUTimeUp' #-}

-- | Returns the largest 'UTime' value representing a time not
-- later than the input 'UTCTime'.
toUTimeDown :: UTCTime -> UTime
toUTimeDown = toUTimeGeneric 0
{-# INLINE toUTimeDown #-}

-- | Returns the largest 'UTime' value representing a time strictly
-- earlier than the input 'UTCTime'.
toUTimeDown' :: UTCTime -> UTime
toUTimeDown' = toUTimeGeneric (-1)
{-# INLINE toUTimeDown' #-}

utime :: Iso' UTCTime UTime
utime = iso toUTime fromUTime
{-# INLINE utime #-}

instance Show UTime where
  showsPrec p = showsPrec p . fromUTime
  show = show . fromUTime
