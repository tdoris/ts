{-# LANGUAGE BangPatterns #-}

module Data.TimeSeries.Periodic (
  Period(..),
  Weekdays(..),
  periodStep,
  periodStepBack,
  PeriodicSequence,
  periodicSequence,
  nth,
  psToList,
  psOver,
  -- * UTime API
  psToUTimeList,
  periodStepUTime,
  periodStepBackUTime,
  psOverUTime,
  ) where

import Control.Arrow (first, second)
import Control.Lens
import Data.Fixed (divMod')
import Data.Time
import Data.Set (Set)
import qualified Data.Set as S
import Data.UTime
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

data Period
  = PicoSeconds Integer
  | Seconds Int
  | Minutes Int
  | Hours Int
  | Days Int
  | Weeks Int
  | Workdays
  | Weekdays (Set Weekdays)
  | Months Int
  | Years Int
  deriving (Eq, Show, Read)

data Weekdays
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Enum)

-- | Represents a sequence of time stamps repeating periodically from
-- a given start time.  The repeating period can be quite flexible,
-- see 'Period'.  To create values of this data type use the
-- 'periodicSequence' function.
data PeriodicSequence
  = PSecs !DiffTime !Integer !DiffTime      -- period, broken down starting time
  | PDays !Integer !Integer !DiffTime   -- period, broken down starting time
  | PCal !Integer !Int !Integer !Int !Int !DiffTime  -- period (years, months), starting time (gregorian (y,m,d), daytime)
  | PWeek !(Vector Int) !Integer !Int !DiffTime  -- period (set of week days), starting time (Monday of the start time, day's index in the set, daytime)
  deriving (Eq, Show)

periodicSequence :: Period   -- ^ Repeation period.
                 -> UTCTime  -- ^ Start time.
                 -> PeriodicSequence
periodicSequence per start = case per of
  PicoSeconds p -> PSecs (picosecondsToDiffTime p) dayi dayt
  Seconds s -> PSecs (fromIntegral s) dayi dayt
  Minutes mp -> PSecs (60 * fromIntegral mp) dayi dayt
  Hours h -> PSecs (3600 * fromIntegral h) dayi dayt
  Days d -> PDays (fromIntegral d) dayi dayt
  Weeks w -> PDays (fromIntegral w * 7) dayi dayt
  Months mp -> PCal 0 mp y m md dayt
  Years yp -> PCal (fromIntegral yp) 0 y m md dayt
  Workdays -> PWeek v (dMon + 7*w) i dayt
    where
      v = V.fromList [0, 1, 2, 3, 4]
      (w, i) = firstAfter v wday
  Weekdays s -> PWeek v (dMon + 7*w) i dayt
    where
      v = V.fromList . map fromEnum . S.toList $ s
      (w, i) = firstAfter v wday
  where
    (UTCTime day@(ModifiedJulianDay dayi) dayt) = start
    (y,m,md) = toGregorian day
    -- Day 0 is a Wednesday
    wday = (dayi + 2) `mod` 7
    dMon = dayi - wday

    firstAfter :: Vector Int -> Integer -> (Integer, Int)
    firstAfter v a = case V.findIndex (>= fromIntegral a) v of
      Just i -> (0, i)
      Nothing -> (1, 0)

-- | Accessor for the @n@th element of a 'PeriodicSequence'.
nth :: PeriodicSequence -> Int -> UTCTime
nth ps k0 = case ps of
  PSecs p day dt -> UTCTime (ModifiedJulianDay $ day + day') dt'
    where (day', dt') = (dt + p * fromIntegral k0) `divMod'` 86400
  PDays p day dt -> UTCTime (ModifiedJulianDay $ day + k * p) dt
  PCal yp mp y m d dt -> UTCTime (fromGregorian (y + k * yp + y') m' d) dt
    where
      (y', m') = first fromIntegral . second (+1) $ (m + k0 * mp - 1) `divMod` 12
  PWeek v day0 mday dt -> UTCTime (ModifiedJulianDay day) dt
    where
      day = day0 + fromIntegral (7 * w + v V.! i)
      (w, i) = (mday + k0) `divMod` V.length v
  where
    k = fromIntegral k0

-- | Returns an infine list of times in the periodic sequence.
--
-- The first element of the result is guaranteed to be not earlier
-- than the start time with which the sequence was created. (But, in
-- the case of Workdays and Weekdays it might be a later time.)
psToList :: PeriodicSequence -> [UTCTime]
psToList ps = go 0
  where
    go !k = nth ps k : go (k+1)

-- | Returns the time value which is one @period@ after the given time.
periodStep :: Period -> UTCTime -> UTCTime
periodStep p t = nth (periodicSequence p t) 1

-- | Returns the time value which is one @period@ before the given time.
periodStepBack :: Period -> UTCTime -> UTCTime
periodStepBack p t = nth (periodicSequence p t) (-1)

-- | Returns the elements of the periodic sequence starting at the
-- beginning and contained in the given time range.
psOver :: Period -> (UTCTime, UTCTime) -> [UTCTime]
psOver p (start, end) = takeWhile (<= end) $ psToList $ periodicSequence p start

--------------------------------------------------------------------------------
-- UTime functions

psToUTimeList :: PeriodicSequence -> [UTime]
psToUTimeList = map toUTime . psToList

periodStepUTime :: Period -> UTime -> UTime
periodStepUTime p = under utime (periodStep p)

periodStepBackUTime :: Period -> UTime -> UTime
periodStepBackUTime p = under utime (periodStepBack p)

psOverUTime :: Period -> (UTime, UTime) -> [UTime]
psOverUTime p (start, end)
  = takeWhile (<= end) . psToUTimeList . periodicSequence p $ fromUTime start

--------------------------------------------------------------------------------
-- Note(klao): there is a 'time-recurrence' package on Hackage, which
-- probably has the required functionality. But, it's licensed under
-- LGPL.
