{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Data.TimeSeries.UTime (
  -- * Simple accessors
  tsLength,
  tsRange,
  tsTraversed,
  tsTraversedWithIndex,
  toPairList,
  -- * Lookup at time
  tsSearch,
  firstAfter,
  lastBefore,
  -- * Construction
  fromSortedPairList,
  fromUnsortedPairList,
  fromPeriodicData,
  -- * Lookup with interpolation
  TSInterpolate(..),
  interpolateAt,
  linearBetween,
  tsiExtend,
  tsiNoExtend,
  interpolateLinear,
  extendInterpolateLinear,
  tsGet,
  -- * Slicing
  tsSlice,
  justAfter,
  tsSliceByCount,
  tsSplitAt,
  -- * Merging time series
  TSMerge(..),
  tsMerge,
  tsMergeWith,
  tsMergeEnhance,
  -- * Resampling time series
  tsResampleLocal,
  extendForward,
  extendBackward,
  tsResampleGlobal,
  tsResampleMoving,
  -- * Shifting time series in time
  tsOffsetGeneral,
  tsOffsetByPeriod,
  ) where

import Control.Applicative
import Control.Lens
import Data.Bits (shiftR)
import Data.Ord (comparing)
import Data.Maybe (catMaybes, fromJust)
import Data.TimeSeries.Class
import Data.TimeSeries.Periodic
import Data.UTime (UTime(..))
import qualified Data.Vector.Generic as G
import Statistics.Function (sortBy)

tsLength :: TSeries ts a => ts a -> Int
tsLength = G.length . toVector
{-# INLINE tsLength #-}

-- | Returns the first and last time stamp of a time series.
tsRange :: TSeries ts a => ts a -> Maybe (UTime, UTime)
tsRange ts = case tsLength ts of
  0 -> Nothing
  _ -> Just (G.head v, G.last v)
  where
    v = tsTimes ts

-- TODO(klao): Going through lists is probably inefficient! Check, and
-- go through streams it that's the case.

-- | Traversal of the values of a time series with access to the time stamp as index.
tsTraversed :: (TSeries ts a, TSeries ts b) => IndexedTraversal UTime (ts a) (ts b) a b
tsTraversed f ts
  = fromVector . G.fromListN (G.length v) <$> traverse (itraversed f) (G.toList v)
  where
    v = toVector ts


-- | Traversal of ('UTime', @value@) pairs of a time series with
-- access to a positional index.
--
-- The user of this traversal should guarantee to not reorder the
-- events (ie. that the time stamps are modified in a monotonic way).
tsTraversedWithIndex :: (TSeries ts a, TSeries ts b)
                        => IndexedTraversal Int (ts a) (ts b) (UTime, a) (UTime, b)
tsTraversedWithIndex f ts
  = fromVector . G.fromListN (G.length v) <$> itraversed f (G.toList v)
  where
    v = toVector ts

-- | Construct a time series from a list of (time stamp, value) pairs
--
-- Precondition: the list /have to/ be sorted by time stamps.
fromSortedPairList :: TSeries ts a => [(UTime, a)] -> ts a
fromSortedPairList = fromVector . G.fromList

-- | Construct a time series from a list of (time stamp, value) pairs
--
-- The list is sorted by time stamps on construction.
fromUnsortedPairList :: TSeries ts a => [(UTime, a)] -> ts a
fromUnsortedPairList = fromVector . sortBy (comparing fst) . G.fromList

-- | Returns a list of (time stamp, value) pairs of a time series
-- sorted by time stamps.
toPairList :: TSeries ts a => ts a -> [(UTime, a)]
toPairList = G.toList . toVector

-- | Zip a 'PeriodicSequence' with a list of values.
fromPeriodicData :: TSeries ts a => PeriodicSequence -> [a] -> ts a
fromPeriodicData ps as = fromSortedPairList $ zip (psToUTimeList ps) as

--------------------------------------------------------------------------------
-- Lookup

-- | Returns the lowest index @i@ such that all events after @i@ have time
-- stamp at least t.
tsSearch :: TSeries ts a => ts a
            -> UTime -- ^ the reference time stamp @t@
            -> Int     -- ^ the returned index @i@
tsSearch ts t | n == 0     = 0
              | otherwise  = t `seq` go 0 n
  where
    v = tsTimes ts
    n = G.length v
    go !l !u | l >= u  = l
             | G.unsafeIndex v k < t  = go (k+1) u
             | otherwise  = go l k
      where
        k = (l + u) `shiftR` 1

-- | Returns the first event that has time stamp not earlier than @t@
-- or 'Nothing' if there's no such event in the series.
firstAfter :: TSeries ts a => ts a
           -> UTime  -- ^ the refenrece time stmap @t@
           -> Maybe (UTime, a)
firstAfter ts t | k < tsLength ts = Just (toVector ts G.! k)
                | otherwise = Nothing
  where
    k = tsSearch ts t

-- | Returns the last event that has time stamp not later than @t@
-- or 'Nothing' if there's no such event in the series.
lastBefore :: TSeries ts a => ts a
           -> UTime  -- ^ the reference time stamp @t@
           -> Maybe (UTime, a)
lastBefore ts t | k < n, t == t2  = Just e2
                | k > 0           = Just e1
                | otherwise       = Nothing
  where
    k = tsSearch ts t
    v = toVector ts
    n = tsLength ts
    e2@(t2, _) = v G.! k
    e1 = v G.! (k-1)

--------------------------------------------------------------------------------
-- Slicing

-- | Returns the slice of the time series within the provided time interval.
--
-- The time interval is interpreted as half-open, ie. the start time
-- is inclusive, but the end time is not. If you need different
-- behavior use the provided convenience function: 'justAfter' (for
-- start time, to make it exclusive; for end time, to make it
-- inclusive).
tsSlice :: TSeries ts a => ts a
        -> UTime -- ^ start time (inclusive)
        -> UTime -- ^ end time (exclusive)
        -> ts a
tsSlice ts tStart tEnd
  | tStart > tEnd  = error "tsSlice: start time later than end time"
  | otherwise  = fromVector $ G.slice start (end - start) $ toVector ts
  where
    start = tsSearch ts tStart
    end = tsSearch ts tEnd

-- | Returns the next representable time value.
justAfter :: UTime -> UTime
justAfter = succ

-- | Returns the slice of the time series within the provided index interval.
--
-- The index interval is half-open: start index is inclusive, end
-- index is exclusive.
tsSliceByCount :: TSeries ts a => ts a
               -> Int -- ^ start index (inclusive)
               -> Int -- ^ end index (exclusive)
               -> ts a
tsSliceByCount ts start end = fromVector $ G.slice start (end - start) $ toVector ts

-- | Split the time series into two parts: everything strictly before
-- the given time stamp, and everything after the given time stamp
-- (including it).
tsSplitAt :: TSeries ts a => UTime -> ts a -> (ts a, ts a)
tsSplitAt t ts = (before, after)
  where
    i = tsSearch ts t
    v = toVector ts
    (before, after) = G.splitAt i v & both %~ fromVector

--------------------------------------------------------------------------------
-- Interpolating

-- | Data type used to set up interpolation functions like
-- 'interpolateAt', 'tsMergeEnhance' and 'tsResampleLocal'.
data TSInterpolate a =
  TSInterpolate
  { tsiBefore  :: UTime -> (UTime, a) -> Maybe (UTime, a)
    -- ^ Function to interpolate for time stamps that are before the first item in the time series.
  , tsiAfter   :: UTime -> (UTime, a) -> Maybe (UTime, a)
    -- ^ Function to interpolate for time stamps that are after the last item in the time series.
  , tsiBetween :: UTime -> (UTime, a) -> (UTime, a) -> Maybe (UTime, a)
    -- ^ Function to interpolate for time stamps in the middle of the time series.
  }

interpolateAt :: TSeries ts a => TSInterpolate a -- ^ Parameters to use during the interpolation.
              -> ts a                -- ^ Input time series to interpolate from.
              -> UTime             -- ^ Time stamp to interpolate at.
              -> Maybe (UTime, a)  -- ^ The result of interpolation.
interpolateAt inter ts t | k < n, t == t2  = Just e2
                         | k < n, k > 0    = tsiBetween inter t e1 e2
                         | n > 0, k == 0   = tsiBefore  inter t e2
                         | n > 0, k == n   = tsiAfter   inter t e1
                         | otherwise       = Nothing
  where
    k = tsSearch ts t
    v = toVector ts
    n = tsLength ts
    e2@(t2, _) = v G.! k
    e1 = v G.! (k-1)

-- | Helper function to linearly interpolate a numeric value between two events.
--
-- Assumes that the two events are in order and the interpolating time
-- is strictly between the two time stamps.
--
-- Useful as the 'tsiBetween' field of a 'TSInterpolate'.
linearBetween :: Fractional a => UTime -> (UTime, a) -> (UTime, a)
                 -> Maybe (UTime, a)
linearBetween ut (ut0, x0) (ut1, x1) = Just (ut, wx / (t1 - t0))
  where
    f (UTime us) = fromIntegral us
    t = f ut
    t0 = f ut0
    t1 = f ut1
    wx = x0 * (t1 - t) + x1 * (t - t0)

-- | Defines trivial extending of a time series: uses the the same
-- value as the first/last item.
--
-- To be used as 'tsiBefore' or 'tsiAfter' field of a 'TSInterpolate'.
tsiExtend :: UTime -> (UTime, a) -> Maybe (UTime, a)
tsiExtend t (_, x) = Just (t, x)

-- | Defines non-extending time series.
--
-- To be used as 'tsiBefore' or 'tsiAfter' field of a 'TSInterpolate'.
tsiNoExtend :: UTime -> (UTime, a) -> Maybe (UTime, a)
tsiNoExtend = const $ const Nothing

-- | Linearly interpolates within time series; does not extend.
interpolateLinear :: Fractional a => TSInterpolate a
interpolateLinear = TSInterpolate tsiNoExtend tsiNoExtend linearBetween

-- | Linearly interpolates within time series; extends at the ends.
extendInterpolateLinear :: Fractional a => TSInterpolate a
extendInterpolateLinear = TSInterpolate tsiExtend tsiExtend linearBetween

-- | Access any time stamp in the given time series, with linear
-- interpolation and trivial extension at the ends if needed.
tsGet :: (Fractional a, TSeries ts a) => ts a -> UTime -> a
tsGet ts = snd . fromJust . interpolateAt extendInterpolateLinear ts

--------------------------------------------------------------------------------
-- Time series merging

-- | Structure describing a recipe for a generic merge.
data TSMerge a b c =
  TSMerge
  { tsmLeft  :: UTime -> a -> Maybe c
  , tsmRight :: UTime -> b -> Maybe c
  , tsmBoth  :: UTime -> a -> b -> Maybe c
  }

-- | Generic (non-interpolating) merge.
--
-- Every time stamp considered independently from all the
-- other. Conversion or combination of values is made according to the
-- provided recipe, based on whether the value is present in one or
-- both time series.
tsMerge :: (TSeries ts a, TSeries ts b, TSeries ts c)
           => TSMerge a b c -> ts a -> ts b -> ts c
tsMerge (TSMerge mleft mright mboth) tsa tsb = fromSortedPairList . catMaybes $ go as0 bs0
  where
    as0 = toPairList tsa
    bs0 = toPairList tsb
    fleft (t,a) = (t,) <$> mleft t a
    fright (t,b) = (t,) <$> mright t b
    go as [] = map fleft as
    go [] bs = map fright bs
    go as@((ta, a) : as') bs@((tb, b) : bs')
      | ta < tb   = fleft (ta, a) : go as' bs
      | ta > tb   = fright (tb, b) : go as bs'
      | otherwise = ((ta,) <$> mboth ta a b) : go as' bs'

-- | Simple (non-interpolating) merge, similar to zipWith.
--
-- Only the time stamps for which value is present in both series are
-- considered. Values are combined by the user-supplied function.
tsMergeWith :: (TSeries ts a, TSeries ts b, TSeries ts c)
           => (UTime -> a -> b -> c) -> ts a -> ts b -> ts c
tsMergeWith fboth tsa tsb = tsMerge merger tsa tsb
  where
    merger = TSMerge (const $ const Nothing) (const $ const Nothing) mboth
    mboth t a b = Just $ fboth t a b

-- | Merges two time series by extending/resampling them to match each other.
--
-- The two time series are extended/resampled with 'tsResampleLocal'
-- using the provided interpolators; and then merged with
-- 'tsMergeWith'.
tsMergeEnhance :: (TSeries ts a, TSeries ts b, TSeries ts c)
                  => (Bool, TSInterpolate a)
                  -> (Bool, TSInterpolate b)
                  -> (UTime -> a -> b -> c)
                  -> ts a -> ts b -> ts c
tsMergeEnhance aInterp bInterp fboth tsa tsb = tsMergeWith fboth tsaEnhanced tsbEnhanced
  where
    tsaEnhanced = tsResampleLocal (fst aInterp) (snd aInterp) (G.toList $ tsTimes tsb) tsa
    tsbEnhanced = tsResampleLocal (fst bInterp) (snd bInterp) (G.toList $ tsTimes tsa) tsb


--------------------------------------------------------------------------------
-- Resampling

-- | Resample or extend a time series to have values at the provided time stamps.
--
-- Resampling is done by locally interpolating between the two
-- neighboring events (using the provided @interpolator@).
--
-- If the @extend?@ argument is @True@, the original time series is
-- extended; otherwise the result will have values only at the new
-- time stamps (and the elements of the original time series are
-- discarded if they don't appear in the provided time stamp
-- sequence).
tsResampleLocal :: TSeries ts a
                   => Bool             -- ^ extend?
                   -> TSInterpolate a  -- ^ interpolator
                   -> [UTime]        -- ^ time stamp sequence
                   -> ts a -> ts a
tsResampleLocal keepOriginal interp times series
  = fromSortedPairList $ catMaybes $ go False (toPairList series) times
  where
    go _ [] _ = []
    go eLast (p : ps) []
      | not keepOriginal = []
      | otherwise = map Just $ if eLast then ps else p : ps
    go eLast ps@(p1@(t1, _) : ps') ts@(t : ts')
      | t1 < t, keepOriginal, not eLast = Just p1 : go True ps ts
      | t < t1  = tsiBefore interp t p1 : go eLast ps ts'
      | t == t1 = Just p1 : go False ps' ts'
      | otherwise = case ps' of
        [] -> map (flip (tsiAfter interp) p1) ts
        (p2@(t2, _) : _)
          | t < t2    -> tsiBetween interp t p1 p2 : go eLast ps ts'
          | otherwise -> go False ps' ts

-- | Extend/resample a time series by copying values forward.
extendForward :: TSeries ts a => Bool -> [UTime] -> ts a -> ts a
extendForward keepOriginal = tsResampleLocal keepOriginal interpForward
  where
    interpForward = TSInterpolate tsiNoExtend tsiExtend keepLeft
    keepLeft t (_,x) _ = Just (t, x)

-- | Extend/resample a time series by copying values backward.
extendBackward :: TSeries ts a => Bool -> [UTime] -> ts a -> ts a
extendBackward keepOriginal = tsResampleLocal keepOriginal interpBackward
  where
    interpBackward = TSInterpolate tsiExtend tsiNoExtend keepRight
    keepRight t _ (_,x) = Just (t, x)

-- | Resample a time series to have values at provided time stamps.
--
-- For every new time stamp a user supplied function is evaluated with
-- that time stamp and two sub-series: one containing every event
-- strictly before the time stamp, and one containing events at or
-- after the time stamp. The results are collected to create the
-- resulting time series.
tsResampleGlobal :: TSeries ts a
                    => (UTime -> ts a -> ts a -> Maybe (UTime, a))
                    -> [UTime]
                    -> ts a -> ts a
tsResampleGlobal sample times series = fromSortedPairList $ catMaybes $ map sliceOn times
  where
    sliceOn t = sample t before after
      where
        (before, after) = tsSplitAt t series

-- | Resamples a time series by calculating aggregates over a moving
-- window of a given duration at the given time stamps.
--
-- If the time stamps for the window positions should be periodic too, you can use
-- @'psOver' ('tsRange' series)@ as the @times@ argument.
tsResampleMoving :: TSeries ts a
                  => (UTime -> ts a -> Maybe a)
                  -> Period      -- ^ window duration
                  -> [UTime]     -- ^ @times@: time stamps for the window ends
                  -> ts a -> ts a
tsResampleMoving sample p = tsResampleGlobal gSample
  where
    gSample t before _ = (t,) <$> sample t window
      where
        (_, window) = tsSplitAt (periodStepBackUTime p t) before



--------------------------------------------------------------------------------
-- Shift time series in time.

-- | Shift time series in time by applying the user provided function
-- to all the time stamps.
--
-- Prerequisite: the provided time-modifying function has to be monotone.
tsOffsetGeneral :: TSeries ts a
                   => (UTime -> UTime)
                   -> ts a -> ts a
tsOffsetGeneral f = tsTraversedWithIndex . _1 %~ f

tsOffsetByPeriod :: TSeries ts a
                    => Period
                    -> ts a -> ts a
tsOffsetByPeriod p = tsOffsetGeneral $ periodStepUTime p
