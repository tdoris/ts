{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Data.TimeSeries (
  -- * Simple accessors
  tsLength,
  (!),
  tsRange,
  tsTraversed,
  tsTraversedWithIndex,
  toPairList,
  -- * Lookup at time
  TSLook(..),
  tsSearch,
  tsLookup,
  -- * Construction
  fromSortedPairList,
  fromUnsortedPairList,
  TSU.fromPeriodicData,
  -- * Lookup with interpolation
  TSInterpolate(..),
  interpolateAt,
  tsiExtend,
  tsiNoExtend,
  interpolateLinear,
  extendInterpolateLinear,
  tsGet,
  -- * Slicing
  tsSlice,
  TSU.tsSliceByCount,
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
import Data.Time (UTCTime)
import Data.TimeSeries.Class
import Data.TimeSeries.Periodic
import qualified Data.TimeSeries.UTime as TSU
import Data.UTime
import qualified Data.Vector.Generic as G

ut2utc :: Setting (->) s t UTime UTCTime -> s -> t
ut2utc s = over s fromUTime

ut2utcL :: [(UTime, a)] -> [(UTCTime, a)]
ut2utcL = ut2utc (mapped._1)

utc2ut :: Setting (->) s t UTCTime UTime -> s -> t
utc2ut s = over s toUTime

utc2utL :: [(UTCTime, a)] -> [(UTime, a)]
utc2utL = utc2ut (mapped._1)


tsLength :: TSeries ts a => ts a -> Int
tsLength = TSU.tsLength
{-# INLINE tsLength #-}

(!) :: TSeries ts a => ts a -> Int -> (UTCTime, a)
(!) ts i = ut2utc _1 $ toVector ts G.! i
{-# INLINE (!) #-}

-- | Returns the first and last time stamp of a time series.
tsRange :: TSeries ts a => ts a -> Maybe (UTCTime, UTCTime)
tsRange = ut2utc (mapped.both) . TSU.tsRange

-- TODO(klao): Going through lists is probably inefficient! Check, and
-- go through streams it that's the case.

-- | Traversal of the values of a time series with access to the time stamp as index.
tsTraversed :: (TSeries ts a, TSeries ts b) => IndexedTraversal UTCTime (ts a) (ts b) a b
tsTraversed f ts = fromVector . G.fromListN (G.length v) <$> traverse g (G.toList v)
  where
    v = toVector ts
    g (t, x) = (t,) <$> indexed f (fromUTime t) x

-- | Traversal of ('UTCTime', @value@) pairs of a time series with
-- access to a positional index.
--
-- The user of this traversal should guarantee to not reorder the
-- events (ie. that the time stamps are modified in a monotonic way).
tsTraversedWithIndex :: (TSeries ts a, TSeries ts b)
                        => IndexedTraversal Int (ts a) (ts b) (UTCTime, a) (UTCTime, b)
tsTraversedWithIndex f ts
  = fromVector . G.fromListN (G.length v) . utc2utL <$> itraversed f (ut2utcL $ G.toList v)
  where
    v = toVector ts

-- | Construct a time series from a list of (time stamp, value) pairs
--
-- Precondition: the list /have to/ be sorted by time stamps.
fromSortedPairList :: TSeries ts a => [(UTCTime, a)] -> ts a
fromSortedPairList = TSU.fromSortedPairList . utc2utL

-- | Construct a time series from a list of (time stamp, value) pairs
--
-- The list is sorted by time stamps on construction.
fromUnsortedPairList :: TSeries ts a => [(UTCTime, a)] -> ts a
fromUnsortedPairList = TSU.fromUnsortedPairList . utc2utL

-- | Returns a list of (time stamp, value) pairs of a time series
-- sorted by time stamps.
toPairList :: TSeries ts a => ts a -> [(UTCTime, a)]
toPairList = ut2utcL . TSU.toPairList

--------------------------------------------------------------------------------
-- Lookup

data TSLook
  = AtOrAfter UTCTime
  | After UTCTime
  | AtOrBefore UTCTime
  | Before UTCTime
  deriving (Eq, Show)

-- | Returns the position in the time series corresponding to the
-- 'TSLook' parameter.
tsSearch :: TSeries ts a => ts a -> TSLook -> Int
tsSearch ts l = if before then i - 1 else i
  where
    (before, ut) = case l of
      AtOrAfter t    -> (False, toUTimeUp t)
      After t  -> (False, toUTimeUp' t)
      AtOrBefore t   -> (True, toUTimeUp' t)
      Before t -> (True, toUTimeUp t)
    i = TSU.tsSearch ts ut

-- | Returns the element of the time series corresponding to the
-- 'TSLook' parameter, or 'Nothing' if there is no suitable element.
--
-- Examples:
--
-- @tsLookup ts ('AtOrAfter' t)@ returns the /first/ element with the time
-- stamp not earlier than @t@.
--
-- @tsLookup ts ('StrictBefore' t)@ returns the /last/ element with
-- the time stamp strictly before @t@.
tsLookup :: TSeries ts a => ts a -> TSLook -> Maybe (UTCTime, a)
tsLookup ts l | i >= 0, i < tsLength ts  = Just (ts ! i)
              | otherwise  = Nothing
  where
    i = tsSearch ts l

--------------------------------------------------------------------------------
-- Slicing

-- | Returns the slice of the time series within the provided time interval.
--
-- The time interval is interpreted as half-open, ie. the element
-- corresponding to the start paramenter is included, but the one
-- corresponding to the end parameter is not. With this, using
-- 'AtOrAfter' and 'After' it is possible to synthesize both inclusive
-- and exclusive behavior at either end. Eg.:
--
-- @tsSlice ts (AtOrAfter t0) (After t1)@ returns series of all
-- elements with time stamp at least @t0@ and at most @t1@, both ends
-- included.
--
-- @tsSlice ts (After t0) (AtOrAfter t1)@ -- like before, but now both
-- @t0@ and @t1@ are excluded.
tsSlice :: TSeries ts a => ts a
        -> TSLook -- ^ start descriptor (inclusive)
        -> TSLook -- ^ end descriptor (exclusive)
        -> ts a
tsSlice ts lStart lEnd
  | start > end  = error "tsSlice: start position later than the end position"
  | otherwise  = fromVector $ G.slice start (end - start) $ toVector ts
  where
    start = max 0 $ tsSearch ts lStart
    end = max 0 $ tsSearch ts lEnd

-- | Split the time series into two parts at the position
-- corresponding to the 'TSLook' parameter.
--
-- The element that would have been returned by 'tsLookup' with the
-- same arguments is the first element of the second part.
tsSplitAt :: TSeries ts a => TSLook -> ts a -> (ts a, ts a)
tsSplitAt l ts = (before, after)
  where
    i = tsSearch ts l
    v = toVector ts
    (before, after) = G.splitAt i v & both %~ fromVector

--------------------------------------------------------------------------------
-- Interpolating

-- | Data type used to set up interpolation functions like
-- 'interpolateAt', 'tsMergeEnhance' and 'tsResampleLocal'.
data TSInterpolate a =
  TSInterpolate
  { tsiBefore  :: UTCTime -> (UTCTime, a) -> Maybe (UTCTime, a)
    -- ^ Function to interpolate for time stamps that are before the first item in the time series.
  , tsiAfter   :: UTCTime -> (UTCTime, a) -> Maybe (UTCTime, a)
    -- ^ Function to interpolate for time stamps that are after the last item in the time series.
  , tsiBetween :: UTCTime -> (UTCTime, a) -> (UTCTime, a) -> Maybe (UTCTime, a)
    -- ^ Function to interpolate for time stamps in the middle of the time series.
  }
  | TSInterpolateUT (TSU.TSInterpolate a)

tsi2ut :: TSInterpolate a -> TSU.TSInterpolate a
tsi2ut (TSInterpolateUT interp) = interp
tsi2ut (TSInterpolate before after between) = TSU.TSInterpolate before' after' between'
  where
    before' t = utc2ut (mapped._1) . before (fromUTime t) . ut2utc _1
    after' t = utc2ut (mapped._1) . after (fromUTime t) . ut2utc _1
    between' t (t0, a) = utc2ut (mapped._1) . between (fromUTime t) (fromUTime t0, a) . ut2utc _1

interpolateAt :: TSeries ts a => TSInterpolate a -- ^ Parameters to use during the interpolation.
              -> ts a                -- ^ Input time series to interpolate from.
              -> UTCTime             -- ^ Time stamp to interpolate at.
              -> Maybe (UTCTime, a)  -- ^ The result of interpolation.
interpolateAt inter ts = ut2utc (mapped._1) . TSU.interpolateAt (tsi2ut inter) ts . toUTime

-- | Defines trivial extending of a time series: uses the the same
-- value as the first/last item.
--
-- To be used as 'tsiBefore' or 'tsiAfter' field of a 'TSInterpolate'.
tsiExtend :: UTCTime -> (UTCTime, a) -> Maybe (UTCTime, a)
tsiExtend t (_, x) = Just (t, x)

-- | Defines non-extending time series.
--
-- To be used as 'tsiBefore' or 'tsiAfter' field of a 'TSInterpolate'.
tsiNoExtend :: UTCTime -> (UTCTime, a) -> Maybe (UTCTime, a)
tsiNoExtend = const $ const Nothing

-- | Linearly interpolates within time series; does not extend.
interpolateLinear :: Fractional a => TSInterpolate a
interpolateLinear = TSInterpolateUT TSU.interpolateLinear

-- | Linearly interpolates within time series; extends at the ends.
extendInterpolateLinear :: Fractional a => TSInterpolate a
extendInterpolateLinear = TSInterpolateUT TSU.extendInterpolateLinear

-- | Access any time stamp in the given time series, with linear
-- interpolation and trivial extension at the ends if needed.
tsGet :: (Fractional a, TSeries ts a) => ts a -> UTCTime -> a
tsGet ts = TSU.tsGet ts . toUTime

--------------------------------------------------------------------------------
-- Time series merging

-- | Structure describing a recipe for a generic merge.
data TSMerge a b c =
  TSMerge
  { tsmLeft  :: UTCTime -> a -> Maybe c
  , tsmRight :: UTCTime -> b -> Maybe c
  , tsmBoth  :: UTCTime -> a -> b -> Maybe c
  }

tsm2ut :: TSMerge a b c -> TSU.TSMerge a b c
tsm2ut (TSMerge mleft mright mboth) = TSU.TSMerge mleft' mright' mboth'
  where
    mleft' = mleft . fromUTime
    mright' = mright . fromUTime
    mboth' = mboth . fromUTime

-- | Generic (non-interpolating) merge.
--
-- Every time stamp considered independently from all the
-- other. Conversion or combination of values is made according to the
-- provided recipe, based on whether the value is present in one or
-- both time series.
tsMerge :: (TSeries ts a, TSeries ts b, TSeries ts c)
           => TSMerge a b c -> ts a -> ts b -> ts c
tsMerge = TSU.tsMerge . tsm2ut

-- | Simple (non-interpolating) merge, similar to zipWith.
--
-- Only the time stamps for which value is present in both series are
-- considered. Values are combined by the user-supplied function.
tsMergeWith :: (TSeries ts a, TSeries ts b, TSeries ts c)
           => (UTCTime -> a -> b -> c) -> ts a -> ts b -> ts c
tsMergeWith fboth = TSU.tsMergeWith (fboth . fromUTime)

-- | Merges two time series by extending/resampling them to match each other.
--
-- The two time series are extended/resampled with 'tsResampleLocal'
-- using the provided interpolators; and then merged with
-- 'tsMergeWith'.
tsMergeEnhance :: (TSeries ts a, TSeries ts b, TSeries ts c)
                  => (Bool, TSInterpolate a)
                  -> (Bool, TSInterpolate b)
                  -> (UTCTime -> a -> b -> c)
                  -> ts a -> ts b -> ts c
tsMergeEnhance aInterp bInterp fboth
  = TSU.tsMergeEnhance (over _2 tsi2ut aInterp) (over _2 tsi2ut bInterp) (fboth . fromUTime)

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
                   -> [UTCTime]        -- ^ time stamp sequence
                   -> ts a -> ts a
tsResampleLocal keepOriginal interp
  = TSU.tsResampleLocal keepOriginal (tsi2ut interp) . map toUTime

-- | Extend/resample a time series by copying values forward.
extendForward :: TSeries ts a => Bool -> [UTCTime] -> ts a -> ts a
extendForward keepOriginal = TSU.extendForward keepOriginal . map toUTime

-- | Extend/resample a time series by copying values backward.
extendBackward :: TSeries ts a => Bool -> [UTCTime] -> ts a -> ts a
extendBackward keepOriginal = TSU.extendBackward keepOriginal . map toUTime

-- | Resample a time series to have values at provided time stamps.
--
-- For every new time stamp a user supplied function is evaluated with
-- that time stamp and two sub-series: one containing every event
-- strictly before the time stamp, and one containing events at or
-- after the time stamp. The results are collected to create the
-- resulting time series.
tsResampleGlobal :: TSeries ts a
                    => (UTCTime -> ts a -> ts a -> Maybe (UTCTime, a))
                    -> [UTCTime]
                    -> ts a -> ts a
tsResampleGlobal sample = TSU.tsResampleGlobal sample' . map toUTime
  where
    sample' t tsl tsr = utc2ut (mapped._1) $ sample (fromUTime t) tsl tsr

-- | Resamples a time series by calculating aggregates over a moving
-- window of a given duration at the given time stamps.
--
-- If the time stamps for the window positions should be periodic too, you can use
-- @'psOver' ('tsRange' series)@ as the @times@ argument.
tsResampleMoving :: TSeries ts a
                  => (UTCTime -> ts a -> Maybe a)
                  -> Period      -- ^ window duration
                  -> [UTCTime]   -- ^ @times@: time stamps for the window ends
                  -> ts a -> ts a
tsResampleMoving sample p = TSU.tsResampleMoving (sample . fromUTime) p . map toUTime

--------------------------------------------------------------------------------
-- Shift time series in time.

-- | Shift time series in time by applying the user provided function
-- to all the time stamps.
--
-- Prerequisite: the provided time-modifying function has to be monotone.
tsOffsetGeneral :: TSeries ts a
                   => (UTCTime -> UTCTime)
                   -> ts a -> ts a
tsOffsetGeneral f = tsTraversedWithIndex . _1 %~ f

tsOffsetByPeriod :: TSeries ts a
                    => Period
                    -> ts a -> ts a
tsOffsetByPeriod p = tsOffsetGeneral $ periodStep p
