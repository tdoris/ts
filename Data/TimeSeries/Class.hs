{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TimeSeries.Class (
  TSeries(..),
  TSTimes,
  TSVector,
  TSValues,
  ) where

import Data.Vector.Generic (Vector)
import Data.UTime (UTime)

----------------------------------------
-- The main type class

-- | The @hybrid-vector@ type backing the time series.
type family TSVector (ts :: * -> *) :: * -> *
-- | The @vector@ type backing the 'UTime's in the time series.
type family TSTimes (ts :: * -> *) :: * -> *
-- | The @vector@ type backing the values in the time series.
type family TSValues (ts :: * -> *) :: * -> *

-- | The time series type class, currently with two implementations:
-- Boxed and Unboxed (Storable).
--
-- The type variable @ts@ denotes the specific time series type and
-- @a@ denotes the stored values.
class (Vector (TSVector ts) (UTime, a),
       Vector (TSTimes ts) UTime,
       Vector (TSValues ts) a) => TSeries ts a where

  fromVector :: TSVector ts (UTime, a) -> ts a
  toVector :: ts a -> TSVector ts (UTime, a)

  tsTimes :: ts a -> TSTimes ts UTime
  tsValues :: ts a -> TSValues ts a

  -- | The two vectors are assumed to have the same length.  This is
  -- not checked!
  fromTimesValues :: TSTimes ts UTime -> TSValues ts a -> ts a
