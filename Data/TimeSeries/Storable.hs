{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TimeSeries.Storable (
  TSeries(..),
  valuesAsHmatrixVector
  ) where

import Data.Typeable (Typeable)
import Data.TimeSeries.Class hiding (TSeries)
import Data.UTime (UTime)
import qualified Data.TimeSeries.Class as TSC
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Hybrid as H

-- | Unboxed time series that can hold 'VS.Storable' values.  The
-- internal representation is a parallel vector of 'UTime's and
-- the the unboxed 'VS.Storable' values.
newtype TSeries a = TSeries (H.Vector VS.Vector VS.Vector (UTime, a))
                    deriving (Eq, Show, Typeable)

type instance TSVector TSeries = H.Vector VS.Vector VS.Vector
type instance TSTimes TSeries = VS.Vector
type instance TSValues TSeries  = VS.Vector

instance VS.Storable a => TSC.TSeries TSeries a where
  toVector (TSeries v) = v
  fromVector = TSeries

  tsValues (TSeries v) = H.projectSnd v
  tsTimes (TSeries v) = H.projectFst v

  fromTimesValues ts vs = TSeries $ H.unsafeZip ts vs

-- | Convert a time series to a @VS.Vector@, by chopping of the time stamps.
--
-- This is the same as 'HMatrix.Vector', but by not declaring that as
-- the return value, we avoid an expensive dependency.
valuesAsHmatrixVector :: VS.Storable a => TSeries a -> VS.Vector a
valuesAsHmatrixVector = tsValues
{-# INLINE valuesAsHmatrixVector #-}
