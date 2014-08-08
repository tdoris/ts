{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TimeSeries.Boxed (
  TSeries(..),
  ) where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Typeable (Typeable)
import Data.TimeSeries.Class hiding (TSeries)
import Data.UTime (UTime)
import qualified Data.TimeSeries.Class as TSC
import qualified Data.Vector as V
import qualified Data.Vector.Hybrid as H
import qualified Data.Vector.Storable as VS
import Prelude hiding (foldr)

-- | Boxed time series that can hold any kind of value.  The internal
-- representation is a parallel vector of 'UTime's and @a@s. The
-- vector of 'UTime's is storable 'VS.Vector' for efficiency and
-- interoperability reasons.
newtype TSeries a = TSeries (H.Vector VS.Vector V.Vector (UTime, a))
                    deriving (Eq, Show, Typeable)

type instance TSVector TSeries = H.Vector VS.Vector V.Vector
type instance TSTimes TSeries = VS.Vector
type instance TSValues TSeries  = V.Vector

instance TSC.TSeries TSeries a where
  toVector (TSeries v) = v
  fromVector = TSeries

  tsValues (TSeries v) = H.projectSnd v
  tsTimes (TSeries v) = H.projectFst v

  fromTimesValues ts vs = TSeries $ H.unsafeZip ts vs

instance Functor TSeries where
  fmap f ts = fromTimesValues times $ fmap f values
    where
      (times, values) = (tsTimes ts, tsValues ts)

instance Foldable TSeries where
  foldr f b = foldr f b . tsValues

instance Traversable TSeries where
  traverse f ts = fromTimesValues times <$> traverse f values
    where
      (times, values) = (tsTimes ts, tsValues ts)

--------------------------------------------------------------------------------
-- *WithIndex instances

instance TraversableWithIndex UTime TSeries where
  itraverse f (TSeries v) =
    fromVector . H.fromListN (H.length v) <$> traverse (itraverse f) (H.toList v)

instance FunctorWithIndex UTime TSeries

instance FoldableWithIndex UTime TSeries
