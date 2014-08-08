{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Applicative
import Control.Lens
import Data.Time
import Data.UTime
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

deriving instance Arbitrary UTime

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> (ModifiedJulianDay <$> day) <*> (review dtPico <$> dayTime)
    where
      day = choose (-36500000, 36500000)    -- 100000 years
      dayTime = choose (0, 86400 * 10^(12::Int) - 1)

mkUtc :: Integer -> Int -> Int -> DiffTime -> UTCTime
mkUtc y m d dt = UTCTime (fromGregorian y m d) dt

withinTolerance :: UTCTime -> UTCTime -> Bool
withinTolerance t1 t2 = abs (diffUTCTime t1 t2) <= 0.0000005

main :: IO ()
main = hspec $ do
  describe "UTime" $ do
    it "epoch at 0" $ do
      mkUtc 1970 01 01 0 @=? fromUTime (UTime 0)
    it "microsecond based" $ do
      mkUtc 1970 01 01 0.000001 @=? fromUTime (UTime 1)
    it "OK on a random future date" $ do
      mkUtc 2155 07 07 7268.123456 @=? fromUTime (UTime 5854212068123456)
    it "OK on a random past date" $ do
      mkUtc 1834 05 17 14463.123456 @=? fromUTime (UTime $ -4279982336876544)

  describe "UTime and back" $ do
    it "identity when rounding" $ do
      property $ \t -> t == (toUTime . fromUTime $ t)
    it "identity when up" $ do
      property $ \t -> t == (toUTimeUp . fromUTime $ t)
    it "identity when down" $ do
      property $ \t -> t == (toUTimeDown . fromUTime $ t)
    it "succ when up'" $ do
      property $ \t -> succ t == (toUTimeUp' . fromUTime $ t)
    it "pred when down'" $ do
      property $ \t -> pred t == (toUTimeDown' . fromUTime $ t)

  -- TODO(klao): modify the 'Arbitrary' instance for UTCTime, so that
  -- strict inequalities have a better chance of failing _if_ they
  -- were false.
  describe "UTCTime and back" $ do
    it "approx identity" $ do
      property $ \t ->
        t `withinTolerance` (fromUTime . toUTime $ t)
    it "up when up" $ do
      property $ \t -> t <= (fromUTime . toUTimeUp $ t)
    it "strictly up when up'" $ do
      property $ \t -> t < (fromUTime . toUTimeUp' $ t)
    it "down when down" $ do
      property $ \t -> t >= (fromUTime . toUTimeDown $ t)
    it "strictly down when down'" $ do
      property $ \t -> t > (fromUTime . toUTimeDown' $ t)
