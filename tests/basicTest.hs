{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main  where

import Control.Lens
import Data.Time
import Data.TimeSeries
import Data.TimeSeries.Class
import qualified Data.TimeSeries.Boxed as TSB
import qualified Data.TimeSeries.Storable as TSS
import Test.Hspec
import Test.HUnit

t1, t12, t2, t23, t3, tOld, tFar :: UTCTime
tOld = UTCTime (fromGregorian 1900 01 01) 0
t1 = UTCTime (fromGregorian 2014 05 05) 0
t12 = UTCTime (fromGregorian 2014 05 20) 3600
t2 = UTCTime (fromGregorian 2014 06 15) 0
t23 = UTCTime (fromGregorian 2014 06 20) 28800
t3 = UTCTime (fromGregorian 2014 07 01) 0
tFar = UTCTime (fromGregorian 2024 07 07) 43200

exAbstract :: TSeries ts Int => ts Int
exAbstract = fromUnsortedPairList [(t1, 10), (t3, 30), (t2, 20)]

ex2Abstract :: TSeries ts Int => ts Int
ex2Abstract = fromUnsortedPairList [(t12, 12), (t2, 2), (t23, 23)]

exDoubleAbstract :: (TSeries ts Int, TSeries ts Double) => ts Double
exDoubleAbstract = exAbstract & tsTraversed %~ fromIntegral

emptyAbstract :: TSeries ts Double => ts Double
emptyAbstract = fromSortedPairList []

type TSeriesTestable ts a = (TSeries ts a, Show (ts a), Eq (ts a))

mainAbstract :: forall ts. ( TSeriesTestable ts Double
                           , TSeriesTestable ts Int
                           , TSeriesTestable ts Bool) => ts Double -> Spec
mainAbstract _witness = do
  let ex = exAbstract :: ts Int
      ex2 = ex2Abstract :: ts Int
      exDouble = exDoubleAbstract :: ts Double
      empty = emptyAbstract :: ts Double

  describe "fromUnsortedPairList" $ do
    it "sorts" $ do
      [(t1, 10), (t2, 20), (t3, 30)] @=? toPairList ex

  describe "tsLength" $ do
    it "0 on empty" $ do
      0 @=? tsLength empty
    it "3 on example" $ do
      3 @=? tsLength ex

  describe "tsRange" $ do
    it "Nothing on empty" $ do
      Nothing @=? tsRange empty
    it "(t1,t3) on example" $ do
      Just (t1,t3) @=? tsRange ex

  describe "tsLookup with AtOrAfter" $ do
    let lookup' t = tsLookup ex (AtOrAfter t)
    it "Nothing on far" $ do
      Nothing @=? lookup' tFar
    it "exact" $ do
      Just (t2,20) @=? lookup' t2
    it "t2 on t12" $ do
      Just (t2,20) @=? lookup' t12
    it "first on old" $ do
      Just (t1, 10) @=? lookup' tOld

  describe "tsLookup with AtOrBefore" $ do
    let lookup' t = tsLookup ex (AtOrBefore t)
    it "Nothing on old" $ do
      Nothing @=? lookup' tOld
    it "exact" $ do
      Just (t2,20) @=? lookup' t2
    it "t1 on t12" $ do
      Just (t1,10) @=? lookup' t12
    it "last on far" $ do
      Just (t3, 30) @=? lookup' tFar

  let roundToTwoDigits :: Maybe (UTCTime, Double) -> Maybe (UTCTime, Int)
      roundToTwoDigits = fmap (mapSnd (round . (* 100)))
        where
          mapSnd f (a, b) = (a, f b)

  let interp1 t = interpolateAt interpolateLinear exDouble t
  describe "interpolateAt with interpolateLinear" $ do
    it "exact" $ do
      Just (t1,10.0) @=? interp1 t1
    it "Nothing on old" $ do
      Nothing @=? interp1 tOld
    it "Nothing on far" $ do
      Nothing @=? interp1 tFar
    it "interpolates" $ do
      roundToTwoDigits (Just (t23, 20 + 10/3)) @=? (roundToTwoDigits $ interp1 t23)

  let interp2 t = interpolateAt extendInterpolateLinear exDouble t
  describe "interpolateAt with extendInterpolateLinear" $ do
    it "exact" $ do
      Just (t1,10.0) @=? interp2 t1
    it "extends on old" $ do
      Just (tOld, 10) @=? interp2 tOld
    it "extends on far" $ do
      Just (tFar, 30) @=? interp2 tFar
    it "interpolates" $ do
      roundToTwoDigits (Just (t23, 20 + 10/3)) @=? (roundToTwoDigits $ interp2 t23)

  describe "tsMergeWith" $ do
    it "empty with empty on the right" $ do
      empty @=? tsMergeWith undefined ex empty
    it "empty with empty on the left" $ do
      empty @=? tsMergeWith undefined empty ex
    it "equates with itself" $ do
      fromSortedPairList [(t1,True), (t2, True), (t3, True)]
      @=? tsMergeWith (const (==)) ex ex

  describe "tsMerge" $ do
    it "does it right" $ do
      fromSortedPairList [(t1, 20), (t12, -12), (t2, 18), (t23, -23), (t3, 60)]
      @=? tsMerge
        (TSMerge (const $ Just . (2*)) (const $ Just . negate) (\_ a b -> Just (a - b)))
        ex ex2

main :: IO ()
main = hspec $ do
  describe "Boxed" $ mainAbstract (undefined :: TSB.TSeries Double)
  describe "Storable" $ mainAbstract (undefined :: TSS.TSeries Double)
