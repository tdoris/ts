module Main (main) where

import qualified Data.Set as S
import Data.Time
import Data.TimeSeries.Periodic
import Test.Hspec
import Test.HUnit

day :: Integer -> Int -> Int -> Day
day = fromGregorian

ut :: Integer -> Int -> Int -> DiffTime -> UTCTime
ut y m d dt = UTCTime (day y m d) dt

t :: Int -> Int -> DiffTime -> DiffTime
t h m s = 3600 * fromIntegral h + 60 * fromIntegral m + s

periodic :: Period -> UTCTime -> Int -> Int -> [UTCTime]
periodic p start d k = take k $ drop d $ psToList $ periodicSequence p start

main :: IO ()
main = hspec $ do
  describe "Seconds" $ do
    let d1 h m s = ut 2014 07 03 (t h m s)
        d2 h m s = ut 2014 07 04 (t h m s)
        d3 h m s = ut 2014 07 05 (t h m s)
    it "overlaps" $ do
      [d1 23 59 58.1, d1 23 59 59.1, d2 0 0 0.1, d2 0 0 1.1]
      @=? periodic (Seconds 1) (d1 23 59 58.1) 0 4
    it "overlaps after a day" $ do
      [d2 23 59 56.1, d2 23 59 59.1, d3 0 0 2.1, d3 0 0 5.1]
      @=? periodic (Seconds 3) (d1 23 59 56.1) (86400 `div` 3) 4

  describe "Hours" $ do
    let d1 d h = ut 2014 07 d (t h 5 10.11)
    it "overlaps" $ do
      [d1 3 22, d1 3 23, d1 4 0, d1 4 1]
      @=? periodic (Hours 1) (d1 3 22) 0 4
    it "overlaps, few days later" $ do
      [d1 8 20, d1 8 23, d1 9 2, d1 9 5]
      @=? periodic (Hours 3) (d1 3 20) 40 4

  let dd y m d = ut y m d (t 2 10 3.4)
  describe "Months" $ do
    it "overlaps" $ do
      [dd 2013 11 05, dd 2013 12 05, dd 2014 01 05, dd 2014 02 05]
      @=? periodic (Months 1) (dd 2013 11 05) 0 4
    it "clips" $ do
      [dd 2011 08 31, dd 2012 02 29, dd 2012 08 31, dd 2013 02 28]
      @=? periodic (Months 6) (dd 2010 08 31) 2 4

  describe "Years" $ do
    it "overlaps" $ do
      [dd 1999 11 05, dd 2000 11 05, dd 2001 11 05, dd 2002 11 05]
      @=? periodic (Years 1) (dd 1899 11 05) 100 4
    it "clips" $ do
      [dd 1880 02 29, dd 1890 02 28, dd 1900 02 28, dd 1910 02 28]
      @=? periodic (Years 10) (dd 1880 02 29) 0 4

  describe "Workdays" $ do
    it "overlaps" $ do
      [dd 2014 07 03, dd 2014 07 04, dd 2014 07 07, dd 2014 07 08]
      @=? periodic Workdays (dd 2014 06 26) 5 4
    it "clips forward" $ do
      [dd 2014 06 30, dd 2014 07 01, dd 2014 07 02, dd 2014 07 03]
      @=? periodic Workdays (dd 2014 06 29) 0 4

  describe "Weekdays" $ do
    let s = S.fromList [Monday, Wednesday, Friday]
    it "overlaps" $ do
      [dd 2014 06 30, dd 2014 07 02, dd 2014 07 04, dd 2014 07 07]
      @=? periodic (Weekdays s) (dd 2014 06 23) 3 4
    it "clips forward" $ do
      [dd 2014 07 02, dd 2014 07 04, dd 2014 07 07, dd 2014 07 09]
      @=? periodic (Weekdays s) (dd 2014 07 01) 0 4
