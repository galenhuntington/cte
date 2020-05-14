{-# LANGUAGE NamedFieldPuns, DeriveDataTypeable #-}

--  This is not actually CTE-related, but is for my Gryd calendar
--  (which I don't much use anymore).

module CTE.Date (
   module Data.ExtendedReal,
   Cutoff, Date(..), JulianDay, jdToDate, jdToDate', dateToJD, dateToJD',
   englishCutoff, gregorianCutoff,
   dualDate, today,
   --  Gryd stuff... future uncertain
   jdToGryd, jdToGrydwk, jdToGrydmon, grydMonth, weekdays,
   jdToDay,
   ) where

import BasePrelude
import Data.ExtendedReal

--  Library calendar functions.
import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.LocalTime


--  The notion of a "date" in a Julian-like calendar.

--  TODO any advantage in using Int8's?
data Date = Date { year :: Integer, month :: Int8, day :: Int8 }
   deriving stock (Eq, Ord)

instance Show Date where
   show (Date y m d) = printf "%04d-%02d-%02d" y m d

--  This may not deserve to be here.
instance Read Date where
   readsPrec _ s =
      case iterate f ([], s) !! 3 of
         (d:m:y:_, s') -> [(Date y (fromIntegral m) (fromIntegral d), s')]
         _ -> []
      where
         f (l, x) = case reads x of
            [(a, b)] -> (a : l, case b of '-':x -> x; '/':x -> x; x -> x)
            _ -> ([], "")

--  The argument is a Julian day to start Gregorian on.
--  Finite values before 0200 Mar 01 should not be used as they break uniqueness
--  (but they're not forbidden).  Days then to 0300 Feb 28 are the same.
type Cutoff = Extended JulianDay
type JulianDay = Integer

offset0, {- offset2k, -} offsetMJD :: JulianDay
offset0 = 1721117   --  Julian day for BC 0001 Feb 28
-- offset2k = dateToJD NegInf (Date 2000 1 1) -- don't seem to need?
offsetMJD = 2400000
julianEpoch :: Day = ModifiedJulianDay (- offsetMJD - 1)

-- cutoff must be >= 1 Mar 200 to make sense, >= 1 Mar 300 to make a difference
gregorianCutoff = Finite $ dateToJD NegInf $ Date 1582 10 15
englishCutoff = Finite $ dateToJD NegInf $ Date 1752 9 14

-- imported from perl, where it's overly complex for overflow control.
-- provides inverse of "timesdiv"-- thus "off by one" from usual
divmodrat :: Integer -> Integer -> Integer -> (Integer, Integer)
''  a b c = let
   (q1, r1) = divMod a b
   (q, r) = divMod (r1*c - 1) b
   in (q + q1*c, div r c)

-- takes list of successive units, build up to that
{-
roll :: Num a => [a] -> [a] -> a
''  us vs = sum $ zipWith (*) vs $ scanl (*) 1 us
-}


--  This was written by me, but could be replaced by library functions.
--  Doesn't do anything if illegal date provided.
dateToJD :: Cutoff -> Date -> Integer
''  cutoff (Date { year, month, day }) = jd where
   (yr, mo) = (if month<3 then (subtract 1)***(+12) else id) (year, toInteger month-3)
   jd' = toInteger day + (153*mo+2) `div` 5 + (yr*1461) `div` 4 + offset0
   adj = 1 - ((yr`div`100)*3-1)`div`4
   jd = jd' + case cutoff of NegInf -> adj; Finite d | d<=jd' -> adj; _ -> 0

--  Attempted replace with library functions.
--  Intended to be "inline-friendly" since a constant cutoff is the most common case.
dateToJD' :: Cutoff -> Date -> Integer
''  cut date@(Date { year, month, day }) =
   flip diffDays julianEpoch $
      (if case cut of
         Finite c -> date >= jdToDate PosInf c
         NegInf -> True
         PosInf -> False
         then fromGregorian else fromJulian)
         year (fromIntegral month) (fromIntegral day)

jdToDate :: Cutoff -> JulianDay -> Date
jdToDate cutoff jd =
  let jd' = jd - offset0
      adj = (((jd'-2)*4 `div` 146097)*3-1) `div` 4 - 1
      mjd = jd' + case cutoff of NegInf -> adj; Finite d | d<=jd -> adj; _ -> 0
      (year', yd) = ((`div`4).fromInteger) `second` ((mjd*4-1) `divMod` 1461)
      (month', day) = ((+4)***((+1).(`div`5))) $ ((yd-30)*5-1) `divMod` (153 :: Int16)
      (year, month) =
        (if month'<=12 then id else ((+1)***(subtract 12))) (year', month')
  in Date year (fromIntegral month) (fromIntegral day)

jdToDate' :: Cutoff -> JulianDay -> Date
''  cutoff jd = Date y (fromIntegral m) (fromIntegral d) where
   (y, m, d) =
      (if cutoff <= Finite jd then toGregorian else toJulian)
         $ addDays jd julianEpoch

-- assumes 2nd millennium date
dualDate :: Date -> String
''  dt@(Date y m d) =
   if (m, d) < (3, 25) then 
      let y' = map snd $ dropWhile (uncurry (==)) (zip (show (y-1)) (show y))
         in printf "%04d/%s-%02d-%02d" (y-1) (if length y'>2 then show y else y') m d
   else show dt

jdToDay :: JulianDay -> Day
''  jd = ModifiedJulianDay (jd - 2400001)


--  In local time zone.
today :: IO JulianDay
''  = (2400001+) <$> toModifiedJulianDay <$> localDay <$> zonedTimeToLocalTime <$> getZonedTime


--  Keep Gryd stuff for now.

jdToGryd jd = (subtract 4725) *** (+1) $ divmodrat (jd + 4711) 12053 33

jdToGrydwk :: (Integral a, Integral b) => Integer -> (Integer, a, b) 
''  jd =
   let (bw, wd) = (jd + 40881) `divMod` 7
       (by, w) = divmodrat bw 12053 231
   in (by - 4824, fromIntegral w, fromIntegral wd)

jdToGrydmon :: (Integral a, Integral b) => Integer -> (Integer, a, b)
''  jd =
   let (bw, wd) = divMod (jd + 40879) 7
       (by, w) = divmodrat bw 12053 231
       (mn, md) = divmodrat (w*7+wd+30) 59 2
   in (by-4824, fromIntegral mn, fromIntegral (md+1))

grydMonth m = (take 13 $ unfoldr (Just . splitAt 2) "NyPyVyGlFlPlMsTsFsHrBrFrYx") !! (m-1)

weekdays :: String
''  = "XMTWHFS"


--  QuickCheck tests.
{-
propDates x = jdToDate NegInf x == jdToDate' NegInf x && jdToDate PosInf x == jdToDate' PosInf x
propDateBij y x = (dateToJD' y (jdToDate y x)) == x && (dateToJD y (jdToDate' y x)) == x
propDateBijs x = propDateBij PosInf x && propDateBij NegInf x && propDateBij englishCutoff x
-}

