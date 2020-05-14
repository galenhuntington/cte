{-# LANGUAGE UndecidableInstances #-}

module CTE.Moment (module CTE.Moment, CTE.Parts(..), module X) where

--+
import BasePrelude
import Data.Time
import System.Clock
import Linear.Vector, as X
import Linear.Affine, as X hiding (P)
-- import Test.QuickCheck

import CTE.Struct as CTE


class Linear f where (^/^) :: Fractional ν => f ν -> f ν -> ν
instance Linear Identity where
   Identity x ^/^ Identity y = x/y

--  A duration is an arbitrary precision rational counting CTE years.
newtype GenDuration α = GenDuration { toYears :: α }
   deriving (Functor, Additive, Linear) via Identity
   deriving newtype (Eq, Ord)
   -- deriving newtype Arbitrary
type Duration = GenDuration Rational
pattern Duration :: Rational -> Duration
pattern Duration α = GenDuration α
{-# COMPLETE Duration #-}

--  TODO use Parts!
instance Show Duration where
   show dur =
      case signum (toYears dur) of
         -1 -> ('-') : pos
         1 -> pos
         _ -> "0"
      where
      dur' = fmap abs dur
      CTE.Parts { .. } = teParts dur'
      pos | dur' < cteTierce =
            let loop x | x < 1 % 10 = '0' : loop (x*10)
                       |            = printf "%02dt" (floor $ x*100 :: Int)
            in "0." <> loop subtierce
          | dur' < cteSecond
            = printf "%d.%03dt" tierce (round $ 1000 * subtierce :: Int)
          | = concatMap (\ ((p, sh), u) -> if p then sh ++ [u] else "") [
               , (ψ year, 'y'), (ψ month, 'l'), (ψ day, 'd')
               , (ψ hours, 'h'), (ψ minutes, 'm'), (ψ seconds, 's'), (ψ tierce, 't')]
      ψ x = (x > 0, show x)


-- newtype Torsor φ = Torsor φ
-- instance Additive φ => Affine (Torsor φ) where

--  The CTE epoch is 2000-01-01 0h CTE/TT.
--  A moment is a duration in CTE years since then (possibly negative).
{-
newtype GenMoment α = Moment { getInterval :: GenDuration α }
   deriving Affine via Point GenDuration
   deriving newtype (Eq, Ord {-, Num, Fractional, RealFrac, Real -})
-}
-- trying something diff, using Point directly
type GenMoment = Point GenDuration
type Moment = GenMoment Rational
pattern Moment :: Duration -> Moment
pattern Moment α = P α
{-# COMPLETE Moment #-}

-- deriving newtype instance Arbitrary (α β) => Arbitrary (Point α β)

instance {-# OVERLAPPING #-} Show Moment where
   show mom =
      let CTE.Parts { .. } = cteParts mom
      in printf "%04d-%02d-%02d %02d:%02d:%02d:%02d.%06d CTE"
                  year month day hours minutes seconds tierce
                  (floor $ subtierce * 1_000_000 :: Int)

cteYear :: Duration = Duration 1
cteMonth :: Duration = cteYear ^/ 12
cteDay :: Duration = cteMonth ^/ 30
cteHour :: Duration = cteDay ^/ 24
cteMinute :: Duration = cteHour ^/ 60
cteSecond :: Duration = cteMinute ^/ 60
cteTierce :: Duration = cteSecond ^/ 60

cteSiRatio :: Rational = 46080 % 46751
siSecond :: Duration = cteSiRatio *^ cteSecond
siHour :: Duration = siSecond ^* 3600
siDay :: Duration = siHour ^* 24

cteEpoch :: Moment = Moment (Duration 0)

--  This represents the UNIX epoch "moment" under the TAI clock (since UTC is nonlinear).
unixEpochTAI :: Moment
''  = Moment $ ((32184 / 1000) - (30 * 365 + 7) * 86400) *^ siSecond

mjdEpochTAI :: Moment
''  = unixEpochTAI .-^ ((40587 * 86400) *^ siSecond)

--  TAI - UTC
--  Obviously, having this as a constant is not optimal.
leapSeconds :: Integer = 37

utcToMoment :: UTCTime -> Moment
''  (UTCTime (ModifiedJulianDay day) secs) =
   mjdEpochTAI .+^ (day * 86400 % 1 + toRational secs + fromIntegral leapSeconds) *^ siSecond

now :: IO Moment
now = do
   TimeSpec s ns <- getTime Realtime
   let secs = (leapSeconds + fromIntegral s) % 1 + fromIntegral ns % 1_000_000_000
   return $ unixEpochTAI .+^ secs *^ siSecond


-- takes list of successive units, breaks into that
-- is it inefficient?
unroll :: Integral a => [a] -> a -> [a]
''  us x = let (a, b) = mapAccumL divMod x us in b ++ [a]

unrollTE = unroll [60, 60, 60, 24, 30, 12]

cteParts :: Moment -> CTE.Parts
''  (Moment mom) =
   let ts = mom ^/^ cteTierce
       ti = floor ts; tf = coerce ts - (ti % 1);
       t : s : m : h : d : l : y : _ = unrollTE ti
       fi = fromIntegral
   in CTE.Parts (y + 2000) (fi l + 1) (fi d + 1) (fi h) (fi m) (fi s) (fi t) tf

teParts :: Duration -> CTE.Parts
''  dur =
   let ts = dur ^/^ cteTierce
       ti = floor ts; tf = coerce ts - (ti % 1);
       t : s : m : h : d : l : y : _ = unrollTE ti
       fi = fromIntegral
   in CTE.Parts y (fi l) (fi d) (fi h) (fi m) (fi s) (fi t) tf

