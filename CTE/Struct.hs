module CTE.Struct where

--  This provides a structure similar to tm_struct for decomposing CTE time.
--  It's in its own module to facilitate qualified importing.
--  Maybe can do something with records extensions?

--+
import BasePrelude

--  Used to represent a time.
--  Also, perhaps dangerously, a duration.

data Parts = Parts {
   , year      :: Integer
   , month     :: Int
   , day       :: Int
   , hours     :: Int
   , minutes   :: Int
   , seconds   :: Int
   , tierce    :: Int
   , subtierce :: Rational  -- ∈[0,1)
   } deriving stock (Eq, Ord, Show)

