-- TODO commiting this temporarly as depending on my fork of datetime is
-- not possibel as this module is not updated to ps@0.11
module Data.Interval
  ( Duration
  , Interval(..)
  , RecurringInterval(..)
  , year
  , month
  , week
  , day
  , hours
  , minutes
  , seconds
  , milliseconds
  ) where

import Prelude

import Data.Foldable (class Foldable, foldrDefault, foldMapDefaultL)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Monoid (class Monoid, mempty)
import Control.Extend (class Extend)

import Data.Maybe (Maybe)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))


data RecurringInterval a = RecurringInterval (Maybe Int) (Interval a)

data Interval a
  = StartEnd      a a
  | DurationEnd   Duration a
  | StartDuration a Duration
  | JustDuration  Duration

instance showInterval ∷ (Show a) => Show (Interval a) where
  show (StartEnd x y) = "(StartEnd " <> show x <> " " <> show y <> ")"
  show (DurationEnd d x) = "(DurationEnd " <> show d <> " " <> show x <> ")"
  show (StartDuration x d) = "(StartDuration " <> show x <> " " <> show d <> ")"
  show (JustDuration d) = "(JustDuration " <> show d <> ")"

instance functorInterval ∷ Functor Interval where
  map f (StartEnd x y) = StartEnd (f x) (f y )
  map f (DurationEnd d x) = DurationEnd d (f x )
  map f (StartDuration x d) = StartDuration (f x) d
  map _ (JustDuration d) = JustDuration d

instance foldableInterval ∷ Foldable Interval where
  foldl f z (StartEnd x y) = (z `f` x) `f` y
  foldl f z (DurationEnd d x) = z `f` x
  foldl f z (StartDuration x d) = z `f` x
  foldl _ z _  = z
  foldr x = foldrDefault x
  foldMap = foldMapDefaultL

instance traversableInterval ∷ Traversable Interval where
  traverse f (StartEnd x y) = StartEnd <$> f x  <*> f y
  traverse f (DurationEnd d x) = f x <#> DurationEnd d
  traverse f (StartDuration x d) = f x <#> (_ `StartDuration` d)
  traverse _ (JustDuration d)  = pure (JustDuration d)
  sequence = sequenceDefault

instance extendInterval ∷ Extend Interval where
  extend f a@(StartEnd x y) = StartEnd (f a) (f a )
  extend f a@(DurationEnd d x) = DurationEnd d (f a )
  extend f a@(StartDuration x d) = StartDuration (f a) d
  extend f (JustDuration d) = JustDuration d


data Duration = Duration DurationIn
type DurationIn = List (Tuple DurationComponent Number)

-- TODO `day 1 == hours 24`
derive instance eqDuration ∷ Eq Duration
instance showDuration ∷ Show Duration where
  show (Duration d)= "(Duration " <> show d <> ")"

instance semigroupDuration ∷ Semigroup Duration where
  append (Duration a) (Duration b) = Duration (appendComponents a b)

instance monoidDuration ∷ Monoid Duration where
  mempty = Duration mempty

appendComponents ∷ DurationIn → DurationIn → DurationIn
appendComponents Nil x = x
appendComponents x Nil = x
appendComponents ass@(a:as) bss@(b:bs) = case a, b of
  Tuple aC aV, Tuple bC bV
    | aC >  bC → a : appendComponents as bss
    | aC <  bC → b : appendComponents ass bs
    | otherwise → Tuple aC (aV + bV) : appendComponents as bs

data DurationComponent =  Seconds | Minutes | Hours | Day | Month | Year

instance showDurationComponent ∷ Show DurationComponent where
  show Year = "Year"
  show Month = "Month"
  show Day = "Day"
  show Hours = "Hours"
  show Minutes = "Minutes"
  show Seconds = "Seconds"

derive instance eqDurationComponent ∷ Eq DurationComponent
derive instance ordDurationComponent ∷ Ord DurationComponent


week ∷ Number → Duration
week = durationFromComponent Day <<< (_ * 7.0)

year ∷ Number → Duration
year = durationFromComponent Year

month ∷ Number → Duration
month = durationFromComponent Month

day ∷ Number → Duration
day = durationFromComponent Day

hours ∷ Number → Duration
hours = durationFromComponent Hours

minutes ∷ Number → Duration
minutes = durationFromComponent Minutes

seconds ∷ Number → Duration
seconds = durationFromComponent Seconds

milliseconds ∷ Number → Duration
milliseconds = durationFromComponent Seconds <<< (_ / 1000.0)

durationFromComponent ∷ DurationComponent → Number →  Duration
durationFromComponent c 0.0 = mempty
durationFromComponent c n = Duration $ pure $ Tuple c n
