-- TODO commiting this temporarly as depending on my fork of datetime is
-- not possibel as this module is not updated to ps@0.11
module Data.Interval
  ( Interval(..)
  , RecurringInterval(..)
  , IsoDuration
  , unIsoDuration
  , mkIsoDuration
  , isValidIsoDuration
  , Duration
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
import Control.Extend (class Extend, (=>>))
import Data.Foldable (class Foldable, fold, foldMap, foldrDefault, foldMapDefaultL)
import Data.Bifoldable (class Bifoldable, bifoldrDefault, bifoldMapDefaultL)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.List ((:), reverse)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Additive (Additive(..))
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..), snd)
import Math as Math


data RecurringInterval d a = RecurringInterval (Maybe Int) (Interval d a)

data Interval d a
  = StartEnd      a a
  | DurationEnd   d a
  | StartDuration a d
  | JustDuration  d

instance showInterval ∷ (Show d, Show a) => Show (Interval d a) where
  show (StartEnd x y) = "(StartEnd " <> show x <> " " <> show y <> ")"
  show (DurationEnd d x) = "(DurationEnd " <> show d <> " " <> show x <> ")"
  show (StartDuration x d) = "(StartDuration " <> show x <> " " <> show d <> ")"
  show (JustDuration d) = "(JustDuration " <> show d <> ")"

instance functorInterval ∷ Functor (Interval d) where
  map = bimap id

instance bifunctorInterval ∷ Bifunctor Interval where
  bimap _ f (StartEnd x y) = StartEnd (f x) (f y )
  bimap g f (DurationEnd d x) = DurationEnd (g d) (f x )
  bimap g f (StartDuration x d) = StartDuration (f x) (g d)
  bimap g _ (JustDuration d) = JustDuration (g d)

instance foldableInterval ∷ Foldable (Interval d) where
  foldl f z (StartEnd x y) = (z `f` x) `f` y
  foldl f z (DurationEnd d x) = z `f` x
  foldl f z (StartDuration x d) = z `f` x
  foldl _ z _  = z
  foldr x = foldrDefault x
  foldMap = foldMapDefaultL

instance bifoldableInterval ∷ Bifoldable Interval where
  bifoldl _ f z (StartEnd x y) = (z `f` x) `f` y
  bifoldl g f z (DurationEnd d x) = (z `g` d) `f` x
  bifoldl g f z (StartDuration x d) = (z `g` d) `f` x
  bifoldl g _ z (JustDuration d)  = z `g` d
  bifoldr x = bifoldrDefault x
  bifoldMap = bifoldMapDefaultL

instance traversableInterval ∷ Traversable (Interval d) where
  traverse f (StartEnd x y) = StartEnd <$> f x  <*> f y
  traverse f (DurationEnd d x) = f x <#> DurationEnd d
  traverse f (StartDuration x d) = f x <#> (_ `StartDuration` d)
  traverse _ (JustDuration d)  = pure (JustDuration d)
  sequence = sequenceDefault

instance extendInterval ∷ Extend (Interval d) where
  extend f a@(StartEnd x y) = StartEnd (f a) (f a )
  extend f a@(DurationEnd d x) = DurationEnd d (f a )
  extend f a@(StartDuration x d) = StartDuration (f a) d
  extend f (JustDuration d) = JustDuration d


mkIsoDuration ∷ Duration → Maybe IsoDuration
mkIsoDuration d | isValidIsoDuration d = Just $ IsoDuration d
mkIsoDuration _ = Nothing

isFractional ∷ Number → Boolean
isFractional a = Math.floor a /= a

-- allow only last number to be fractional
isValidIsoDuration ∷ Duration → Boolean
isValidIsoDuration (Duration m) = Map.toAscUnfoldable m
  # reverse
  =>> (validateFractionalUse >>> Conj)
  # fold
  # unConj
  where
    unConj (Conj a) = a
    validateFractionalUse = case _ of
      (Tuple _ n):as | isFractional n → foldMap (snd >>> Additive) as == mempty
      _ → true

unIsoDuration ∷ IsoDuration → Duration
unIsoDuration (IsoDuration a) = a

data IsoDuration = IsoDuration Duration
derive instance eqIsoDuration ∷ Eq IsoDuration
instance showIsoDuration ∷ Show IsoDuration where
  show (IsoDuration d)= "(IsoDuration " <> show d <> ")"


data Duration = Duration (Map.Map DurationComponent Number)
-- TODO `day 1 == hours 24`
derive instance eqDuration ∷ Eq Duration

instance showDuration ∷ Show Duration where
  show (Duration d)= "(Duration " <> show d <> ")"

instance semigroupDuration ∷ Semigroup Duration where
  append (Duration a) (Duration b) = Duration $ Map.unionWith (+) a b

instance monoidDuration ∷ Monoid Duration where
  mempty = Duration mempty

data DurationComponent =  Seconds | Minutes | Hours | Day | Month | Year
derive instance eqDurationComponent ∷ Eq DurationComponent
derive instance ordDurationComponent ∷ Ord DurationComponent

instance showDurationComponent ∷ Show DurationComponent where
  show Year = "Year"
  show Month = "Month"
  show Day = "Day"
  show Hours = "Hours"
  show Minutes = "Minutes"
  show Seconds = "Seconds"


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

durationFromComponent ∷ DurationComponent → Number → Duration
-- durationFromComponent _ 0.0 = mempty
durationFromComponent k v= Duration $ Map.singleton k v
