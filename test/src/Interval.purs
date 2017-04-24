module Test.Interval (intervalTest) where

import Prelude

import Data.DateTime (DateTime(..))
import Data.Interval as I
import Control.MonadZero (guard)
import Data.Foldable (for_)
import Data.Time (Time(..))
import Data.Date (canonicalDate)
import Data.Formatter.Interval (unformatDuration, unformatRecurringInterval, getDate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Enum (toEnum)
import Partial.Unsafe (unsafePartialBecause)
import Unsafe.Coerce (unsafeCoerce)
import Data.Formatter.Parser.Utils (runP)
import Control.Monad.Aff (Aff)
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

intervalTest ∷ ∀ e. Spec e Unit
intervalTest = describe "Data.Formatter.Interval" do
  it "should unformat valid durations" do
    for_ durations \d -> do
      (unformatDuration d.str) `shouldEqual` (Right d.dur)

  it "should unformat valid ISO DateTime" do
    for_ dates \d -> do
      (runP getDate d.str) `shouldEqual` (Right d.date)

  it "shouldn't unformat invalid ISO DateTime" do
    for_ invalidDurations \d -> do
      let dur = (unformatDuration d.str) :: Either String I.IsoDuration
      dur `shouldEqual` (Left $ "extracted Duration is not valid ISO duration@" <> d.pos)

  describe "Interval variations" do
    it "should unformat Interval.StartEnd" intervalStartEndTest
    it "should unformat Interval.DurationEnd" intervalDurationEndTest
    it "should unformat Interval.StartDuration" intervalStartDurationTest
    it "should unformat Interval.JustDuration" intervalJustDurationTest


unsafeMkToIsoDuration :: I.Duration -> I.IsoDuration
unsafeMkToIsoDuration d = unsafePartialBecause "the duration must be valid ISO duration" fromJust $ I.mkIsoDuration d

makeDateTime ∷ Int -> Int -> Int -> Int -> Int -> Int -> Int -> DateTime
makeDateTime year month day h m s ms=
  DateTime
    (canonicalDate (fromMaybe bottom $ toEnum year) (fromMaybe bottom $ toEnum month) (fromMaybe bottom $ toEnum day))
    (Time (fromMaybe bottom $ toEnum h) (fromMaybe bottom $ toEnum m) (fromMaybe bottom $ toEnum s) (fromMaybe bottom $ toEnum ms))

durations :: Array { str:: String, dur :: I.IsoDuration }
durations =
  [ { str: "P1W", dur: I.day 7.0 }
  , { str: "P1.0W", dur: I.day 7.0 }
  , { str: "P1DT1H1M1S", dur: I.day 1.0 <> I.hours 1.0 <> I.minutes 1.0 <> I.seconds 1.0 }
  , { str: "P1.9748600D", dur: I.day 1.97486 }
  , { str: "P1DT1H1M0S", dur: I.day 1.0 <> I.hours 1.0 <> I.minutes 1.0 <> I.seconds 0.0 }
  , { str: "P1DT1H1M1.5S", dur: I.day 1.0 <> I.hours 1.0 <> I.minutes 1.0 <> I.seconds 1.5 }
  , { str: "P1DT1H1.5M", dur: I.day 1.0 <> I.hours 1.0 <> I.minutes 1.5 }
  , { str: "P1DT1.5H", dur: I.day 1.0 <> I.hours 1.5 }
  ] <#> (\a -> a { dur = unsafeMkToIsoDuration a.dur })

invalidDurations :: Array { str :: String, pos :: String}
invalidDurations =
  [ { str: "P1DT1.5H0M1S", pos:"1:13" } -- TODO add some more from https://github.com/arnau/ISO8601/blob/master/spec/iso8601/duration_spec.rb
  ]

invalidIntervals :: Array String
invalidIntervals =
  [ -- TODO add some more from https://github.com/arnau/ISO8601/blob/master/spec/iso8601/time_interval_spec.rb
  ]

recurrences ∷ Array { str :: String, rec :: Maybe Int }
recurrences =
  [ {str: "1", rec: Just 1}
  , {str: "", rec: Nothing}
  , {str: "99", rec: Just 99}
  , {str: "7", rec: Just 7}
  ]

dates :: Array { str:: String, date :: DateTime }
dates =
  [ { str: "2015-07-22T00:00:00Z", date: makeDateTime 2015 7 22 0  0  0  0 }
  , { str: "2015-07-23T11:12:13Z", date: makeDateTime 2015 7 23 11 12 13 0 }
  , { str: "2015-07-29T13:00:00Z", date: makeDateTime 2015 7 29 13 0  0  0 }
  ]

forceIsoDuration :: ∀ a. I.Interval a DateTime -> I.Interval I.IsoDuration DateTime
forceIsoDuration = unsafeCoerce

intervalStartEndTest ∷ ∀ e. Aff e Unit
intervalStartEndTest = for_ items test
  where
  test ({ start, end, rec }) =
    shouldEqual
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> start.str <> "/" <> end.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.StartEnd start.date end.date)

  items = do
    start <- dates
    end <- dates
    rec <- recurrences
    guard $ start.str /= end.str -- investigatge if this is needed
    pure { start, end, rec}

intervalDurationEndTest ∷ ∀ e. Aff e Unit
intervalDurationEndTest = for_ items test
  where
  test ({ dur, end, rec }) =
    shouldEqual
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> dur.str <> "/" <> end.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.DurationEnd dur.dur end.date)

  items = do
    dur <- durations
    end <- dates
    rec <- recurrences
    pure { dur, end, rec}

intervalStartDurationTest ∷ ∀ e. Aff e Unit
intervalStartDurationTest = for_ items test
  where
  test ({ dur, start, rec }) =
    shouldEqual
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> start.str <> "/" <> dur.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.StartDuration start.date dur.dur)

  items = do
    dur <- durations
    start <- dates
    rec <- recurrences
    pure { dur, start, rec}

intervalJustDurationTest ∷ ∀ e. Aff e Unit
intervalJustDurationTest = for_ items test
  where
  test ({ dur, rec }) =
    shouldEqual
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> dur.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.JustDuration dur.dur)

  items = do
    dur <- durations
    rec <- recurrences
    pure { dur, rec}
