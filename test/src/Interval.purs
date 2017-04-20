module Test.Interval
  ( intervalTest
  ) where


import Prelude

import Data.DateTime (DateTime(..))
import Data.Interval as I
import Control.MonadZero (guard)
import Data.Foldable (for_)
import Data.Time (Time(..))
import Data.Date (canonicalDate)
import Data.Formatter.Interval (unformatDuration, unformatRecurringInterval, getDate, class HasDuration)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Enum (toEnum)
import Partial.Unsafe (unsafePartialBecause)
import Test.Test (Tests, assertEq, log)
import Unsafe.Coerce (unsafeCoerce)
import Data.Formatter.Parser.Utils (runP)

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

invalidDurations :: Array String
invalidDurations =
  [ "P1DT1.5H0M1S" -- TODO add some more from https://github.com/arnau/ISO8601/blob/master/spec/iso8601/duration_spec.rb
  ]

invalidIntervals :: Array String
invalidIntervals =
  [ "P1DT1.5H0M1S" -- TODO add some more from https://github.com/arnau/ISO8601/blob/master/spec/iso8601/time_interval_spec.rb
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

intervalStartEndTest ∷ ∀ e. Tests e Unit
intervalStartEndTest = for_ items test
  where
  test ({ start, end, rec }) =
    assertEq
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> start.str <> "/" <> end.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.StartEnd start.date end.date)

  items = do
    start <- dates
    end <- dates
    rec <- recurrences
    guard $ start.str /= end.str -- investigatge if this is needed
    pure { start, end, rec}

intervalDurationEndTest ∷ ∀ e. Tests e Unit
intervalDurationEndTest = for_ items test
  where
  test ({ dur, end, rec }) =
    assertEq
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> dur.str <> "/" <> end.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.DurationEnd dur.dur end.date)

  items = do
    dur <- durations
    end <- dates
    rec <- recurrences
    pure { dur, end, rec}

intervalStartDurationTest ∷ ∀ e. Tests e Unit
intervalStartDurationTest = for_ items test
  where
  test ({ dur, start, rec }) =
    assertEq
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> start.str <> "/" <> dur.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.StartDuration start.date dur.dur)

  items = do
    dur <- durations
    start <- dates
    rec <- recurrences
    pure { dur, start, rec}

--
intervalJustDurationTest ∷ ∀ e. Tests e Unit
intervalJustDurationTest = for_ items test
  where
  test ({ dur, rec }) =
    assertEq
      (unformatRecurringInterval $ "R" <> rec.str <> "/" <> dur.str)
      (Right $ I.RecurringInterval rec.rec $ forceIsoDuration $ I.JustDuration dur.dur)

  items = do
    dur <- durations
    rec <- recurrences
    pure { dur, rec}


intervalTest :: ∀ e. Tests e Unit
intervalTest = do
  log "- Data.Formatter.Interval"

  for_ durations \d -> do
    assertEq (unformatDuration d.str) (Right d.dur)

  for_ dates \d -> do
    assertEq (runP getDate d.str) (Right d.date)

  for_ invalidDurations \d -> do
    let dur = (unformatDuration d) :: Either String I.IsoDuration
    assertEq dur (Left "extracted Duration is not valid ISO duration")

  log "- Data.Formatter.Interval.StartEnd"
  intervalStartEndTest

  log "- Data.Formatter.Interval.DurationEnd"
  intervalDurationEndTest

  log "- Data.Formatter.Interval.StartDuration"
  intervalStartDurationTest

  log "- Data.Formatter.Interval.JustDuration"
  intervalJustDurationTest
