module Test.Interval (intervalTest) where

import Prelude

import Effect.Aff (Aff)
import Data.DateTime (DateTime)
import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable, fold)
import Data.Formatter.Interval (unformatInterval, unformatRecurringInterval, formatRecurringInterval)
import Data.Formatter.Parser.Interval (parseIsoDuration)
import Data.Formatter.Parser.Utils (runP)
import Data.Interval as I
import Data.Interval.Duration.Iso (IsoDuration, mkIsoDuration)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (forAll, makeDateTime)

prop ∷ ∀ e f. Foldable f ⇒ String → f {str ∷ String | e} → ({str ∷ String | e} → Aff Unit) → Spec Unit
prop = forAll (show <<< _.str)

intervalTest ∷ Spec Unit
intervalTest = describe "Data.Formatter.Interval" do
  prop "shouldn't unformat invalid Interval" invalidIntervals \({str, err}) → do
    (unformatInterval str) `shouldEqual` (Left $ err)

  prop "shouldn't unformat invalid Duration" invalidDurations \({str, err}) → do
    (runP parseIsoDuration str) `shouldEqual` (Left $ err)

  prop "should unformat RecurringInterval" arbRecurringInterval \({ str, interval }) → do
    (unformatRecurringInterval str) `shouldEqual` (Right interval)

  prop "format (unformat s) = s" arbRecurringInterval \({ str, interval, formatedStr }) → do
    (formatRecurringInterval <$> (unformatRecurringInterval str)) `shouldEqual` (Right formatedStr)

  prop "unformat (format s) = s" arbRecurringInterval \({ str, interval, formatedStr }) → do
    (unformatRecurringInterval $ formatRecurringInterval interval) `shouldEqual` (Right interval)


unsafeMkToIsoDuration ∷ I.Duration → IsoDuration
unsafeMkToIsoDuration d = mkIsoDuration d
  # unsafePartial fromRight -- the duration must be valid ISO duration

durations ∷ Array { str∷ String, formatedStr∷ String, dur ∷ IsoDuration }
durations =
  [ { str: "P1W", formatedStr: "P1W", dur: I.week 1.0 }
  , { str: "P1.0W", formatedStr: "P1W", dur: I.week 1.0 }
  , { str: "P1DT1H1M1S", formatedStr: "P1DT1H1M1S", dur: I.day 1.0 <> I.hour 1.0 <> I.minute 1.0 <> I.second 1.0 }
  , { str: "P1.9748600D", formatedStr: "P1.97486D", dur: I.day 1.97486 }
  , { str: "P1DT1H1M0S", formatedStr: "P1DT1H1M0S", dur: I.day 1.0 <> I.hour 1.0 <> I.minute 1.0 <> I.second 0.0 }
  , { str: "P1DT1H1M1.5S", formatedStr: "P1DT1H1M1.5S", dur: I.day 1.0 <> I.hour 1.0 <> I.minute 1.0 <> I.second 1.5 }
  , { str: "P1DT1H1.5M", formatedStr: "P1DT1H1.5M", dur: I.day 1.0 <> I.hour 1.0 <> I.minute 1.5 }
  , { str: "P1DT1.5H", formatedStr: "P1DT1.5H", dur: I.day 1.0 <> I.hour 1.5 }
  , { str: "PT1M", formatedStr: "PT1M", dur: I.minute 1.0 }
  , { str: "PT1S", formatedStr: "PT1S", dur: I.second 1.0 }
  , { str: "PT1H1S", formatedStr: "PT1H1S", dur: I.hour 1.0 <> I.second 1.0 }
  ] <#> (\a → a { dur = unsafeMkToIsoDuration a.dur })

invalidDurations ∷ Array { err ∷ String, str ∷ String}
invalidDurations =
  [ { err: errInvalidISO "Hour" <> "(line 1, col 13)", str: "P1DT1.5H0M1S" }
  , { err: errInvalidISO "Year" <> "(line 1, col 10)", str: "P1.5Y0.5M" }
  , { err: errInvalidISO "Year" <> "(line 1, col 8)", str: "P1.5Y1M" }
  , { err: errInvalidISO "Month" <> "(line 1, col 12)", str: "P1.5MT10.5S" }
  , { err: errInvalidComponent <> "(line 1, col 2)", str: "P" }
  , { err: errInvalidComponent <> "(line 1, col 2)", str: "PW" }
  , { err: errInvalidComponent <> "(line 1, col 2)", str: "PD" }
  , { err: errNoTimeComponent <> "(line 1, col 3)", str: "PT" }
  , { err: errNoTimeComponent <> "(line 1, col 3)", str: "PTH" }
  , { err: errNoTimeComponent <> "(line 1, col 5)", str: "P1YT" }
  , { err: errPrefix <> "(line 1, col 1)", str: "" }
  , { err: errPrefix <> "(line 1, col 1)", str: "T" }
  , { err: errPrefix <> "(line 1, col 1)", str: "~P1Y" }
  , { err: errPrefix <> "(line 1, col 1)", str: ".P1Y" }
  , { err: errEOF <> "(line 1, col 4)", str: "P1Y1W" }
  ]
  where
  errInvalidComponent = "Must contain valid duration components "
  errPrefix = "Expected \"P\" "
  errEOF = "Expected EOF "
  errInvalidISO c =
    "Extracted Duration is not valid ISO duration " <>
    "(Invalid usage of Fractional value at component `" <> c <> "`) "
  errNoTimeComponent = "None of valid duration components ([\"H\",\"M\",\"S\"]) were present "

invalidIntervals ∷ Array {err ∷ String, str ∷ String}
invalidIntervals =
  [ { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00ZP1Y2M10DT2H30M" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z-P1Y2M10D" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z~P1Y2M10D" }
  , { err: "Expected EOF (line 1, col 15)", str: "P1Y2M10DT2H30M2007-03-01T13:00:00Z" }
  , { err: "Expected EOF (line 1, col 9)", str: "P1Y2M10D-2007-03-01T13:00:00Z" }
  , { err: "Expected EOF (line 1, col 9)", str: "P1Y2M10D~2007-03-01T13:00:00Z" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z2008-05-11T15:30:00Z" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z-2008-05-11T15:30:00Z" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z~2008-05-11T15:30:00Z" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z/" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z/P" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z/PT" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z/2010-0-09" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z/2010-05-09T103012+0400" }
  , { err: "Expected \"P\" (line 1, col 1)", str: "2007-03-01T13:00:00Z/2014-W15-02T10:11:12Z" }
  , { err: "Expected EOF (line 1, col 9)", str: "P1Y2M10D/P1Y2M10D" }
  , { err: "Expected EOF (line 1, col 8)", str: "P1Y0.5M/P1Y0.5M" }
  ]

recurrences ∷ Array { str ∷ String, rec ∷ Maybe Int }
recurrences =
  [ {str: "", rec: Nothing}
  , {str: "18", rec: Just 18}
  ]

dates ∷ Array { str∷ String, date ∷ DateTime }
dates =
  [ { str: "2015-07-23T11:12:13Z", date: makeDateTime 2015 7 23 11 12 13 0 }
  , { str: "2015-07-22T00:00:00Z", date: makeDateTime 2015 7 22 0  0  0  0 }
  ]

type ArbRecurringInterval = Array { str ∷ String, formatedStr ∷ String, interval ∷ I.RecurringInterval IsoDuration DateTime}
type ArbInterval = Array { str ∷ String, formatedStr ∷ String, interval ∷ I.Interval IsoDuration DateTime}

arbRecurringInterval ∷ ArbRecurringInterval
arbRecurringInterval = do
  rec ← recurrences
  i ← arbInterval
  pure
    { str : "R" <> rec.str <> "/" <> i.str
    , formatedStr : "R" <> rec.str <> "/" <> i.formatedStr
    , interval: I.RecurringInterval rec.rec i.interval
    }

arbInterval ∷ ArbInterval
arbInterval = fold
  [ arbIntervalStartEnd
  , arbIntervalDurationEnd
  , arbIntervalStartDuration
  , arbIntervalDurationOnly
  ]

arbIntervalStartEnd ∷ ArbInterval
arbIntervalStartEnd = do
  start ← dates
  end ← dates
  pure
    { str: start.str <> "/" <> end.str
    , formatedStr: start.str <> "/" <> end.str
    , interval: I.StartEnd start.date end.date
    }

arbIntervalDurationEnd ∷ ArbInterval
arbIntervalDurationEnd = do
  dur ← durations
  end ← dates
  pure
    { str: dur.str <> "/" <> end.str
    , formatedStr: dur.formatedStr <> "/" <> end.str
    , interval: I.DurationEnd dur.dur end.date
    }

arbIntervalStartDuration ∷ ArbInterval
arbIntervalStartDuration = do
  dur ← durations
  start ← dates
  pure
    { str:  start.str <> "/" <> dur.str
    , formatedStr:  start.str <> "/" <> dur.formatedStr
    , interval: I.StartDuration start.date dur.dur
    }

arbIntervalDurationOnly ∷ ArbInterval
arbIntervalDurationOnly = do
  dur ← durations
  pure
    { str: dur.str
    , formatedStr: dur.formatedStr
    , interval: I.DurationOnly dur.dur
    }
