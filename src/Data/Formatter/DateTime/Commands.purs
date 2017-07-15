module Data.Formatter.DateTime.Commands where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data FormatterCommand
  = YearFull
  | YearTwoDigits
  | YearAbsolute
  | MonthFull
  | MonthShort
  | MonthTwoDigits
  | DayOfMonthTwoDigits
  | DayOfMonth
  | UnixTimestamp
  | DayOfWeek
  | Hours24
  | Hours12
  | Meridiem
  | Minutes
  | MinutesTwoDigits
  | Seconds
  | SecondsTwoDigits
  | Milliseconds
  | MillisecondsShort
  | MillisecondsTwoDigits
  | Placeholder String

derive instance eqFormatterCommand ∷ Eq FormatterCommand
derive instance genericFormatterCommand ∷ Generic FormatterCommand _
instance showFormatterCommand ∷ Show FormatterCommand where
  show = genericShow
