module Data.Formatter.Interval
  ( unformatRecurringInterval
  , unformatInterval
  , formatInterval
  , formatRecurringInterval
  ) where

import Prelude

import Data.Formatter.Parser.Utils (runP)
import Data.Formatter.DateTime as FDT
import Data.Interval as I
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Map (lookup)
import Data.Int as Int
import Data.Foldable (foldMap)
import Data.Formatter.Parser.Interval (parseRecurringInterval, parseInterval, parseIsoDuration, parseDateTime, extendedDateTimeFormatInUTC)

formatRecurringInterval :: I.RecurringInterval I.IsoDuration DateTime -> String
formatRecurringInterval (I.RecurringInterval n i) = "R" <> (maybe "" formatInteger n) <> "/" <> (formatInterval i)

formatInterval :: I.Interval I.IsoDuration DateTime -> String
formatInterval (I.StartEnd x y) = (formatDateTime x) <> "/" <> (formatDateTime y)
formatInterval (I.DurationEnd d x) = (formatIsoDuration d) <> "/" <> (formatDateTime x)
formatInterval (I.StartDuration x d) = (formatDateTime x) <> "/" <> (formatIsoDuration d)
formatInterval (I.JustDuration d) = (formatIsoDuration d)

formatInteger :: Int -> String
formatInteger = show

formatNumber :: Number -> String
formatNumber n = if Int.toNumber (Int.floor n) == n then show (Int.floor n) else show n

formatIsoDuration :: I.IsoDuration -> String
formatIsoDuration = formatDuration <<< I.unIsoDuration

formatDuration :: I.Duration -> String
formatDuration (I.Duration m) = "P" <> datePart <> timePart
  where
  datePart = componentToString `foldMap` dateComponentsToStr
  timePart = ("T" <> _) `ifmempty` (componentToString `foldMap` timeComponentsToStr)
  ifmempty _ a | a == mempty = mempty
  ifmempty f a = f a
  componentToString (Tuple k s) = maybe "" (formatComponent s) $ lookup k m
  formatComponent designator num = formatNumber num <> designator
  dateComponentsToStr = [ Tuple I.Year "Y", Tuple I.Month "M", Tuple I.Day "D" ]
  timeComponentsToStr = [ Tuple I.Hours "H", Tuple I.Minutes "M", Tuple I.Seconds "S" ]

formatDateTime :: DateTime -> String
formatDateTime = FDT.format extendedDateTimeFormatInUTC

unformatRecurringInterval :: String → Either String (I.RecurringInterval I.IsoDuration DateTime)
unformatRecurringInterval = runP $ parseRecurringInterval parseIsoDuration parseDateTime

unformatInterval :: String → Either String (I.Interval I.IsoDuration DateTime)
unformatInterval = runP $ parseInterval parseIsoDuration parseDateTime
