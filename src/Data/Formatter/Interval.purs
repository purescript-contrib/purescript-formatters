module Data.Formatter.Interval
  ( unformatRecurringInterval
  , unformatInterval
  ) where

import Prelude

import Data.Formatter.Parser.Utils (runP)
import Data.Interval as I
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.Parser.Interval (parseRecurringInterval, parseInterval, parseIsoDuration, parseDateTime)

-- formatRecurringInterval :: ∀ a b. RecurringInterval I.IsoDuration DateTime -> String
-- formatInterval :: ∀ a b. Interval I.IsoDuration DateTime -> String

unformatRecurringInterval :: String → Either String (I.RecurringInterval I.IsoDuration DateTime)
unformatRecurringInterval = runP $ parseRecurringInterval parseIsoDuration parseDateTime

unformatInterval :: String → Either String (I.Interval I.IsoDuration DateTime)
unformatInterval = runP $ parseInterval parseIsoDuration parseDateTime

-- TODO read iso spec and check if local datetimes or datetimes with offset are supported
-- 2017-04-13T15:36:07+00:00
-- 2017-04-13T15:36:07Z
-- TODO instance for local versions
-- * LocalDate
-- * LocalDateTime
