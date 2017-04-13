module Data.Formatter.Interval
  ( unformatRecurringInterval
  , unformatInterval
  , unformatDuration
  ) where

import Prelude
import Data.Interval as I
import Data.Formatter.Parser.Interval (parseRecurringInterval, parseInterval, parseIsoDuration, parseDuration)

unformatRecurringInterval ::
  ∀ a b
  . HasDuration a
  ⇒ HasDate b
  ⇒ String
  → Either String (RecurringInterval a b)
unformatRecurringInterval = run $ parseRecurringInterval getDuration getDate

unformatInterval ::
  ∀ a b
  . HasDuration a
  ⇒ HasDate b
  ⇒ String
  → Either String (Interval a b)
unformatInterval = run $ parseInterval getDuration getDate

unformatDuration ::
  ∀ a
  . HasDuration a
  ⇒ String
  → Either String a
unformatDuration = run getDuration


run :: Parser String a → String → Either String a
run p s = lmap P.parseErrorMessage $ P.runParser s p


class HasDuration a where
  getDuration :: Parser String a

instance hasDurationDuration :: HasDuration Duration where
  getDuration = parseDuration

instance hasDurationIsoDuration :: HasDuration IsoDuration where
  getDuration = parseIsoDuration


class HasDate a where
  getDate :: Parser String a

-- instance hasDateDate :: HasDate DateTime where
--   getDate = parseFormatString "YYYY-MM-DD`T`HH:MM:SS`Z`" >>=  (_ `unformat` str)
-- TODO
-- 2017-04-13T15:36:07+00:00
-- 2017-04-13T15:36:07Z


-- TODO implement date parsers

-- TODO instance for local versions
-- * LocalDate
-- * LocalDateTime

-- TODO Q? should we define for LocalTime and Time
