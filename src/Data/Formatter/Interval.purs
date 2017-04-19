module Data.Formatter.Interval
  ( unformatRecurringInterval
  , unformatInterval
  , unformatDuration
  , class HasDuration
  , class HasDate
  , getDuration
  , getDate
  ) where

import Prelude

import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as PS
import Data.Formatter.Parser.Utils (runP)
import Data.Interval as I
import Data.DateTime as D
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Data.Formatter.DateTime (Formatter, unformatParser, parseFormatString)
import Data.Formatter.Parser.Interval (parseRecurringInterval, parseInterval, parseIsoDuration, parseDuration)

unformatRecurringInterval ::
  ∀ a b
  . HasDuration a
  ⇒ HasDate b
  ⇒ String
  → Either String (I.RecurringInterval a b)
unformatRecurringInterval = runP $ parseRecurringInterval getDuration getDate <* PS.eof

unformatInterval ::
  ∀ a b
  . HasDuration a
  ⇒ HasDate b
  ⇒ String
  → Either String (I.Interval a b)
unformatInterval = runP $ parseInterval getDuration getDate <* PS.eof

unformatDuration ::
  ∀ a
  . HasDuration a
  ⇒ String
  → Either String a
unformatDuration = runP $ getDuration <* PS.eof




class HasDuration a where
  getDuration :: P.Parser String a

instance hasDurationDuration :: HasDuration I.Duration where
  getDuration = parseDuration

instance hasDurationIsoDuration :: HasDuration I.IsoDuration where
  getDuration = parseIsoDuration


class HasDate a where
  getDate :: P.Parser String a

isoDateTimeFormatter ∷ Either String Formatter
isoDateTimeFormatter = parseFormatString "YYYY-MM-DDTHH:MM:SSZ"

instance hasDateDate :: HasDate D.DateTime where
  getDate = do
    case isoDateTimeFormatter of
      Right f -> unformatParser f
      Left e -> P.fail e

-- TODO
-- 2017-04-13T15:36:07+00:00
-- 2017-04-13T15:36:07Z


-- TODO implement date parsers

-- TODO instance for local versions
-- * LocalDate
-- * LocalDateTime

-- TODO Q? should we define for LocalTime and Time
