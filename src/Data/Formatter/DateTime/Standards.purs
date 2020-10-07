-- | Formatters for standard date and time formats.
module Data.Formatter.DateTime.Standards
  ( rfc3339
  , rfc3339WithMilliseconds ) where

import Prelude

import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.List (List(..), (:))

-- | Formats according to RFC 3339, with no fractional seconds and always in
-- | UTC, which is always specified with "Z", not "+00:00".
-- |
-- | E.g. `"2019-03-14T21:01:42Z"` can be parsed by this formatter.
rfc3339 ∷ Formatter
rfc3339 = makeRfc3339 z

-- | Formats according to RFC 3339, with fractional seconds to 3 digits and
-- | always in UTC, which is always specified with "Z", not "+00:00".
-- |
-- | E.g. `"2019-03-14T21:01:42.820Z"` can be parsed by this formatter.
rfc3339WithMilliseconds ∷ Formatter
rfc3339WithMilliseconds = makeRfc3339 $ Placeholder "." : Milliseconds : z

makeRfc3339 ∷ List FormatterCommand → Formatter
makeRfc3339 tail =
  let dash = Placeholder "-"
      colon = Placeholder ":"
  in
  YearFull : dash : MonthTwoDigits : dash : DayOfMonthTwoDigits
    : Placeholder "T"
    : Hours24 : colon : MinutesTwoDigits : colon : SecondsTwoDigits
    : tail

z ∷ List FormatterCommand
z = Placeholder "Z" : Nil
