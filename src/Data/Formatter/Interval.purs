module Data.Formatter.Interval
  -- TODO parser should't be exposed
  ( parseDuration
  ) where

import Prelude
import Data.Interval as I
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Control.Alt ((<|>))
import Data.Array (length, some)
import Data.Formatter.Internal (digit, foldDigits)
import Data.Int (toNumber)
import Data.Monoid (mempty)


nums ∷ P.Parser String Int
nums = foldDigits <$> some digit

-- TODO try to use unformatNumberParser here
number ∷ P.Parser String Number
number = do
  whole ← nums
  _ ← (PC.try $ PS.string ".") <|> (PC.try $ PS.string ",") <|> pure ""
  restdigits ← PC.try (some digit) <|> pure [0]
  let rest = foldDigits restdigits
  pure $ if rest == 0 then toNumber whole else toNumber whole + ((toNumber rest) / (toNumber $ length restdigits))



component ∷ String → P.Parser String Number
component designator = number <* PS.string designator

tryOr :: ∀ a. a → P.Parser String a → P.Parser String a
tryOr a p = PC.option a $ PC.try p

parseDuration :: P.Parser String (I.Duration)
-- parseDuration = PS.string "P" *> weekDuration
parseDuration = PS.string "P" *> (weekDuration <|> fullDuration)-- <* PS.eof
  where
    weekDuration :: P.Parser String I.Duration
    weekDuration = PC.try $ I.week <$> component "W"

    fullDuration ∷ P.Parser String I.Duration
    fullDuration = append <$> durationDatePart <*> durationTimePart

    durationDatePart ∷ P.Parser String I.Duration
    durationDatePart = (\y m d → I.year y <> I.month m <> I.day d)
      <$> (tryOr 0.0 $ component "Y")
      <*> (tryOr 0.0 $ component "M")
      <*> (tryOr 0.0 $ component "D")

    durationTimePart ∷ P.Parser String I.Duration
    durationTimePart = tryOr mempty $
      PS.string "T" *>
        pure (\h m s → I.hours h <> I.minutes m <> I.seconds s)
          <*> (tryOr 0.0 $ component "H")
          <*> (tryOr 0.0 $ component "M")
          <*> (tryOr 0.0 $ component "S")
