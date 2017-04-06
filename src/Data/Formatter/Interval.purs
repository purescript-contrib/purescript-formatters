module Data.Formatter.Interval
  -- TODO parser should't be exposed
  ( parseDuration
  ) where

import Prelude
import Data.Interval as I
import Math as Math
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Control.Alt ((<|>))
import Data.Array (some)
import Data.Function (on)
import Data.Formatter.Internal (digit, foldDigits)
import Data.Int (toNumber, floor)
import Data.Monoid (mempty)


numOfDigits ∷ Int → Int
numOfDigits 0 = 0
numOfDigits n = 1 + (floor $ log10 $ toNumber n)

log10 ∷ Number → Number
log10 n = Math.log10e * Math.log n

integer ∷ P.Parser String Int
integer = some digit <#> foldDigits

pow :: Int -> Int -> Number
pow = Math.pow `on` toNumber

fractional ∷ P.Parser String Number
fractional = integer <#> case _ of
  0 ->  0.0
  n -> (toNumber n) / (pow 10 $ numOfDigits n)

number ∷ P.Parser String Number
number = (+)
  <$> (integer <#> toNumber)
  <*> (PC.option 0.0 $ PC.try $ PS.oneOf ['.', ','] *> fractional)

component ∷ String → P.Parser String Number
component designator = number <* PS.string designator

tryOr :: ∀ a. a → P.Parser String a → P.Parser String a
tryOr a p = PC.option a $ PC.try p

parseDuration :: P.Parser String I.Duration
parseDuration = PS.string "P" *> (weekDuration <|> fullDuration) <* PS.eof
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
