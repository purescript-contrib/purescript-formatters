module Data.Formatter.Interval
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
import Data.Foldable (class Foldable, fold)
import Data.Formatter.Internal (digit, foldDigits)
import Data.Function (on)
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))


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

durationParser :: Array (Tuple (Number -> I.Duration) String) -> P.Parser String I.Duration
durationParser arr = arr
  <#> applyDurations
  # sequence
  <#> foldFoldableMaybe

applyDurations :: Tuple (Number -> I.Duration) String -> P.Parser String (Maybe I.Duration)
applyDurations (Tuple f c) = PC.optionMaybe $ PC.try (f <$> component c)

foldFoldableMaybe :: ∀ f a. (Foldable f, Monoid a) => f (Maybe a) -> a
foldFoldableMaybe = fold >>> unMaybe

unMaybe :: ∀ a. (Monoid a) => Maybe a -> a
unMaybe = maybe mempty id

component ∷ String → P.Parser String Number
component designator = number <* PS.string designator

tryM :: ∀ a. (Monoid a) => P.Parser String a → P.Parser String a
tryM p = PC.option mempty $ PC.try p

parseIsoDuration :: P.Parser String I.IsoDuration
parseIsoDuration = do
  dur ← parseDuration
  case I.mkIsoDuration dur of
    Nothing -> PC.fail "extracted Duration is not valid ISO duration"
    Just a -> pure a

parseDuration :: P.Parser String I.Duration
parseDuration =
  PS.string "P" *> (weekDuration <|> fullDuration) <* PS.eof
  where
    weekDuration = durationParser [ Tuple I.week "W" ]
    fullDuration = append <$> durationDatePart <*> durationTimePart
    durationDatePart = durationParser [ Tuple I.year "Y" , Tuple I.month "M" , Tuple I.day "D" ]
    durationTimePart = tryM $ PS.string "T" *>
      (durationParser [ Tuple I.hours "H" , Tuple I.minutes "M" , Tuple I.seconds "S" ])
