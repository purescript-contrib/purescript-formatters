module Data.Formatter.Parser.Interval
  ( parseRecurringInterval
  , parseInterval
  , parseIsoDuration
  ) where

import Prelude
import Data.Interval as I
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Control.Alt ((<|>))
import Data.Foldable (class Foldable, fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)

import Data.Formatter.Parser.Number (parseNumber, parseMaybeInteger)

parseRecurringInterval :: ∀ a b. P.Parser String a -> P.Parser String b -> P.Parser String (I.RecurringInterval a b)
parseRecurringInterval duration date =
  I.RecurringInterval <$> (PS.string "R" *> parseMaybeInteger) <*> (PS.string "/" *> parseInterval duration date)

parseInterval :: ∀ a b. P.Parser String a -> P.Parser String b -> P.Parser String (I.Interval a b)
parseInterval duration date = [startEnd, durationEnd, startDuration, justDuration] <#> PC.try # PC.choice
  where
    startEnd = I.StartEnd <$> date <* PS.string "/" <*> date
    durationEnd = I.DurationEnd <$> duration <* PS.string "/" <*> date
    startDuration = I.StartDuration <$> date <* PS.string "/" <*> duration
    justDuration = I.JustDuration <$> duration

parseIsoDuration :: P.Parser String I.IsoDuration
parseIsoDuration = do
  dur ← parseDuration
  case I.mkIsoDuration dur of
    Nothing -> P.fail "extracted Duration is not valid ISO duration"
    Just a -> pure a

parseDuration :: P.Parser String I.Duration
parseDuration = PS.string "P" *> (weekDuration <|> fullDuration)
  where
    weekDuration = mkComponentsParser [ Tuple I.week "W" ]
    fullDuration = append <$> durationDatePart <*> durationTimePart
    durationDatePart = mkComponentsParser [ Tuple I.year "Y" , Tuple I.month "M" , Tuple I.day "D" ]
    durationTimePart = tryM $ PS.string "T" *>
      (mkComponentsParser [ Tuple I.hours "H" , Tuple I.minutes "M" , Tuple I.seconds "S" ])


mkComponentsParser :: Array (Tuple (Number -> I.Duration) String) -> P.Parser String I.Duration
mkComponentsParser arr = do
  dur <- arr <#> applyDurations # sequence <#> foldFoldableMaybe
  if dur == mempty
    then P.fail $ "none of valid duration components (" <> (show $ snd <$> arr) <> ") were present"
    else pure dur
  where
    applyDurations :: Tuple (Number -> I.Duration) String -> P.Parser String (Maybe I.Duration)
    applyDurations (Tuple f c) = PC.optionMaybe $ PC.try (f <$> component c)

    foldFoldableMaybe :: ∀ f a. Foldable f => Monoid a => f (Maybe a) -> a
    foldFoldableMaybe = fold >>> unMaybe

    unMaybe :: ∀ a. Monoid a => Maybe a -> a
    unMaybe = maybe mempty id

    component ∷ String → P.Parser String Number
    component designator = parseNumber <* PS.string designator

tryM :: ∀ a. Monoid a => P.Parser String a → P.Parser String a
tryM p = PC.option mempty $ PC.try p
