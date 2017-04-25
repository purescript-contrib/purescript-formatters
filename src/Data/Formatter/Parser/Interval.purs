module Data.Formatter.Parser.Interval
  ( parseRecurringInterval
  , parseInterval
  , parseIsoDuration
  , parseDateTime
  , extendedDateTimeFormatInUTC
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
import Data.Either (Either, fromRight)
import Data.Formatter.DateTime (unformatParser, Formatter, parseFormatString)
import Data.DateTime (DateTime)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartialBecause)

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
    fullDuration = (append <$> durationDatePart <*> durationTimePart) `notEmpty` "must contain valid duration components"
    durationDatePart = PC.option mempty $ PC.try $ mkComponentsParser [ Tuple I.year "Y" , Tuple I.month "M" , Tuple I.day "D" ]
    durationTimePart = PC.option mempty $ (PC.try $ PS.string "T") *> (mkComponentsParser [ Tuple I.hours "H" , Tuple I.minutes "M" , Tuple I.seconds "S" ])


notEmpty :: ∀ a. Monoid a => Eq a => P.Parser String a -> String -> P.Parser String a
notEmpty p str = p >>= \x -> if x == mempty then P.fail str else pure x

mkComponentsParser :: Array (Tuple (Number -> I.Duration) String) -> P.Parser String I.Duration
mkComponentsParser arr = p `notEmpty` ("none of valid duration components (" <> (show $ snd <$> arr) <> ") were present")

  where
    p = arr <#> applyDurations # sequence <#> foldFoldableMaybe
    applyDurations :: Tuple (Number -> I.Duration) String -> P.Parser String (Maybe I.Duration)
    applyDurations (Tuple f c) = PC.optionMaybe $ PC.try (f <$> component c)

    foldFoldableMaybe :: ∀ f a. Foldable f => Monoid a => f (Maybe a) -> a
    foldFoldableMaybe = fold >>> unMaybe

    unMaybe :: ∀ a. Monoid a => Maybe a -> a
    unMaybe = maybe mempty id

    component ∷ String → P.Parser String Number
    component designator = parseNumber <* PS.string designator


-- parser for DateTime in UTC time zone using "extended format"
parseDateTime :: ∀ m. Monad m => P.ParserT String m DateTime
parseDateTime = unformatParser extendedDateTimeFormatInUTC

extendedDateTimeFormatInUTC ∷ Formatter
extendedDateTimeFormatInUTC = unEither $ parseFormatString "YYYY-MM-DDTHH:mm:ssZ"
  where
    unEither :: Either String Formatter -> Formatter
    unEither e = (unsafePartialBecause "(this must be unrechable) error in parsing ISO date format") (fromRight e)
    --TODO check why this are not working?
    -- unEither = (unsafePartialBecause "(this must be unrechable) error in parsing ISO date format") <<< fromRight
    -- unEither = fromRight >>> (unsafePartialBecause "(this must be unrechable) error in parsing ISO date format")
