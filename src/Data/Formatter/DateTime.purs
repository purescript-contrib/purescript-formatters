module Data.Formatter.DateTime
  ( Formatter
  , FormatterCommand(..)
  , Meridiem
  , printFormatter
  , parseFormatString
  , format
  , formatDateTime
  , unformat
  , unformatDateTime
  , unformatParser
  ) where

import Prelude

import Control.Monad.State (State, modify, put, runState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Class (get)
import Data.Ord (abs)
import Data.Array as Array
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Tuple (Tuple(..))
import Data.Foldable (foldMap)
import Control.Alt ((<|>))
import Data.Date as D
import Data.DateTime as DT
import Data.DateTime.Instant (instant, toDateTime, fromDateTime, unInstant)
import Data.Either (Either(..), either)
import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Time as T
import Data.Time.Duration as Dur
import Data.Formatter.Internal (foldDigits)
import Data.Formatter.Parser.Number (parseDigit)
import Data.Formatter.Parser.Utils (runP, oneOfAs)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.String as PS

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data FormatterCommand
  = YearFull
  | YearTwoDigits
  | YearAbsolute
  | MonthFull
  | MonthShort
  | MonthTwoDigits
  | DayOfMonthTwoDigits
  | DayOfMonth
  | UnixTimestamp
  | DayOfWeek
  | Hours24
  | Hours12
  | Meridiem
  | Minutes
  | MinutesTwoDigits
  | Seconds
  | SecondsTwoDigits
  | Milliseconds
  | MillisecondsShort
  | MillisecondsTwoDigits
  | Placeholder String

derive instance eqFormatterCommand ∷ Eq (FormatterCommand)
derive instance genericFormatter ∷ Generic FormatterCommand _
instance showFormatter ∷ Show FormatterCommand where
  show = genericShow

type Formatter = List.List FormatterCommand

printFormatterCommand ∷ FormatterCommand → String
printFormatterCommand = case _ of
  YearFull → "YYYY"
  YearTwoDigits → "YY"
  YearAbsolute → "Y"
  MonthFull → "MMMM"
  MonthShort → "MMM"
  MonthTwoDigits → "MM"
  DayOfMonthTwoDigits → "DD"
  DayOfMonth → "D"
  UnixTimestamp → "X"
  DayOfWeek → "E"
  Hours24 → "HH"
  Hours12 → "hh"
  Meridiem → "a"
  Minutes → "m"
  MinutesTwoDigits → "mm"
  Seconds → "s"
  SecondsTwoDigits → "ss"
  MillisecondsShort → "S"
  MillisecondsTwoDigits → "SS"
  Milliseconds → "SSS"
  Placeholder s → s

printFormatter ∷ Formatter → String
printFormatter = foldMap printFormatterCommand

parseFormatString ∷ String → Either String Formatter
parseFormatString = runP formatParser

placeholderContent ∷ P.Parser String String
placeholderContent =
  Str.toCharArray "YMDEHhamsS"
  # PS.noneOf
  # Array.some
  <#> Str.fromCharArray

formatterCommandParser ∷ P.Parser String FormatterCommand
formatterCommandParser = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "YYYY" YearFull
  , Tuple "YY" YearTwoDigits
  , Tuple "Y" YearAbsolute
  , Tuple "MMMM" MonthFull
  , Tuple "MMM" MonthShort
  , Tuple "MM" MonthTwoDigits
  , Tuple "DD" DayOfMonthTwoDigits
  , Tuple "D" DayOfMonth
  , Tuple "E" DayOfWeek
  , Tuple "HH" Hours24
  , Tuple "hh" Hours12
  , Tuple "a" Meridiem
  , Tuple "mm" MinutesTwoDigits
  , Tuple "m" Minutes
  , Tuple "ss" SecondsTwoDigits
  , Tuple "s" Seconds
  , Tuple "SSS" Milliseconds
  , Tuple "SS" MillisecondsTwoDigits
  , Tuple "S" MillisecondsShort
  ] <|> (Placeholder <$> placeholderContent)

formatParser ∷ P.Parser String Formatter
formatParser = List.some formatterCommandParser

-- | Formatting function that accepts a number that is a year,
-- | and strips away the non-significant digits, leaving only the
-- | ones and tens positions.
formatYearTwoDigits ∷ Int → String
formatYearTwoDigits i = case dateLength of
  1 → "0" <> dateString
  2 → dateString
  _ → Str.drop (dateLength - 2) dateString
  where
  dateString = show $ abs i
  dateLength = Str.length $ dateString

fix12 ∷ Int -> Int
fix12 h = if h == 0 then 12 else h

formatCommand ∷ DT.DateTime → FormatterCommand → String
formatCommand dt@(DT.DateTime d t) = case _ of
  YearFull → show $ fromEnum $ D.year d
  YearTwoDigits → formatYearTwoDigits $ fromEnum $ D.year d
  YearAbsolute → show $ fromEnum $ D.year d
  MonthFull → show $ D.month d
  MonthShort → printShortMonth $ D.month d
  MonthTwoDigits → padSingleDigit $ fromEnum $ D.month d
  DayOfMonthTwoDigits → padSingleDigit $ fromEnum $ D.day d
  DayOfMonth → show $ fromEnum $ D.day d
  UnixTimestamp → show $ Int.floor $ (_ / 1000.0) $ unwrap $ unInstant $ fromDateTime dt
  DayOfWeek → show $ fromEnum $ D.weekday d
  Hours24 → padSingleDigit (fromEnum $ T.hour t)
  Hours12 → padSingleDigit $ fix12 $ (fromEnum $ T.hour t) `mod` 12
  Meridiem → if (fromEnum $ T.hour t) >= 12 then "PM" else "AM"
  Minutes → show $ fromEnum $ T.minute t
  MinutesTwoDigits → padSingleDigit <<< fromEnum $ T.minute t
  Seconds → show $ fromEnum $ T.second t
  SecondsTwoDigits → padSingleDigit <<< fromEnum $ T.second t
  Milliseconds → padDoubleDigit <<< fromEnum $ T.millisecond t
  MillisecondsShort → show $ (_ / 100) $ fromEnum $ T.millisecond t
  MillisecondsTwoDigits → padSingleDigit $ (_ / 10) $ fromEnum $ T.millisecond t
  Placeholder s → s

padSingleDigit ∷ Int → String
padSingleDigit i
  | i < 10    = "0" <> (show i)
  | otherwise = show i

padDoubleDigit ∷ Int → String
padDoubleDigit i
  | i < 10  = "00" <> (show i)
  | i < 100 = "0" <> (show i)
  | otherwise = show i

format ∷ Formatter → DT.DateTime → String
format f d = foldMap (formatCommand d) f

formatDateTime ∷ String → DT.DateTime → Either String String
formatDateTime pattern datetime =
  parseFormatString pattern <#> (_ `format` datetime)

unformat ∷ Formatter → String → Either String DT.DateTime
unformat = runP <<< unformatParser

data Meridiem = AM | PM

derive instance eqMeridiem ∷ Eq Meridiem

-- TODO: consider using Map Int
type UnformatAccum =
  { year ∷ Maybe Int
  , month ∷ Maybe Int
  , day ∷ Maybe Int
  , hour ∷ Maybe Int
  , minute ∷ Maybe Int
  , second ∷ Maybe Int
  , millisecond ∷ Maybe Int
  , meridiem ∷ Maybe Meridiem
  }

initialAccum ∷ UnformatAccum
initialAccum =
  { year: Nothing
  , month: Nothing
  , day: Nothing
  , hour: Nothing
  , minute: Nothing
  , second: Nothing
  , millisecond: Nothing
  , meridiem: Nothing
  }

unformatAccumToDateTime ∷ UnformatAccum → Either String DT.DateTime
unformatAccumToDateTime a =
  DT.DateTime
    <$> (D.canonicalDate
           <$> (maybe (Left "Incorrect year") pure $ toEnum $ fromMaybe zero a.year)
           <*> (maybe (Left "Incorrect month") pure $ toEnum $ fromMaybe one a.month)
           <*> (maybe (Left "Incorrect day") pure $ toEnum $ fromMaybe one a.day))
    <*> (T.Time
           <$> (maybe
                  (Left "Incorrect hour") pure
                  $ toEnum
                  =<< (adjustMeridiem $ fromMaybe zero a.hour))
           <*> (maybe (Left "Incorrect minute") pure $ toEnum $ fromMaybe zero a.minute)
           <*> (maybe (Left "Incorrect second") pure $ toEnum $ fromMaybe zero a.second)
           <*> (maybe (Left "Incorrect millisecond") pure $ toEnum $ fromMaybe zero a.millisecond))
  where
  adjustMeridiem ∷ Int → Maybe Int
  adjustMeridiem inp
    | a.meridiem /= Just PM = Just inp
    | inp == 12 = pure 0
    | inp < 12 = pure $ inp + 12
    | otherwise = Nothing



exactLength ∷ ∀ e. ReaderT { maxLength ∷ Int, length ∷ Int | e } (Either String) Unit
exactLength = ask >>= \({maxLength, length}) → if maxLength /= length
  then lift $ Left $ "Expected " <> (show maxLength) <> " digits but got " <> (show length)
  else lift $ Right unit

validateRange ∷ ∀ e. Int → Int → ReaderT { num ∷ Int | e } (Either String) Unit
validateRange min max = ask >>= \({num}) → if num < min || num > max
  then lift $ Left $ "Number is out of range [ " <> (show min) <> ", " <> (show max) <> " ]"
  else lift $ Right unit

parseInt ∷ ∀ m
  . Monad m
  ⇒ Int
  → ReaderT { length ∷ Int, num ∷ Int, maxLength ∷ Int } (Either String) Unit
  → String
  → P.ParserT String m Int
parseInt maxLength validators errMsg = do
  ds ← LazyList.take maxLength <$> (LazyList.some parseDigit)
  let length = LazyList.length ds
  let num = foldDigits ds
  case runReaderT validators {length, num, maxLength} of
    Left err → P.fail $ errMsg <> "(" <> err <> ")"
    Right _ → pure num

unformatCommandParser ∷ FormatterCommand → P.ParserT String (State UnformatAccum) Unit
unformatCommandParser = case _ of
  YearFull → _{year = _} `modifyWithParser`
    (parseInt 4 exactLength "Incorrect full year")
  YearTwoDigits → _{year = _} `modifyWithParser`
    (parseInt 2 exactLength "Incorrect 2-digit year")
  YearAbsolute → _{year = _} `modifyWithParser`
    (pure (*)
      <*> (PC.option 1 $ PC.try $ PS.string "-" <#> (const (-1)))
      <*> (List.some parseDigit <#> foldDigits))
  MonthFull → _{month = _} `modifyWithParser`
    (fromEnum <$> parseMonth)
  MonthShort → _{month = _} `modifyWithParser`
    (fromEnum <$> parseShortMonth)
  MonthTwoDigits → _{month = _} `modifyWithParser`
    (parseInt 2 (validateRange 1 12 <> exactLength) "Incorrect 2-digit month")
  DayOfMonthTwoDigits → _{day = _} `modifyWithParser`
    (parseInt 2 (validateRange 1 31 <> exactLength) "Incorrect day of month")
  DayOfMonth → _{day = _} `modifyWithParser`
    (parseInt 2 (validateRange 1 31) "Incorrect day of month")
  UnixTimestamp → do
    s ← map foldDigits $ List.some parseDigit
    case map toDateTime $ instant $ Dur.Milliseconds $ 1000.0 * Int.toNumber s of
      Nothing → P.fail "Incorrect timestamp"
      Just (DT.DateTime d t) → lift $ put
        { year: Just $ fromEnum $ D.year d
        , month: Just $ fromEnum $ D.month d
        , day: Just $ fromEnum $ D.day d
        , hour: Just $ fromEnum $ T.hour t
        , minute: Just $ fromEnum $ T.minute t
        , second: Just $ fromEnum $ T.second t
        , millisecond: Just $ fromEnum $ T.millisecond t
        , meridiem: (Nothing ∷ Maybe Meridiem)
        }
  -- TODO we would need to use this value if we support date format using week number
  DayOfWeek → void $ parseInt 1 (validateRange 1 7) "Incorrect day of week"
  Hours24 → _{hour = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 23 <> exactLength) "Incorrect 24 hour")
  Hours12 → _{hour = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 11 <> exactLength) "Incorrect 12 hour")
  Meridiem → _{meridiem = _} `modifyWithParser` parseMeridiem
  MinutesTwoDigits → _{minute = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59 <> exactLength) "Incorrect 2-digit minute")
  Minutes → _{minute = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59) "Incorrect minute")
  SecondsTwoDigits → _{second = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59 <> exactLength) "Incorrect 2-digit second")
  Seconds → _{second = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59) "Incorrect second")
  Milliseconds → _{millisecond = _} `modifyWithParser`
    (parseInt 3 exactLength "Incorrect millisecond")
  Placeholder s → void $ PS.string s
  MillisecondsShort → _{millisecond = _} `modifyWithParser`
    (parseInt 1 exactLength "Incorrect 1-digit millisecond")
  MillisecondsTwoDigits → _{millisecond = _} `modifyWithParser`
    (parseInt 2 exactLength "Incorrect 2-digit millisecond")
  where
  modifyWithParser ∷ ∀ s' s x. (s → Maybe x → s) → P.ParserT s' (State s) x → P.ParserT s' (State s) Unit
  modifyWithParser f p = do
    v ← p
    lift $ modify (flip f (Just v))

unformatParser ∷ ∀ m. Monad m ⇒ Formatter → P.ParserT String m DT.DateTime
unformatParser f = do
  acc ← P.mapParserT unState $ foldMap unformatCommandParser f
  either P.fail pure $ unformatAccumToDateTime acc
  where
  unState ∷ ∀ x y n. Monad n ⇒ State UnformatAccum (Tuple (Either y Unit) x) → n (Tuple (Either y UnformatAccum) x)
  unState s = case runState s initialAccum of
    Tuple (Tuple e state) res → pure (Tuple (e $> res) state)



unformatDateTime ∷ String → String → Either String DT.DateTime
unformatDateTime pattern str =
  parseFormatString pattern >>= (_ `unformat` str)

parseMeridiem ∷ ∀ m. Monad m ⇒ P.ParserT String m Meridiem
parseMeridiem =  (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "am" AM
  , Tuple "AM" AM
  , Tuple "pm" PM
  , Tuple "PM" PM
  ]

parseMonth ∷ ∀ m. Monad m ⇒ P.ParserT String m D.Month
parseMonth = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "January" D.January
  , Tuple "February" D.February
  , Tuple "March" D.March
  , Tuple "April" D.April
  , Tuple "May" D.May
  , Tuple "June" D.June
  , Tuple "July" D.July
  , Tuple "August" D.August
  , Tuple "September" D.September
  , Tuple "October" D.October
  , Tuple "November" D.November
  , Tuple "December" D.December
  ]

parseShortMonth ∷ ∀ m. Monad m ⇒ P.ParserT String m D.Month
parseShortMonth = (PC.try <<< PS.string) `oneOfAs`
  [ Tuple "Jan" D.January
  , Tuple "Feb" D.February
  , Tuple "Mar" D.March
  , Tuple "Apr" D.April
  , Tuple "May" D.May
  , Tuple "Jun" D.June
  , Tuple "Jul" D.July
  , Tuple "Aug" D.August
  , Tuple "Sep" D.September
  , Tuple "Oct" D.October
  , Tuple "Nov" D.November
  , Tuple "Dec" D.December
  ]

printShortMonth ∷ D.Month → String
printShortMonth = case _ of
  D.January → "Jan"
  D.February → "Feb"
  D.March → "Mar"
  D.April → "Apr"
  D.May → "May"
  D.June → "Jun"
  D.July → "Jul"
  D.August → "Aug"
  D.September → "Sep"
  D.October → "Oct"
  D.November → "Nov"
  D.December → "Dec"
