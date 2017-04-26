module Data.Formatter.DateTime
  ( Formatter
  , FormatterF(..)
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

import Control.Lazy as Lazy
import Control.Monad.State (State, modify, put, runState)
import Control.Monad.Trans.Class (lift)

import Data.Ord (abs)
import Data.Array (some)
import Data.List.Lazy as List
import Data.Tuple (Tuple(..))
import Data.Date as D
import Data.DateTime as DT
import Data.DateTime.Instant (instant, toDateTime, fromDateTime, unInstant)
import Data.Either (Either(..), either)
import Data.Enum (fromEnum, toEnum)
import Data.Functor.Mu (Mu, unroll, roll)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Time as T
import Data.Eq (class Eq1)
import Data.Time.Duration as Dur
import Data.Formatter.Internal (foldDigits)
import Data.Formatter.Parser.Number (parseDigit)
import Data.Formatter.Parser.Utils (runP, oneOfAs)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS


data FormatterF a
  = YearFull a
  | YearTwoDigits a
  | YearAbsolute a
  | MonthFull a
  | MonthShort a
  | MonthTwoDigits a
  | DayOfMonthTwoDigits a
  | DayOfMonth a
  | UnixTimestamp a
  | DayOfWeek a
  | Hours24 a
  | Hours12 a
  | Meridiem a
  | Minutes a
  | MinutesTwoDigits a
  | Seconds a
  | SecondsTwoDigits a
  | Milliseconds a
  | MillisecondsShort a
  | MillisecondsTwoDigits a
  | Placeholder String a
  | End

derive instance functorFormatterF :: Functor FormatterF

instance showFormatterF ∷ Show a => Show (FormatterF a) where
  show (YearFull a) = "(YearFull " <> (show a) <> "c"
  show (YearTwoDigits a) = "(YearTwoDigits " <> (show a) <> ")"
  show (YearAbsolute a) = "(YearAbsolute " <> (show a) <> ")"
  show (MonthFull a) = "(MonthFull " <> (show a) <> ")"
  show (MonthShort a) = "(MonthShort " <> (show a) <> ")"
  show (MonthTwoDigits a) = "(MonthTwoDigits " <> (show a) <> ")"
  show (DayOfMonthTwoDigits a) = "(DayOfMonthTwoDigits " <> (show a) <> ")"
  show (DayOfMonth a) = "(DayOfMonth " <> (show a) <> ")"
  show (UnixTimestamp a) = "(UnixTimestamp " <> (show a) <> ")"
  show (DayOfWeek a) = "(DayOfWeek " <> (show a) <> ")"
  show (Hours24 a) = "(Hours24 " <> (show a) <> ")"
  show (Hours12 a) = "(Hours12 " <> (show a) <> ")"
  show (Meridiem a) = "(Meridiem " <> (show a) <> ")"
  show (Minutes a) = "(Minutes " <> (show a) <> ")"
  show (MinutesTwoDigits a) = "(MinutesTwoDigits " <> (show a) <> ")"
  show (Seconds a) = "(Seconds " <> (show a) <> ")"
  show (SecondsTwoDigits a) = "(SecondsTwoDigits " <> (show a) <> ")"
  show (Milliseconds a) = "(Milliseconds " <> (show a) <> ")"
  show (MillisecondsShort a) = "(MillisecondsShort " <> (show a) <> ")"
  show (MillisecondsTwoDigits a) = "(MillisecondsTwoDigits " <> (show a) <> ")"
  show (Placeholder str a) = "(Placeholder " <> (show str) <> " "<> (show a) <> ")"
  show End = "End"

derive instance eqFormatterF :: Eq a => Eq (FormatterF a)

instance eq1FormatterF :: Eq1 FormatterF where
  eq1 = eq


type Formatter = Mu FormatterF

printFormatterF
  ∷ ∀ a
  . (a → String)
  → FormatterF a
  → String
printFormatterF cb = case _ of
  YearFull a → "YYYY" <> cb a
  YearTwoDigits a → "YY" <> cb a
  YearAbsolute a → "Y" <> cb a
  MonthFull a → "MMMM" <> cb a
  MonthShort a → "MMM" <> cb a
  MonthTwoDigits a → "MM" <> cb a
  DayOfMonthTwoDigits a → "DD" <> cb a
  DayOfMonth a → "D" <> cb a
  UnixTimestamp a → "X" <> cb a
  DayOfWeek a → "E" <> cb a
  Hours24 a → "HH" <> cb a
  Hours12 a → "hh" <> cb a
  Meridiem a → "a" <> cb a
  Minutes a → "m" <> cb a
  MinutesTwoDigits a → "mm" <> cb a
  Seconds a → "s" <> cb a
  SecondsTwoDigits a → "ss" <> cb a
  MillisecondsShort a → "S" <> cb a
  MillisecondsTwoDigits a → "SS" <> cb a
  Milliseconds a → "SSS" <> cb a
  Placeholder s a → s <> cb a
  End → ""

printFormatter ∷ Formatter → String
printFormatter f = printFormatterF printFormatter $ unroll f

parseFormatString ∷ String → Either String Formatter
parseFormatString = runP formatParser

-- | Formatting function that accepts a number that is a year,
-- | and strips away the non-significant digits, leaving only the
-- | ones and tens positions.
formatYearTwoDigits :: Int → String
formatYearTwoDigits i = case dateLength of
  1 → "0" <> dateString
  2 → dateString
  _ → Str.drop (dateLength - 2) dateString
  where
  dateString = show $ abs i
  dateLength = Str.length $ dateString


placeholderContent ∷ P.Parser String String
placeholderContent =
  map Str.fromCharArray
    $ PC.try
    $ some
    $ PS.noneOf
    $ Str.toCharArray "YMDEHhamsS"

formatterFParser
  ∷ ∀ a
  . P.Parser String a
  → P.Parser String (FormatterF a)
formatterFParser cb =
  PC.choice
    [ (PC.try $ PS.string "YYYY") *> map YearFull cb
    , (PC.try $ PS.string "YY") *> map YearTwoDigits cb
    , (PC.try $ PS.string "Y") *> map YearAbsolute cb
    , (PC.try $ PS.string "MMMM") *> map MonthFull cb
    , (PC.try $ PS.string "MMM") *> map MonthShort cb
    , (PC.try $ PS.string "MM") *> map MonthTwoDigits cb
    , (PC.try $ PS.string "DD") *> map DayOfMonthTwoDigits cb
    , (PC.try $ PS.string "D") *> map DayOfMonth cb
    , (PC.try $ PS.string "E") *> map DayOfWeek cb
    , (PC.try $ PS.string "HH") *> map Hours24 cb
    , (PC.try $ PS.string "hh") *> map Hours12 cb
    , (PC.try $ PS.string "a") *> map Meridiem cb
    , (PC.try $ PS.string "mm") *> map MinutesTwoDigits cb
    , (PC.try $ PS.string "m") *> map Minutes cb
    , (PC.try $ PS.string "ss") *> map SecondsTwoDigits cb
    , (PC.try $ PS.string "s") *> map Seconds cb
    , (PC.try $ PS.string "SSS") *> map Milliseconds cb
    , (PC.try $ PS.string "SS") *> map MillisecondsTwoDigits cb
    , (PC.try $ PS.string "S") *> map MillisecondsShort cb
    , (Placeholder <$> placeholderContent <*> cb)
    , (PS.eof $> End)
    ] PC.<?> "to contain only valid characters"

formatParser ∷ P.Parser String Formatter
formatParser =
  Lazy.fix \f → map roll $ formatterFParser f

formatF
  ∷ ∀ a
  . (a → String)
  → DT.DateTime
  → FormatterF a
  → String
formatF cb dt@(DT.DateTime d t) = case _ of
  YearFull a →
    (show $ fromEnum $ D.year d) <> cb a
  YearTwoDigits a →
    (formatYearTwoDigits $ fromEnum $ D.year d) <> cb a
  YearAbsolute a →
    show (fromEnum $ D.year d) <> cb a
  MonthFull a →
    show (D.month d) <> cb a
  MonthShort a →
    (printShortMonth $ D.month d) <> cb a
  MonthTwoDigits a →
    (padSingleDigit $ fromEnum $ D.month d) <> cb a
  DayOfMonthTwoDigits a →
    (padSingleDigit $ fromEnum $ D.day d) <> cb a
  DayOfMonth a →
    show (fromEnum $ D.day d) <> cb a
  UnixTimestamp a →
    (show $ Int.floor $ (_ / 1000.0) $ unwrap $ unInstant $ fromDateTime dt) <> cb a
  DayOfWeek a →
    show (fromEnum $ D.weekday d) <> cb a
  Hours24 a →
    padSingleDigit (fromEnum $ T.hour t) <> cb a
  Hours12 a →
    let fix12 h = if h == 0 then 12 else h
    in (padSingleDigit $ fix12 $ (fromEnum $ T.hour t) `mod` 12) <> cb a
  Meridiem a →
    (if (fromEnum $ T.hour t) >= 12 then "PM" else "AM") <> cb a
  Minutes a →
    show (fromEnum $ T.minute t) <> cb a
  MinutesTwoDigits a →
    (padSingleDigit <<< fromEnum $ T.minute t) <> cb a
  Seconds a →
    show (fromEnum $ T.second t) <> cb a
  SecondsTwoDigits a →
    (padSingleDigit <<< fromEnum $ T.second t) <> cb a
  Milliseconds a →
    (padDoubleDigit <<< fromEnum $ T.millisecond t) <> cb a
  MillisecondsShort a →
    (show $ (_ / 100) $ fromEnum $ T.millisecond t) <> cb a
  MillisecondsTwoDigits a →
    (padSingleDigit $ (_ / 10) $ fromEnum $ T.millisecond t) <> cb a
  Placeholder s a →
    s <> cb a
  End → ""

padSingleDigit :: Int → String
padSingleDigit i
  | i < 10    = "0" <> (show i)
  | otherwise = show i

padDoubleDigit :: Int → String
padDoubleDigit i
  | i < 10  = "00" <> (show i)
  | i < 100 = "0" <> (show i)
  | otherwise = show i

format ∷ Formatter → DT.DateTime → String
format f dt = formatF (flip format dt) dt $ unroll f

formatDateTime ∷ String → DT.DateTime → Either String String
formatDateTime pattern datetime =
  parseFormatString pattern <#> flip format datetime

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


-- NOTE `ReaderT s (Either e) Unit` forms Monoid where
-- `mempty = lift $ Right unit` (noValidate) and `concat = (*>)`
noValidate ∷ ∀ e. ReaderT { maxLength ∷ Int, length ∷ Int | e } (Either String) Unit
noValidate = lift $ Right unit

exactLength ∷ ∀ e. ReaderT { maxLength ∷ Int, length ∷ Int | e } (Either String) Unit
exactLength = ask >>= \({maxLength, length}) → if maxLength /= length
  then lift $ Left $ "Expected " <> (show maxLength) <> " digits but got " <> (show length)
  else lift $ Right unit

validateRange ∷ ∀ e. Int → Int → ReaderT { num ∷ Int | e } (Either String) Unit
validateRange min max = ask >>= \({num}) → if num < min || num > max
  then lift $ Left $ "Number is out of range [ " <> (show min) <> ", " <> (show max) <> " ]"
  else lift $ Right unit

parseInt :: ∀ m
  . Monad m
  ⇒ Int
  → ReaderT { length ∷ Int, num ∷ Int, maxLength ∷ Int } (Either String) Unit
  → String
  → P.ParserT String m Int
parseInt maxLength validators errMsg = do
  ds ← List.take maxLength <$> (List.some parseDigit)
  let length = List.length ds
  let num = foldDigits ds
  case runReaderT validators {length, num, maxLength} of
    Left err → P.fail $ errMsg <> "(" <> err <> ")"
    Right _ → pure num

-- take
unformatFParser
  ∷ ∀ a
  . (a → P.ParserT String (State UnformatAccum) Unit)
  → FormatterF a
  → P.ParserT String (State UnformatAccum) Unit
unformatFParser cb = case _ of
  YearFull a → _{year = _} `modifyWithParser`
    (parseInt 4 exactLength "Incorrect full year") *> cb a
  YearTwoDigits a → _{year = _} `modifyWithParser`
    (parseInt 2 exactLength "Incorrect 2-digit year") *> cb a
  YearAbsolute a → _{year = _} `modifyWithParser`
    (pure (*)
      <*> (PC.option 1 $ PC.try $ PS.string "-" <#> (const (-1)))
      <*> (some parseDigit <#> foldDigits)) *> cb a
  MonthFull a → _{month = _} `modifyWithParser`
    (fromEnum <$> parseMonth) *> cb a
  MonthShort a → _{month = _} `modifyWithParser`
    (fromEnum <$> parseShortMonth) *> cb a
  MonthTwoDigits a → _{month = _} `modifyWithParser`
    (parseInt 2 (validateRange 1 12 *> exactLength) "Incorrect 2-digit month") *> cb a
  DayOfMonthTwoDigits a → _{day = _} `modifyWithParser`
    (parseInt 2 (validateRange 1 31 *> exactLength) "Incorrect day of month") *> cb a
  DayOfMonth a → _{day = _} `modifyWithParser`
    (parseInt 2 (validateRange 1 31) "Incorrect day of month") *> cb a
  UnixTimestamp a → do
    s ← map foldDigits $ some parseDigit
    case map toDateTime $ instant $ Dur.Milliseconds $ 1000.0 * Int.toNumber s of
      Nothing → P.fail "Incorrect timestamp"
      Just (DT.DateTime d t) → do
        lift $ put { year: Just $ fromEnum $ D.year d
                   , month: Just $ fromEnum $ D.month d
                   , day: Just $ fromEnum $ D.day d
                   , hour: Just $ fromEnum $ T.hour t
                   , minute: Just $ fromEnum $ T.minute t
                   , second: Just $ fromEnum $ T.second t
                   , millisecond: Just $ fromEnum $ T.millisecond t
                   , meridiem: (Nothing ∷ Maybe Meridiem)
                   }
        cb a
  -- TODO we would need to use this value if we support date format using week number
  DayOfWeek a → (parseInt 1 (validateRange 1 7) "Incorrect day of week") *> cb a
  Hours24 a → _{hour = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 23 *> exactLength) "Incorrect 24 hour") *> cb a
  Hours12 a → _{hour = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 11 *> exactLength) "Incorrect 12 hour") *> cb a
  Meridiem a → _{meridiem = _} `modifyWithParser`
    parseMeridiem *> cb a
  MinutesTwoDigits a → _{minute = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59 *> exactLength) "Incorrect 2-digit minute") *> cb a
  Minutes a → _{minute = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59) "Incorrect minute") *> cb a
  SecondsTwoDigits a → _{second = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59 *> exactLength) "Incorrect 2-digit second") *> cb a
  Seconds a → _{second = _} `modifyWithParser`
    (parseInt 2 (validateRange 0 59) "Incorrect second") *> cb a
  Milliseconds a → _{millisecond = _} `modifyWithParser`
    (parseInt 3 exactLength "Incorrect millisecond") *> cb a
  Placeholder s a → PS.string s *> cb a
  MillisecondsShort a → _{millisecond = _} `modifyWithParser`
    (parseInt 1 exactLength "Incorrect 1-digit millisecond") *> cb a
  MillisecondsTwoDigits a → _{millisecond = _} `modifyWithParser`
    (parseInt 2 exactLength "Incorrect 2-digit millisecond") *> cb a
  End → pure unit
  where
  modifyWithParser :: ∀ s' s x. (s → Maybe x → s) → P.ParserT s' (State s) x → P.ParserT s' (State s) Unit
  modifyWithParser f p = do
    v <- p
    lift $ modify (flip f (Just v))

unformatParser ∷ ∀ m. Monad m => Formatter → P.ParserT String m DT.DateTime
unformatParser f' = do
  acc <- P.mapParserT unState $ rec f'
  either P.fail pure $ unformatAccumToDateTime acc
  where
  rec ∷ Formatter → P.ParserT String (State UnformatAccum) Unit
  rec f = unformatFParser rec $ unroll f
  unState :: ∀ x y n. Monad n => State UnformatAccum (Tuple (Either y Unit) x) → n (Tuple (Either y UnformatAccum) x)
  unState s = case runState s initialAccum of
    Tuple (Tuple e state) res → pure (Tuple (e $> res) state)



unformatDateTime ∷ String → String → Either String DT.DateTime
unformatDateTime pattern str =
  parseFormatString pattern >>= flip unformat str

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
