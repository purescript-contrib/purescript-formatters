module Data.Formatter.DateTime
  ( Formatter
  , FormatterF(..)
  , printFormatter
  , parseFormatString
  , format
  , formatDateTime
  , unformat
  , unformatDateTime
  ) where

import Prelude

import Control.Lazy as Lazy
import Control.Monad.State (State, mapStateT, modify, put, runState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except.Trans (mapExceptT)

import Data.Ord (abs)
import Data.Array (some)
import Data.Tuple (Tuple(..))
import Data.Array as Arr
import Data.Bifunctor (lmap)
import Data.Date as D
import Data.DateTime as DT
import Data.DateTime.Instant (instant, toDateTime, fromDateTime, unInstant)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Functor.Mu (Mu, unroll, roll)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe, isJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Time as T
import Data.Time.Duration as Dur
import Data.Formatter.Internal (foldDigits)
import Data.Formatter.Parser.Number (parseDigit)
import Data.Formatter.Parser.DateTime (parseMonth, parseShortMonth)

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
  | Seconds a
  | Milliseconds a
  | Placeholder String a
  | End

instance formatterFFunctor ∷ Functor FormatterF where
  map f (YearFull a) = YearFull $ f a
  map f (YearTwoDigits a) = YearTwoDigits $ f a
  map f (YearAbsolute a) = YearAbsolute $ f a
  map f (MonthFull a) = MonthFull $ f a
  map f (MonthShort a) = MonthShort $ f a
  map f (MonthTwoDigits a) = MonthTwoDigits $ f a
  map f (DayOfMonthTwoDigits a) = DayOfMonthTwoDigits $ f a
  map f (DayOfMonth a) = DayOfMonth $ f a
  map f (UnixTimestamp a) = UnixTimestamp $ f a
  map f (DayOfWeek a) = DayOfWeek $ f a
  map f (Hours24 a) = Hours24 $ f a
  map f (Hours12 a) = Hours12 $ f a
  map f (Meridiem a) = Meridiem $ f a
  map f (Minutes a) = Minutes $ f a
  map f (Seconds a) = Seconds $ f a
  map f (Milliseconds a) = Milliseconds $ f a
  map f (Placeholder str a) = Placeholder str $ f a
  map f End = End

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
  Minutes a → "mm" <> cb a
  Seconds a → "ss" <> cb a
  Milliseconds a → "SSS" <> cb a
  Placeholder s a → s <> cb a
  End → ""

printFormatter ∷ Formatter → String
printFormatter f = printFormatterF printFormatter $ unroll f

parseFormatString ∷ String → Either String Formatter
parseFormatString s =
  lmap P.parseErrorMessage $ P.runParser s formatParser

-- | Formatting function that accepts a number that is a year,
-- | and strips away the non-significant digits, leaving only the
-- | ones and tens positions.
formatYearTwoDigits :: Int -> String
formatYearTwoDigits i = case dateLength of
  1 -> "0" <> dateString
  2 -> dateString
  _ -> Str.drop (dateLength - 2) dateString
  where
    dateString = show $ abs i
    dateLength = Str.length $ dateString


placeholderContent ∷ P.Parser String String
placeholderContent =
  map Str.fromCharArray
    $ PC.try
    $ Arr.some
    $ PS.noneOf
    $ Str.toCharArray "YQMDXWEHhamsS"

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
    , (PC.try $ PS.string "mm") *> map Minutes cb
    , (PC.try $ PS.string "ss") *> map Seconds cb
    , (PC.try $ PS.string "SSS") *> map Milliseconds cb
    , (Placeholder <$> placeholderContent <*> cb)
    , (PS.eof $> End)
    ]

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
  YearTwoDigits a ->
    let y = (fromEnum $ D.year d)
    in (formatYearTwoDigits y) <> cb a
  YearAbsolute a →
    show (fromEnum $ D.year d) <> cb a
  MonthFull a →
    show (D.month d) <> cb a
  MonthShort a →
    printShortMonth (D.month d) <> cb a
  MonthTwoDigits a →
    let month = fromEnum $ D.month d
    in (padSingleDigit month) <> cb a
  DayOfMonthTwoDigits a →
    (padSingleDigit $ fromEnum $ D.day d) <> cb a
  DayOfMonth a →
    show (fromEnum $ D.day d) <> cb a
  UnixTimestamp a →
    (show $ Int.floor $ (_ / 1000.0) $ unwrap $ unInstant $ fromDateTime dt) <> cb a
  DayOfWeek a →
    show (fromEnum $ D.weekday d) <> cb a
  Hours24 a →
    show (fromEnum $ T.hour t) <> cb a
  Hours12 a →
    let fix12 h = if h == 0 then 12 else h
    in (padSingleDigit $ fix12 $ (fromEnum $ T.hour t) `mod` 12) <> cb a
  Meridiem a →
    (if (fromEnum $ T.hour t) >= 12 then "PM" else "AM") <> cb a
  Minutes a →
    show (fromEnum $ T.minute t) <> cb a
  Seconds a →
    show (fromEnum $ T.second t) <> cb a
  Milliseconds a →
    show (fromEnum $ T.millisecond t) <> cb a
  Placeholder s a →
    s <> cb a
  End → ""

padSingleDigit :: Int -> String
padSingleDigit i
  | i < 10    = "0" <> (show i)
  | otherwise = show i

format ∷ Formatter → DT.DateTime → String
format f dt = formatF (flip format dt) dt $ unroll f

formatDateTime ∷ String → DT.DateTime → Either String String
formatDateTime pattern datetime =
  parseFormatString pattern <#> flip format datetime

unformat ∷ Formatter → String → Either String DT.DateTime
unformat f s =
  unformatParser f
    # P.runParser s
    # lmap P.parseErrorMessage
    >>= unformatAccumToDateTime

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


unformatFParser
  ∷ ∀ a
  . (a → P.ParserT String (State UnformatAccum) Unit)
  → FormatterF a
  → P.ParserT String (State UnformatAccum) Unit
unformatFParser cb = case _ of
  YearFull a → do
    ds ← some parseDigit
    when (Arr.length ds /= 4) $ P.fail "Incorrect full year"
    lift $ modify _{year = Just $ foldDigits ds}
    cb a
  YearTwoDigits a → do
    ds ← some parseDigit
    when (Arr.length ds /= 2) $ P.fail "Incorrect 2-digit year"
    let y = foldDigits ds
    lift $ modify _{year = Just $ if y > 69 then y + 1900 else y + 2000}
    cb a
  YearAbsolute a → do
    sign ← PC.optionMaybe $ PC.try $ PS.string "-"
    year ← map foldDigits $ some parseDigit
    lift $ modify _{year = Just $ (if isJust sign then -1 else 1) * year}
    cb a
  MonthFull a → do
    month ← parseMonth
    lift $ modify _{month = Just $ fromEnum month}
    cb a
  MonthShort a → do
    month ← parseShortMonth
    lift $ modify _{month = Just $ fromEnum month}
    cb a
  MonthTwoDigits a → do
    ds ← some parseDigit
    let month = foldDigits ds
    when (Arr.length ds /= 2 || month > 12 || month < 1) $ P.fail "Incorrect 2-digit month"
    lift $ modify _{month = Just month}
    cb a
  DayOfMonthTwoDigits a → do
    ds ← some parseDigit
    let dom = foldDigits ds
    when (Arr.length ds /= 2 || dom > 31 || dom < 1) $ P.fail "Incorrect day of month"
    lift $ modify _{day = Just dom}
    cb a
  DayOfMonth a → do
    ds ← some parseDigit
    let dom = foldDigits ds
    when (Arr.length ds > 2 || dom > 31 || dom < 1) $ P.fail "Incorrect day of month"
    lift $ modify _{day = Just dom}
    cb a
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
  DayOfWeek a → do
    dow ← parseDigit
    when (dow > 7 || dow < 1) $ P.fail "Incorrect day of week"
    cb a
  Hours24 a → do
    ds ← some parseDigit
    let hh = foldDigits ds
    when (Arr.length ds /= 2 || hh < 0 || hh > 23) $ P.fail "Incorrect 24 hour"
    lift $ modify _{hour = Just hh}
    cb a
  Hours12 a → do
    ds ← some parseDigit
    let hh = foldDigits ds
    when (Arr.length ds /= 2 || hh < 0 || hh > 11) $ P.fail "Incorrect 24 hour"
    lift $ modify _{hour = Just hh}
    cb a
  Meridiem a → do
    m ←
      PC.choice [ PC.try $ PS.string "am" $> AM
                , PC.try $ PS.string "AM" $> AM
                , PC.try $ PS.string "pm" $> PM
                , PC.try $ PS.string "PM" $> PM
                ]
    lift $ modify _{meridiem = Just m}
    cb a
  Minutes a → do
    ds ← some parseDigit
    let mm = foldDigits ds
    when (Arr.length ds /= 2 || mm < 0 || mm > 59) $ P.fail "Incorrect minute"
    lift $ modify _{minute = Just mm}
    cb a
  Seconds a → do
    ds ← some parseDigit
    let ss = foldDigits ds
    when (Arr.length ds /= 2 || ss < 0 || ss > 59) $ P.fail "Incorrect second"
    lift $ modify _{second = Just ss}
    cb a
  Milliseconds a → do
    ds ← some parseDigit
    let sss = foldDigits ds
    when (Arr.length ds /= 3 || sss < 0 || sss > 999) $ P.fail "Incorrect millisecond"
    lift $ modify _{millisecond = Just sss}
    cb a
  Placeholder s a →
    PS.string s *> cb a
  End →
    pure unit


hoistParserT' :: ∀ b n s a m. (m (Tuple (Either P.ParseError a) (P.ParseState s)) -> n (Tuple (Either P.ParseError b) (P.ParseState s))) -> P.ParserT s m a -> P.ParserT s n b
hoistParserT' f (P.ParserT m) = P.ParserT (mapExceptT (mapStateT f) m)

-- unformatParser ∷ ∀ m. Formatter → P.ParserT String m UnformatAccum
unformatParser ∷ Formatter → P.Parser String UnformatAccum
unformatParser f' = hoistParserT' unState $ rec f'
  where
    rec ∷ Formatter → P.ParserT String (State UnformatAccum) Unit
    rec f = unformatFParser rec $ unroll f
    unState :: ∀ x y m. Monad m => State UnformatAccum (Tuple (Either y Unit) x) -> m (Tuple (Either y UnformatAccum) x)
    unState s = case runState s initialAccum of
      Tuple (Tuple e state) res -> pure (Tuple (e $> res) state)



unformatDateTime ∷ String → String → Either String DT.DateTime
unformatDateTime pattern str =
  parseFormatString pattern >>= flip unformat str

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
