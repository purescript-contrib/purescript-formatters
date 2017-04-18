module Data.Formatter.Parser.DateTime
  ( parseMonth
  , parseShortMonth
  ) where

import Prelude

import Text.Formatter.Parser.Utils (oneOfAs)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Data.Date as D

-- TODO use `oneOfAs`
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

-- TODO use `oneOfAs`
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
