module Data.Formatter.Parser.DateTime
  ( parseMonth
  , parseShortMonth
  ) where

import Prelude

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Data.Date as D

-- TODO use `oneOfAs`
parseMonth ∷ ∀ m. Monad m ⇒ P.ParserT String m D.Month
parseMonth =
  PC.choice
    [ (PC.try $ PS.string "January") $> D.January
    , (PC.try $ PS.string "February") $> D.February
    , (PC.try $ PS.string "March") $> D.March
    , (PC.try $ PS.string "April") $> D.April
    , (PC.try $ PS.string "May") $> D.May
    , (PC.try $ PS.string "June") $> D.June
    , (PC.try $ PS.string "July") $> D.July
    , (PC.try $ PS.string "August") $> D.August
    , (PC.try $ PS.string "September") $> D.September
    , (PC.try $ PS.string "October") $> D.October
    , (PC.try $ PS.string "November") $> D.November
    , (PC.try $ PS.string "December") $> D.December
    ]

-- TODO use `oneOfAs`
parseShortMonth ∷ ∀ m. Monad m ⇒ P.ParserT String m D.Month
parseShortMonth =
  PC.choice
    [ (PC.try $ PS.string "Jan") $> D.January
    , (PC.try $ PS.string "Feb") $> D.February
    , (PC.try $ PS.string "Mar") $> D.March
    , (PC.try $ PS.string "Apr") $> D.April
    , (PC.try $ PS.string "May") $> D.May
    , (PC.try $ PS.string "Jun") $> D.June
    , (PC.try $ PS.string "Jul") $> D.July
    , (PC.try $ PS.string "Aug") $> D.August
    , (PC.try $ PS.string "Sep") $> D.September
    , (PC.try $ PS.string "Oct") $> D.October
    , (PC.try $ PS.string "Nov") $> D.November
    , (PC.try $ PS.string "Dec") $> D.December
    ]
