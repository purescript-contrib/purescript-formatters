module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Functor.Mu (roll)
import Data.Formatter.Number as FN
import Data.Formatter.DateTime as FDT
import Data.Maybe (fromMaybe)
import Data.Either (Either(..))
import Data.Enum (toEnum)

import Data.DateTime as DTi
import Data.Date as D
import Data.Time as T
import Debug.Trace as DT


fnOne ∷ FN.Formatter
fnOne =
  { comma: false
  , before: 3
  , after: 2
  , abbreviations: false
  , sign: false
  }


fnTwo ∷ FN.Formatter
fnTwo =
  { comma: true
  , before: one
  , after: 4
  , abbreviations: false
  , sign: true
  }

fnThree ∷ FN.Formatter
fnThree =
 { comma: false
 , before: 2
 , after: 2
 , abbreviations: true
 , sign: true
 }


numeral ∷ ∀ e. Eff (console ∷ CONSOLE |e) Unit
numeral = do
  log $ "NUMERAL TESTS\n\n"

  log $ "\nPRINT FORMATTER"
  log $ FN.printFormatter fnOne
  log $ FN.printFormatter fnTwo
  log $ FN.printFormatter fnThree

  log $ "\nPARSE FORMAT STRING"
  DT.traceAnyA $ FN.parseFormatString "000,0.00"
  DT.traceAnyA $ FN.parseFormatString "000"
  DT.traceAnyA $ FN.parseFormatString "0a"
  DT.traceAnyA $ FN.parseFormatString "-0,0.000"
  DT.traceAnyA $ FN.parseFormatString "+000.0"

  log $ "\n FORMAT"
  log $ FN.format fnOne 100.2
  log $ FN.format fnTwo 100.1
  log $ FN.format fnThree 100.3
  log $ FN.format fnThree 10004000.0

  log $ "\n UNFORMAT"
  DT.traceAnyA $ FN.unformat fnOne "001.12"
  DT.traceAnyA $ FN.unformat fnOne "-123.12"
  DT.traceAnyA $ FN.unformat fnOne "12.12"
  DT.traceAnyA $ FN.unformat fnThree "+123"
  DT.traceAnyA $ FN.unformat fnTwo "-100,000.1234"

  log $ "\n FORMAT NUMBER"
  DT.traceAnyA $ FN.formatNumber "00.00" 12.0
  DT.traceAnyA $ FN.formatNumber "00000,0.000" 123345.1235
  DT.traceAnyA $ FN.formatNumber "0.0" 123345.1235
  DT.traceAnyA $ FN.formatNumber "0.0" (-123345.1235)

  log $ "\n UNFORMAT NUMBER"
  DT.traceAnyA $ FN.unformatNumber "0.00" "12.00"

fdtOne ∷ FDT.Formatter
fdtOne =
  roll $ FDT.Placeholder "format string is "
  $ roll $ FDT.YearFull
  $ roll $ FDT.Placeholder "-"
  $ roll $ FDT.MonthShort
  $ roll FDT.End

testDateTime ∷ DTi.DateTime
testDateTime =
  DTi.DateTime
    (D.canonicalDate (fromMaybe bottom $ toEnum 1234) D.April (fromMaybe bottom $ toEnum 12))
    (T.Time
       (fromMaybe bottom $ toEnum 11)
       (fromMaybe bottom $ toEnum 34)
       (fromMaybe bottom $ toEnum 34)
       (fromMaybe bottom $ toEnum 234))

timeTest ∷ ∀ e. Eff (console ∷ CONSOLE |e) Unit
timeTest = do
  log $ "\n\n DATETIME FORMATTER"
  log "\nPRINT FORMATTER"
  log $ FDT.printFormatter fdtOne

  log $ "\nPARSE FORMAT STRING"
  DT.traceAnyA $ FDT.parseFormatString "YYYY-MM-DD"
  DT.traceAnyA $ FDT.parseFormatString "YY-Q-dddd HH:mm Z"

  log $ "\nFORMAT"
  DT.traceAnyA $ FDT.parseFormatString "YYYY-MM-DD trololo Q" <#> flip FDT.format testDateTime

  log $ "\nUNFORMAT"
  case FDT.parseFormatString "DD-MM-YYYY HH-:-mm" of
    Left _ → DT.traceAnyA "?"
    Right f → DT.traceAnyA $ FDT.unformat f "12-10-1345 04-:-32"

  log $ "\nUNFORMATDATETIME"
  DT.traceAnyA $ FDT.unformatDateTime "YYYY-DD-MM SSS" "3456-09-10 333"


main ∷ forall e. Eff (console ∷ CONSOLE | e) Unit
main = do
  timeTest
  numeral
