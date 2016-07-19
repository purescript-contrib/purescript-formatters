module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Formatter.Number as FN
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
  log $ FN.printFormatter fnOne
  log $ FN.printFormatter fnTwo
  log $ FN.printFormatter fnThree

  DT.traceAnyA $ FN.parseFormatString "000,0.00"
  DT.traceAnyA $ FN.parseFormatString "000"
  DT.traceAnyA $ FN.parseFormatString "0a"
  DT.traceAnyA $ FN.parseFormatString "-0,0.000"
  DT.traceAnyA $ FN.parseFormatString "+000.0"

  log $ FN.format fnOne 100.2
  log $ FN.format fnTwo 100.1
  log $ FN.format fnThree 100.3
  log $ FN.format fnThree 10004000.0

  DT.traceAnyA $ FN.unformat fnOne "001.12"
  DT.traceAnyA $ FN.unformat fnOne "-123.12"
  DT.traceAnyA $ FN.unformat fnOne "12.12"

  DT.traceAnyA $ FN.unformat fnThree "+123"
  DT.traceAnyA $ FN.unformat fnTwo "-100,000.1234"


  DT.traceAnyA $ FN.formatNumber "00.00" 12.0
  DT.traceAnyA $ FN.formatNumber "00000,0.000" 123345.1235

  DT.traceAnyA $ FN.unformatNumber "0.00" "12.00"

main ∷ forall e. Eff (console ∷ CONSOLE | e) Unit
main = do
  pure unit
