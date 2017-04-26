module Test.Main where

import Prelude

import Test.Interval (intervalTest)
import Test.DateTime (datetimeTest)
import Test.Number (numberTest)
import Test.Spec.Reporter.Console (consoleReporter)
import Control.Monad.Eff (Eff)
import Test.Spec.Runner (RunnerEffects, run)

main ∷ Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  intervalTest
  datetimeTest
  numberTest
