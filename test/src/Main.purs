module Test.Main where

import Prelude

import Test.Interval (intervalTest)
import Test.DateTime (datetimeTest)
import Test.Number (numberTest)
import Test.Spec.Reporter.Console (consoleReporter)
import Effect (Effect)
import Test.Spec.Runner (run)

main ∷ Effect Unit
main = run [consoleReporter] do
  intervalTest
  datetimeTest
  numberTest
