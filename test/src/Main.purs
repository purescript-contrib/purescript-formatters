module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.DateTime (datetimeTest)
import Test.Interval (intervalTest)
import Test.Number (numberTest)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  intervalTest
  datetimeTest
  numberTest
