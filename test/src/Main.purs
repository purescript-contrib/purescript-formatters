module Test.Main where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.DateTime (datetimeTest)
import Test.Interval (intervalTest)
import Test.Number (numberTest)

main :: Effect Unit
main = launchAff_ $ flip runReaderT 0 do
  intervalTest
  datetimeTest
  numberTest
