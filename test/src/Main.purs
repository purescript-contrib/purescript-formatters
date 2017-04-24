module Test.Main where

import Prelude

import Data.Date as D
import Data.DateTime as DTi

import Data.Foldable (for_)
import Data.Formatter.DateTime as FDT
import Data.Formatter.Number as FN
import Data.Time as T
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Functor.Mu (roll)
import Data.Maybe (fromMaybe)
import Test.Interval (intervalTest)
import Control.Alternative (class Alternative, empty)


import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)


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


-- TODO refactor this
-- numeralTests :: forall e. Spec e Unit
-- numeralTests = do
--   log $ "\nNUMERAL TESTS\n"
--
--   log $ "\nPRINT FORMATTER"
--   log $ FN.printFormatter fnOne
--   log $ FN.printFormatter fnTwo
--   log $ FN.printFormatter fnThree
--
--   log $ "\nPARSE FORMAT STRING"
--   DT.traceAnyA $ FN.parseFormatString "000,0.00"
--   DT.traceAnyA $ FN.parseFormatString "000"
--   DT.traceAnyA $ FN.parseFormatString "0a"
--   DT.traceAnyA $ FN.parseFormatString "-0,0.000"
--   DT.traceAnyA $ FN.parseFormatString "+000.0"
--
--   log $ "\n FORMAT"
--   log $ FN.format fnOne 100.2
--   log $ FN.format fnTwo 100.1
--   log $ FN.format fnThree 100.3
--   log $ FN.format fnThree 10004000.0
--
--   log $ "\n UNFORMAT"
--   DT.traceAnyA $ FN.unformat fnOne "001.12"
--   DT.traceAnyA $ FN.unformat fnOne "-123.12"
--   DT.traceAnyA $ FN.unformat fnOne "12.12"
--   DT.traceAnyA $ FN.unformat fnThree "+123"
--   DT.traceAnyA $ FN.unformat fnTwo "-100,000.1234"
--
--   log $ "\n FORMAT NUMBER"
--   DT.traceAnyA $ FN.formatNumber "00.00" 12.0
--   DT.traceAnyA $ FN.formatNumber "00000,0.000" 123345.1235
--   DT.traceAnyA $ FN.formatNumber "0.0" 123345.1235
--   DT.traceAnyA $ FN.formatNumber "0.0" (-123345.1235)
--
--   log $ "\n UNFORMAT NUMBER"
--   DT.traceAnyA $ FN.unformatNumber "0.00" "12.00"
--

makeDateTime ∷ Int -> Int -> Int -> DTi.DateTime
makeDateTime year month day =
  DTi.DateTime
    (D.canonicalDate (fromMaybe bottom $ toEnum year) (fromMaybe bottom $ toEnum month) (fromMaybe bottom $ toEnum day))
    -- XXX at 11:34:34:234
    (T.Time
       (fromMaybe bottom $ toEnum 11)
       (fromMaybe bottom $ toEnum 34)
       (fromMaybe bottom $ toEnum 34)
       (fromMaybe bottom $ toEnum 234))



testDateTime :: DTi.DateTime
testDateTime = makeDateTime 2017 4 12 -- April 12th 2017


assertFormatting :: forall e. String -> String -> DateTime -> Aff e Unit
assertFormatting target' format dateTime = result `shouldEqual` target
  where
  result = FDT.formatDateTime format dateTime
  target = Right target'

dates :: Array DateTime
dates =
  [ testDateTime
  , makeDateTime 2017 4 1
  , makeDateTime 20017 4 12
  , makeDateTime 0 4 12
  , makeDateTime (-1) 4 12
  ]

timeTest :: forall e. Spec e Unit
timeTest = describe "Data.Formatter.DateTime" do
  describe "formatDateTime" do
    it "should formatt dateTime" do
      let
        items =
          [ { format: "MM/DD/YYYY", dateStr: "04/12/2017" , date: testDateTime}
          , { format: "MMMM", dateStr: "April" , date: testDateTime}
          , { format: "YYYY-DD-MM", dateStr: "2017-12-04" , date: testDateTime}
          , { format: "YYYY-MMM", dateStr: "2017-Apr" , date: testDateTime}
          , { format: "MMM D", dateStr: "Apr 1" , date: makeDateTime 2017 4 1}
          , { format: "hh:mm:ss:SSS a", dateStr: "11:34:34:234 AM" , date: testDateTime}
          , { format: "YY", dateStr: "17" , date: testDateTime}
          , { format: "YY", dateStr: "17" , date: makeDateTime 20017 4 12} -- Format 20017 with YY
          , { format: "YY", dateStr: "00" , date: makeDateTime 0 4 12} -- Format 0 with YY
          , { format: "YY", dateStr: "01" , date: makeDateTime (-1) 4 12} -- Format -1 with YY
          ]
      for_ items \({ format, dateStr, date }) -> do
        (format `FDT.formatDateTime` date) `shouldEqual` (Right dateStr)

  describe "parseFormatString" do
    it "should parse" do
      for_ dateformats \f -> (FDT.parseFormatString f.str) `shouldEqual` (Right f.format)

    it "shouldn't parse" do
      for_ invalidDateformats \f -> (FDT.parseFormatString f.str) `shouldEqual` (Left $ "Format contains invalid string@" <> f.pos)

  it "s ≡ format (unformat s)" do
    let items = [ {date: "2017-12-04 234", format: "YYYY-DD-MM SSS" } , {date: "3456-09-10 333", format: "YYYY-DD-MM SSS" } ]
    for_ items \({date, format}) -> do
      (FDT.unformatDateTime format date >>= FDT.formatDateTime format) `shouldEqual` (Right date)

  it "s ≡ unformat (format s)" do
    let
      items = do
        format <- dateformats
        date <- dates
        guard format.lossless
        pure { date, format: format.format }
    for_ items \({ date, format }) -> do
      FDT.unformat format (FDT.format format date) `shouldEqual` (Right date)


invalidDateformats ∷ Array { str :: String , pos :: String }
invalidDateformats =
  [ { str: "YY-Q-dddd HH:mm Z", pos: "1:4" }
  , { str: "YYYY-MM-DD Q", pos: "1:12" }
  ]

dateformats ∷ Array { str :: String , lossless :: Boolean, format :: FDT.Formatter }
dateformats =
  [ { str: "YYYY-MM-DD"
    , lossless: false
    , format:
      roll $ FDT.YearFull $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.MonthTwoDigits $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.DayOfMonthTwoDigits $
      roll FDT.End
    }
  , { str: "Y-MM-DD HH:mm:ss:SSS"
    , lossless: true
    , format:
      roll $ FDT.YearAbsolute $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.MonthTwoDigits $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.DayOfMonthTwoDigits $
      roll $ FDT.Placeholder " " $
      roll $ FDT.Hours24 $
      roll $ FDT.Placeholder ":" $
      roll $ FDT.Minutes $
      roll $ FDT.Placeholder ":" $
      roll $ FDT.Seconds $
      roll $ FDT.Placeholder ":" $
      roll $ FDT.Milliseconds $
      roll FDT.End
    }
  , { str: "YY-Z-DD HH:mm Z"
    , lossless: false
    , format:
      roll $ FDT.YearTwoDigits $
      roll $ FDT.Placeholder "-Z-" $
      roll $ FDT.DayOfMonthTwoDigits $
      roll $ FDT.Placeholder " " $
      roll $ FDT.Hours24 $
      roll $ FDT.Placeholder ":" $
      roll $ FDT.Minutes $
      roll $ FDT.Placeholder " Z" $
      roll FDT.End
    }
  , { str: "DD-MM-YYYY trololo HH-:-mm"
    , lossless: false
    , format:
      roll $ FDT.DayOfMonthTwoDigits $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.MonthTwoDigits $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.YearFull $
      roll $ FDT.Placeholder " trololo " $
      roll $ FDT.Hours24 $
      roll $ FDT.Placeholder "-:-" $
      roll $ FDT.Minutes $
      roll FDT.End
    }
  , { str: "YYYY-DD-MM SSS"
    , lossless: false
    , format:
      roll $ FDT.YearFull $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.DayOfMonthTwoDigits $
      roll $ FDT.Placeholder "-" $
      roll $ FDT.MonthTwoDigits $
      roll $ FDT.Placeholder " " $
      roll $ FDT.Milliseconds $
      roll FDT.End
    }
  ]

filter :: ∀ m a. Alternative m => Monad m => (a -> Boolean) -> m a -> m a
filter f m = m >>= \x -> if f x then pure x else empty

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  intervalTest
  timeTest
--     --numeralTests
