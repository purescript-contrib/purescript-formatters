module Test.DateTime (datetimeTest) where

import Prelude


import Data.Date as D
import Data.Time as T
import Data.DateTime as DT
import Data.Foldable (for_)
import Data.Formatter.DateTime as FDT
import Control.Monad.Aff (Aff)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Control.MonadZero (guard)
import Data.Enum (toEnum)
import Data.Functor.Mu (roll)
import Data.Maybe (fromMaybe)
import Control.Alternative (class Alternative, empty)

import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

datetimeTest :: forall e. Spec e Unit
datetimeTest = describe "Data.Formatter.DateTime" do
  describe "formatDateTime" do
    it "should formatt dateTime" do
      let
        items =
          [ { format: "MM/DD/YYYY", dateStr: "04/12/2017" , date: makeDateTime 2017 4 12 11 3 4 234}
          , { format: "MMMM", dateStr: "April" , date: makeDateTime 2017 4 12 11 3 4 234}
          , { format: "YYYY-DD-MM", dateStr: "2017-12-04" , date: makeDateTime 2017 4 12 11 3 4 234}
          , { format: "YYYY-MMM", dateStr: "2017-Apr" , date: makeDateTime 2017 4 12 11 3 4 234}
          , { format: "MMM D", dateStr: "Apr 1" , date: makeDateTime 2017 4 1 0 0 0 0}
          , { format: "hh:mm:ss:SSS a", dateStr: "11:03:04:234 AM" , date: makeDateTime 2017 4 12 11 3 4 234}
          , { format: "YY", dateStr: "17" , date: makeDateTime 2017 4 12 11 3 4 234}
          , { format: "YY", dateStr: "17" , date: makeDateTime 20017 4 12 0 0 0 0} -- Format 20017 with YY
          , { format: "YY", dateStr: "00" , date: makeDateTime 0 4 12 0 0 0 0} -- Format 0 with YY
          , { format: "YY", dateStr: "01" , date: makeDateTime (-1) 4 12 0 0 0 0} -- Format -1 with YY
          , { format: "hh:m:s a", dateStr: "11:3:4 AM", date: testDateTime }
          , { format: "hh:mm:ss a", dateStr: "11:03:04 AM", date: testDateTime }
          , { format: "hh:mm:ss.SSS", dateStr: "11:12:30.123", date: makeDateTime 2017 4 10 11 12 30 123 }
          , { format: "hh:mm:ss.SSS", dateStr: "11:12:30.023", date: makeDateTime 2017 4 10 11 12 30 23 }
          , { format: "hh:mm:ss.SSS", dateStr: "11:12:30.003", date: makeDateTime 2017 4 10 11 12 30 3 }
          , { format: "hh:mm:ss.SS", dateStr: "11:12:30.12", date: makeDateTime 2017 4 10 11 12 30 123 }
          , { format: "hh:mm:ss.S", dateStr: "11:12:30.1", date: makeDateTime 2017 4 10 11 12 30 123 }
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


makeDateTime ∷ Int -> Int -> Int -> Int -> Int -> Int -> Int -> DT.DateTime
makeDateTime year month day hour minute second millisecond =
  DT.DateTime
    (D.canonicalDate (fromMaybe bottom $ toEnum year) (fromMaybe bottom $ toEnum month) (fromMaybe bottom $ toEnum day))
    (T.Time
       (fromMaybe bottom $ toEnum hour )
       (fromMaybe bottom $ toEnum minute )
       (fromMaybe bottom $ toEnum second )
       (fromMaybe bottom $ toEnum millisecond))

assertFormatting :: forall e. String -> String -> DateTime -> Aff e Unit
assertFormatting target' format dateTime = result `shouldEqual` target
  where
  result = FDT.formatDateTime format dateTime
  target = Right target'

dates :: Array DateTime
dates =
  [ makeDateTime 2017 4 12 11 3 4 234
  , makeDateTime 2017 4 1 0 0 0 0
  , makeDateTime 20017 4 12 0 0 0 0
  , makeDateTime 0 4 12 0 0 0 0
  , makeDateTime (-1) 4 12 0 0 0 0
  ]

invalidDateformats ∷ Array { str :: String , pos :: String }
invalidDateformats =
  [ { str: "YY-h-dddd HH:mm Z", pos: "1:4" }
  , { str: "YYYY-MM-DD M", pos: "1:12" }
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
      roll $ FDT.MinutesTwoDigits $
      roll $ FDT.Placeholder ":" $
      roll $ FDT.SecondsTwoDigits $
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
      roll $ FDT.MinutesTwoDigits $
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
      roll $ FDT.MinutesTwoDigits $
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
