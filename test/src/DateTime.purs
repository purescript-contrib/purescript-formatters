module Test.DateTime (datetimeTest) where

import Prelude

import Data.Formatter.DateTime as FDT
import Data.List (fromFoldable)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Control.MonadZero (guard)
import Control.Alternative (class Alternative, empty)
import Test.Spec (describe, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (forAll, makeDateTime)

datetimeTest ∷ ∀ e. Spec e Unit
datetimeTest = describe "Data.Formatter.DateTime" do
  forAll (\a → a.format <> " | " <> a.dateStr)
    "formatDateTime/unformaDateTime should formatt/unforma dateTime"
    [ { format: "MM/DD/YYYY", dateStr: "04/12/2017" , date: makeDateTime 2017 4 12 11 3 4 234}
    , { format: "MMMM", dateStr: "April" , date: makeDateTime 2017 4 12 11 3 4 234}
    , { format: "YYYY", dateStr: "17" , date: makeDateTime 17 0 0 0 0 0 0}
    , { format: "YYYY-DD-MM", dateStr: "2017-12-04" , date: makeDateTime 2017 4 12 11 3 4 234}
    , { format: "YYYY-MMM", dateStr: "2017-Apr" , date: makeDateTime 2017 4 12 11 3 4 234}
    , { format: "MMM D", dateStr: "Apr 1" , date: makeDateTime 2017 4 1 0 0 0 0}
    , { format: "hh:mm:ss:SSS a", dateStr: "11:03:04:234 AM" , date: makeDateTime 2017 4 12 11 3 4 234}
    , { format: "YY", dateStr: "17" , date: makeDateTime 2017 4 12 11 3 4 234}
    , { format: "YY", dateStr: "17" , date: makeDateTime 20017 4 12 0 0 0 0} -- Format 20017 with YY
    , { format: "YY", dateStr: "00" , date: makeDateTime 0 4 12 0 0 0 0} -- Format 0 with YY
    , { format: "YY", dateStr: "01" , date: makeDateTime (-1) 4 12 0 0 0 0} -- Format -1 with YY
    , { format: "hh:m:s a", dateStr: "11:3:4 AM", date: makeDateTime 2017 4 12 11 3 4 234 }
    , { format: "hh:mm:ss a", dateStr: "11:03:04 AM", date: makeDateTime 2017 4 12 11 3 4 234 }
    , { format: "hh:mm:ss.SSS", dateStr: "11:12:30.123", date: makeDateTime 2017 4 10 11 12 30 123 }
    , { format: "hh:mm:ss.SSS", dateStr: "11:12:30.023", date: makeDateTime 2017 4 10 11 12 30 23 }
    , { format: "hh:mm:ss.SSS", dateStr: "11:12:30.003", date: makeDateTime 2017 4 10 11 12 30 3 }
    , { format: "hh:mm:ss.SS", dateStr: "11:12:30.12", date: makeDateTime 2017 4 10 11 12 30 123 }
    , { format: "hh:mm:ss.S", dateStr: "11:12:30.1", date: makeDateTime 2017 4 10 11 12 30 123 }
    , { format: "hhmmss a", dateStr: "110304 AM", date: makeDateTime 2017 4 12 11 3 4 234 }
    , { format: "hhmmssSSS", dateStr: "111230123", date: makeDateTime 2017 4 10 11 12 30 123 }
    , { format: "hhmmssSSS", dateStr: "111230023", date: makeDateTime 2017 4 10 11 12 30 23 }
    , { format: "hhmmssSSS", dateStr: "111230003", date: makeDateTime 2017 4 10 11 12 30 3 }
    , { format: "hhmmssSS", dateStr: "11123012", date: makeDateTime 2017 4 10 11 12 30 123 }
    , { format: "hhmmssS", dateStr: "1112301", date: makeDateTime 2017 4 10 11 12 30 123 }
    , { format: "HHmmssSSS", dateStr: "134530123", date: makeDateTime 2017 4 10 13 45 30 123 }
    , { format: "HHmm", dateStr: "1345", date: makeDateTime 2017 4 10 13 45 30 123 }
    ]
    (\({ format, dateStr, date }) → do
      (format `FDT.formatDateTime` date) `shouldEqual` (Right dateStr)
      (void $ format `FDT.unformatDateTime` dateStr) `shouldEqual` (Right unit)
    )

  describe "parseFormatString" do
    forAll
      _.str
      "should parse"
      dateformats
      (\f → (FDT.parseFormatString f.str) `shouldEqual` (Right f.format))

    forAll
      _.str
     "shouldn't parse"
      invalidDateformats
      (\f → (FDT.parseFormatString f.str) `shouldEqual` (Left $ "Expected EOF@" <> f.pos))

  forAll
    (\a → a.format <> " | " <> a.date)
    "s ≡ format (unformat s)"
    [ {date: "2017-12-04 234", format: "YYYY-DD-MM SSS" }
    , {date: "3456-09-10 333", format: "YYYY-DD-MM SSS" }
    , {date: "111230003", format: "hhmmssSSS"}
    , {date: "11123012", format: "hhmmssSS"}
    , {date: "1112301", format: "hhmmssS"}

    ]
    (\({date, format}) → (FDT.unformatDateTime format date >>= FDT.formatDateTime format) `shouldEqual` (Right date))

  forAll
    (\a → show a.date <> "|" <> FDT.printFormatter a.format)
    "s ≡ unformat (format s)"
    (do
      format ← dateformats
      date ← dates
      guard format.lossless
      pure { date, format: format.format })
    (\({ date, format }) → FDT.unformat format (FDT.format format date) `shouldEqual` (Right date))



dates ∷ Array DateTime
dates =
  [ makeDateTime 2017 4 12 11 3 4 234
  , makeDateTime 2017 4 1 0 0 0 0
  , makeDateTime 20017 4 12 0 0 0 0
  , makeDateTime 0 4 12 0 0 0 0
  , makeDateTime (-1) 4 12 0 0 0 0
  ]

invalidDateformats ∷ Array { str ∷ String , pos ∷ String }
invalidDateformats =
  [ { str: "YY-h-dddd HH:mm Z", pos: "1:4" }
  , { str: "YYYY-MM-DD M", pos: "1:12" }
  , { str: "YYYYM", pos: "1:5" }
  ]

dateformats ∷ Array { str ∷ String , lossless ∷ Boolean, format ∷ FDT.Formatter }
dateformats =
  [ { str: "YYYY-MM-DD"
    , lossless: false
    , format: fromFoldable
      [ FDT.YearFull
      , FDT.Placeholder "-"
      , FDT.MonthTwoDigits
      , FDT.Placeholder "-"
      , FDT.DayOfMonthTwoDigits
      ]
    }
  , { str: "Y-MM-DD HH:mm:ss:SSS"
    , lossless: true
    , format: fromFoldable
      [ FDT.YearAbsolute
      , FDT.Placeholder "-"
      , FDT.MonthTwoDigits
      , FDT.Placeholder "-"
      , FDT.DayOfMonthTwoDigits
      , FDT.Placeholder " "
      , FDT.Hours24
      , FDT.Placeholder ":"
      , FDT.MinutesTwoDigits
      , FDT.Placeholder ":"
      , FDT.SecondsTwoDigits
      , FDT.Placeholder ":"
      , FDT.Milliseconds
      ]
    }
  , { str: "YY-Z-DD HH:mm Z"
    , lossless: false
    , format: fromFoldable
      [ FDT.YearTwoDigits
      , FDT.Placeholder "-Z-"
      , FDT.DayOfMonthTwoDigits
      , FDT.Placeholder " "
      , FDT.Hours24
      , FDT.Placeholder ":"
      , FDT.MinutesTwoDigits
      , FDT.Placeholder " Z"
      ]
    }
  , { str: "DD-MM-YYYY trololo HH-:-mm"
    , lossless: false
    , format: fromFoldable
      [ FDT.DayOfMonthTwoDigits
      , FDT.Placeholder "-"
      , FDT.MonthTwoDigits
      , FDT.Placeholder "-"
      , FDT.YearFull
      , FDT.Placeholder " trololo "
      , FDT.Hours24
      , FDT.Placeholder "-:-"
      , FDT.MinutesTwoDigits
      ]
    }
  , { str: "YYYY-DD-MM SSS"
    , lossless: false
    , format: fromFoldable
      [ FDT.YearFull
      , FDT.Placeholder "-"
      , FDT.DayOfMonthTwoDigits
      , FDT.Placeholder "-"
      , FDT.MonthTwoDigits
      , FDT.Placeholder " "
      , FDT.Milliseconds
      ]
    }
  ]

filter ∷ ∀ m a. Alternative m ⇒ Monad m ⇒ (a → Boolean) → m a → m a
filter f m = m >>= \x → if f x then pure x else empty
