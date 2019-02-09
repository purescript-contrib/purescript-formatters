module Test.Number (numberTest) where

import Prelude

import Data.Formatter.Number (Formatter(..), printFormatter, parseFormatString, format, unformat)
import Data.Either (Either(..))

import Test.Spec (describe, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (forAll)

numberTest ∷ Spec Unit
numberTest = describe "Data.Formatter.Number" do
  forAll _.str
    "should print formatter"
    numberformatts
    (\({fmt, str}) → printFormatter fmt `shouldEqual` str)

  forAll _.str
    "parse format string"
    numberformatts
    (\({fmt, str}) → parseFormatString str `shouldEqual` (Right fmt))

  forAll show
    "unformat (format n) = n"
    [100.2, 100.1, 100.3, 10004000.0, -100.2, -100.1, -100.3, -10004000.0]
    (\n → unformat fmt1 (format fmt1 n) `shouldEqual` (Right n))

  forAll show
    "format (unformat n) = n"
    ["001.12", "001.02", "-001.12", "-001.02"]
    (\n →  (format fmt1 <$> (unformat fmt1 n)) `shouldEqual` (Right n))

  forAll show
    "format (unformat n) = n"
    ["+02.12", "+13.12", "-02.12", "-13.12"]
    (\n →  (format fmt3 <$> (unformat fmt3 n)) `shouldEqual` (Right n))

fmt1 ∷ Formatter
fmt1 = Formatter
  { comma: false
  , before: 3
  , after: 2
  , abbreviations: false
  , sign: false
  }

fmt2 ∷ Formatter
fmt2 = Formatter
  { comma: true
  , before: 1
  , after: 4
  , abbreviations: false
  , sign: true
  }

fmt3 ∷ Formatter
fmt3 = Formatter
  { comma: false
  , before: 2
  , after: 2
  , abbreviations: true
  , sign: true
  }

numberformatts ∷ Array { fmt ∷ Formatter, str ∷ String }
numberformatts =
  [ { str: "000.00"
    , fmt: fmt1
    }
  , { str: "+0,0.0000"
    , fmt: fmt2
    }
  , { str: "+00.00a"
    , fmt: fmt3
    }
  ]
