module Test.Number (numberTest) where

import Prelude

import Data.Foldable (for_)
import Data.Formatter.Number (Formatter(..), printFormatter, parseFormatString, format, unformat)
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))


import Test.Spec (describe, it, pending, Spec)
import Test.Spec.Assertions (shouldEqual)

numberTest :: forall e. Spec e Unit
numberTest = describe "Data.Formatter.Number" do
  it "should print formatter" do
    for_ numberformatts \({fmt, str}) -> do
      printFormatter fmt `shouldEqual` str

  it "parse format string" do
    for_ numberformatts \({fmt, str}) -> do
      parseFormatString str `shouldEqual` (Right fmt)

  it "unformat (format n) = n" do
    let ns = [100.2, 100.1, 100.3, 10004000.0]
    for_ ns \n -> do
      unformat fmt1 (format fmt1 n) `shouldEqual` (Right n)

  -- TODO fails on negative numbers
  pending' "format (unformat n) = n" do
    let ns = ["001.12", "-012.12", "-123.12"]
    for_ ns \n -> do
      (format fmt1 <$> (unformat fmt1 n)) `shouldEqual` (Right n)
    -- TODO check for different formatters
    -- DT.traceAnyA $ unformat fnThree "+123"
    -- DT.traceAnyA $ unformat fnTwo "-100,000.1234"


-- TODO remove after https://github.com/owickstrom/purescript-spec/pull/48
pending' :: forall r. String
        -> Aff r Unit
        -> Spec r Unit
pending' name _ = pending name

fmt1 :: Formatter
fmt1 = Formatter
  { comma: false
  , before: 3
  , after: 2
  , abbreviations: false
  , sign: false
  }

fmt2 :: Formatter
fmt2 = Formatter
  { comma: true
  , before: one
  , after: 4
  , abbreviations: false
  , sign: true
  }

fmt3 :: Formatter
fmt3 = Formatter
  { comma: false
  , before: 2
  , after: 2
  , abbreviations: true
  , sign: true
  }

numberformatts :: Array { fmt :: Formatter, str :: String }
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
