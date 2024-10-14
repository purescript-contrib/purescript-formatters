module Test.Number (numberTest) where

import Prelude

import Control.Monad.Reader.Class (class MonadReader)
import Data.Either (Either(..))
import Data.Formatter.Number (Formatter(..), format, parseFormatString, printFormatter, unformat, withSeparators)
import Effect.Aff.Class (class MonadAff)
import Test.Utils (forAll, describe, shouldEqual)

numberTest :: forall m. MonadReader Int m => MonadAff m => m Unit
numberTest = describe "Data.Formatter.Number" do
  forAll _.str
    "should print formatter"
    numberformatts
    (\({ fmt, str }) -> printFormatter fmt `shouldEqual` str)

  forAll _.str
    "should print formatter with different separators the same"
    numberformatts
    (\({ fmt, str }) -> printFormatter (germanStyleSeparators fmt) `shouldEqual` str)

  forAll _.str
    "parse format string"
    numberformatts
    (\({ fmt, str }) -> parseFormatString str `shouldEqual` (Right fmt))

  forAll show
    "unformat (format n) = n"
    [ 100.2, 100.1, 100.3, 10004000.0, -100.2, -100.1, -100.3, -10004000.0 ]
    (\n -> unformat fmt1 (format fmt1 n) `shouldEqual` (Right n))

  forAll show
    "unformat (format n) = n"
    [ 100.2, 100.1, 100.3, 10004000.0, -100.2, -100.1, -100.3, -10004000.0 ]
    (\n -> unformat fmt1 (format fmt1 n) `shouldEqual` (Right n))

  forAll show
    "unformat (format n) = n for changed separators"
    [ 100.2, 100.1, 100.3, 10004000.0, -100.2, -100.1, -100.3, -10004000.0 ]
    (\n -> unformat (germanStyleSeparators fmt1) (format (germanStyleSeparators fmt1) n) `shouldEqual` (Right n))

  forAll show
    "format (unformat n) = n"
    [ "001.12", "001.02", "-001.12", "-001.02" ]
    (\n -> (format fmt1 <$> (unformat fmt1 n)) `shouldEqual` (Right n))

  forAll show
    "format (unformat n) = n for changed separators"
    [ "1,12", "1,02", "-1,12", "-1,02", "-1.012,33" ]
    (\n -> (format (germanStyleSeparators fmt1') <$> (unformat (germanStyleSeparators fmt1') n)) `shouldEqual` (Right n))

  forAll show
    "format (unformat n) = n"
    [ "+02.12", "+13.12", "-02.12", "-13.12" ]
    (\n -> (format fmt3 <$> (unformat fmt3 n)) `shouldEqual` (Right n))

  forAll show
    "format (unformat n) = n for changed separators"
    [ "+02,12", "+13,12", "-02,12", "-13,12" ]
    (\n -> (format (germanStyleSeparators fmt3) <$> (unformat (germanStyleSeparators fmt3) n)) `shouldEqual` (Right n))

  forAll (\{ fmt: (Formatter fmt), input } -> "rounds up " <> show input <> " (" <> show fmt.after <> " digits)")
    "rounding"
    [ { fmt: fmt4, input: 1.99999, expected: "02" }
    , { fmt: fmt1, input: 1.99999, expected: "002.00" }
    , { fmt: fmt5, input: 1.99999, expected: "2.0000" }
    , { fmt: fmt1, input: 1.89999, expected: "001.90" }
    , { fmt: fmt5, input: 1.67899, expected: "1.6790" }
    , { fmt: fmt6, input: 12.9, expected: "13" }
    , { fmt: fmt7, input: 1.123456789012345678901234, expected: "1.1234567890123457" }
    , { fmt: fmt6, input: 12345678901234567.8901234, expected: "12,345,678,901,234,568" }
    , { fmt: fmt5, input: 123456789012.345678901234, expected: "123,456,789,012.3457" }
    ]
    ( \{ fmt, input, expected } -> do
        format fmt input `shouldEqual` expected
        format fmt (negate input) `shouldEqual` ("-" <> expected)
    )

  forAll (\{ fmt: (Formatter fmt), input } -> "rounds up " <> show input <> " (" <> show fmt.after <> " digits)")
    "rounding for changed separators"
    [ { fmt: germanStyleSeparators fmt4, input: 1.99999, expected: "02" }
    , { fmt: germanStyleSeparators fmt1, input: 1.99999, expected: "002,00" }
    , { fmt: germanStyleSeparators fmt5, input: 1.99999, expected: "2,0000" }
    , { fmt: germanStyleSeparators fmt1, input: 1.89999, expected: "001,90" }
    , { fmt: germanStyleSeparators fmt5, input: 1.67899, expected: "1,6790" }
    , { fmt: germanStyleSeparators fmt6, input: 12.9, expected: "13" }
    , { fmt: germanStyleSeparators fmt7, input: 1.123456789012345678901234, expected: "1,1234567890123457" }
    , { fmt: germanStyleSeparators fmt6, input: 12345678901234567.8901234, expected: "12.345.678.901.234.568" }
    , { fmt: germanStyleSeparators fmt5, input: 123456789012.345678901234, expected: "123.456.789.012,3457" }
    ]
    ( \{ fmt, input, expected } -> do
        format fmt input `shouldEqual` expected
        format fmt (negate input) `shouldEqual` ("-" <> expected)
    )

fmt1 :: Formatter
fmt1 = Formatter
  { comma: false
  , before: 3
  , after: 2
  , abbreviations: false
  , sign: false
  , groupSeparator: ','
  , decimalSeparator: '.'
  }

fmt1' :: Formatter
fmt1' = Formatter
  { comma: true
  , before: 0
  , after: 2
  , abbreviations: false
  , sign: false
  , groupSeparator: ','
  , decimalSeparator: '.'
  }

fmt2 :: Formatter
fmt2 = Formatter
  { comma: true
  , before: 1
  , after: 4
  , abbreviations: false
  , sign: true
  , groupSeparator: ','
  , decimalSeparator: '.'
  }

fmt3 :: Formatter
fmt3 = Formatter
  { comma: false
  , before: 2
  , after: 2
  , abbreviations: true
  , sign: true
  , groupSeparator: ','
  , decimalSeparator: '.'
  }

fmt4 :: Formatter
fmt4 = Formatter
  { comma: false
  , before: 2
  , after: 0
  , abbreviations: false
  , sign: false
  , groupSeparator: ','
  , decimalSeparator: '.'
  }

fmt5 :: Formatter
fmt5 = Formatter
  { comma: true
  , before: 1
  , after: 4
  , abbreviations: false
  , sign: false
  , groupSeparator: ','
  , decimalSeparator: '.'
  }

fmt6 :: Formatter
fmt6 = Formatter
  { comma: true
  , before: 1
  , after: -1
  , abbreviations: false
  , sign: false
  , groupSeparator: ','
  , decimalSeparator: '.'
  }

fmt7 :: Formatter
fmt7 = Formatter
  { comma: true
  , before: 1
  , after: 16
  , abbreviations: false
  , sign: false
  , groupSeparator: ','
  , decimalSeparator: '.'
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

germanStyleSeparators :: Formatter -> Formatter
germanStyleSeparators = withSeparators { decimalSeparator: ',', groupSeparator: '.' }
