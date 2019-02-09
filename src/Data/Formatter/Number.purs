-- | This module has no support of percents and currencies.
-- | Please, note that using simple formatter that tabulates number with
-- | zeros and put commas between thousands should be enough for everything
-- | because one could just compose it with `flip append "%"` or whatever
module Data.Formatter.Number
  ( Formatter(..)
  , printFormatter
  , parseFormatString
  , format
  , unformat
  , formatNumber
  , formatOrShowNumber
  , unformatNumber
  ) where

import Prelude

import Data.Array (many, some)
import Data.Array as Arr
import Data.Either (Either, either)
import Data.Formatter.Internal (foldDigits, repeat)
import Data.Formatter.Parser.Number (parseDigit)
import Data.Formatter.Parser.Utils (runP)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.String as Str
import Data.String.CodeUnits as CU
import Data.Traversable (for)
import Math as Math
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS


newtype Formatter = Formatter
  { comma ∷ Boolean
  , before ∷ Int
  , after ∷ Int
  , abbreviations ∷ Boolean
  , sign ∷ Boolean
  }

derive instance genericFormatter ∷ Generic Formatter _
derive instance newtypeFormatter ∷ Newtype Formatter _

instance showFormatter ∷ Show Formatter where
  show = genericShow

derive instance eqFormatter ∷ Eq Formatter

printFormatter ∷ Formatter → String
printFormatter (Formatter f) =
    (if f.sign then "+" else "")
    <> repeat "0" (f.before - one)
    <> (if f.comma then "0,0" else "0")
    <> (if f.after > zero then "." else "")
    <> (repeat "0" f.after)
    <> (if f.abbreviations then "a" else "")

parseFormatString ∷ String → Either String Formatter
parseFormatString = runP formatParser


formatParser ∷ P.Parser String Formatter
formatParser = do
  sign ← PC.optionMaybe $ PC.try $ PS.string "+"
  before ← some $ PS.string "0"
  comma ← PC.optionMaybe $ PC.try $ PS.string ",0"
  dot ← PC.optionMaybe $ PC.try $ PS.string "."
  after ← for dot \_ →
    PC.try $ many $ PS.string "0"
  abbreviations ← PC.optionMaybe $ PC.try $ PS.string "a"

  pure $ Formatter
    { sign: isJust sign
    , before: Arr.length before
    , comma: isJust comma
    , after: fromMaybe zero $ Arr.length <$> after
    , abbreviations: isJust abbreviations
    }


format ∷ Formatter → Number → String
format (Formatter f) num =
  let
    absed = Math.abs num
    tens =
      if absed > 0.0
        then max (Int.floor $ Math.log absed / Math.ln10) 0
        else 0
  in if f.abbreviations
     then
       let
         thousands = tens / 3
         abbr | thousands == 0 = ""
              | thousands == 1 = "K"
              | thousands == 2 = "M"
              | thousands == 3 = "G"
              | thousands == 4 = "T"
              | thousands == 5 = "P"
              | thousands == 6 = "E"
              | thousands == 7 = "Z"
              | thousands == 8 = "Y"
              | otherwise = "10e+" <> show (thousands * 3)
         newNum = if thousands < 1 then num else num / Math.pow 1000.0 (Int.toNumber thousands)
       in
        format (Formatter f{abbreviations = false}) newNum <> abbr
     else
       let
         zeros = f.before - tens - one
         integer = Int.floor absed
         leftover = absed - Int.toNumber integer
         rounded = Int.round $ leftover * (Math.pow 10.0 (Int.toNumber f.after))
         roundedWithZeros =
           let roundedString = show rounded
               roundedLength = Str.length roundedString
               zeros' = repeat "0" (f.after - roundedLength)
           in zeros' <> roundedString
         shownNumber =
           if f.comma
             then addCommas [] zero (Arr.reverse (CU.toCharArray (repeat "0" zeros <> show integer)))
             else repeat "0" zeros <> show integer

         addCommas ∷ Array Char → Int → Array Char → String
         addCommas acc counter input = case Arr.uncons input of
           Nothing → CU.fromCharArray acc
           Just {head, tail} | counter < 3 →
             addCommas (Arr.cons head acc) (counter + one) tail
           _ →
             addCommas (Arr.cons ',' acc) zero input
       in
        (if num < zero then "-" else if num > zero && f.sign then "+" else "")
        <> shownNumber
        <> (if f.after < 1
              then ""
              else
              "."
              <> (if rounded == 0 then repeat "0" f.after else "")
              <> (if rounded > 0 then roundedWithZeros else ""))


unformat ∷ Formatter → String → Either String Number
unformat = runP <<< unformatParser

unformatParser ∷ Formatter → P.Parser String Number
unformatParser (Formatter f) = do
  minus ← PC.optionMaybe $ PC.try $ PS.string "-"
  sign ← case minus of
    Nothing | f.sign →
      (PS.string "+") $> 1.0
    Nothing | otherwise →
      pure 1.0
    Just _ →
      pure (-1.0)

  let
    digitsWithCommas ∷ P.Parser String (Array Int)
    digitsWithCommas =
      if not f.comma
        then do
        some parseDigit <* PS.string "."
        else
        digitsWithCommas' [ ]

    digitsWithCommas' ∷ Array Int → P.Parser String (Array Int)
    digitsWithCommas' accum = do
      ds ← some parseDigit

      when (Arr.null accum && Arr.length ds > 3)
        $ P.fail "Wrong number of digits between thousand separators"
      when (Arr.length ds /= 3)
        $ P.fail "Wrong number of digits between thousand separators"

      sep ← PS.oneOf [',', '.']
      case sep of
        '.' → pure $ accum <> ds
        ',' → digitsWithCommas' $ accum <> ds
        _ → P.fail "Incorrect symbol, expected ',' or '.'"

  beforeDigits ← digitsWithCommas
  before ←
    if Arr.length beforeDigits < f.before
    then P.fail "Error: too few digits before dot"
    else pure $ Int.toNumber $ foldDigits beforeDigits

  afterDigits ← some parseDigit
  after ←
    if Arr.length afterDigits < f.after
    then P.fail "Error: too few digits after dot"
    else pure $ Int.toNumber $ foldDigits afterDigits

  abbr ←
    if f.abbreviations
      then do
      letter ← PC.optionMaybe $ PC.try $ PS.oneOf ['K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y']
      case letter of
        Nothing → do
          e ← PC.optionMaybe $ PS.string "10e+"
          case e of
            Nothing →
              pure 0
            Just _ →
              map foldDigits $ many parseDigit
        Just 'K' → pure 3
        Just 'M' → pure 6
        Just 'G' → pure 9
        Just 'T' → pure 12
        Just 'P' → pure 15
        Just 'E' → pure 18
        Just 'Z' → pure 21
        Just 'Y' → pure 24
        _ → pure 0
      else pure 0
  pure
    $ Math.pow 10.0 (Int.toNumber abbr)
    * sign
    * (before + after / Math.pow 10.0 (Int.toNumber f.after))

formatNumber ∷ String → Number → Either String String
formatNumber pattern number =
  parseFormatString pattern <#> flip format number

unformatNumber ∷ String → String → Either String Number
unformatNumber pattern str =
  parseFormatString pattern >>= flip unformat str

-- Supposed to be used in chaining, because after calling format number there is no
-- good way to extract number back to show.
formatOrShowNumber ∷ String → Number → String
formatOrShowNumber patter number =
  either (const $ show number) identity
  $ formatNumber patter number
