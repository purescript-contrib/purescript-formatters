module Utils.Formatter where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Monoid (class Monoid, mempty)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as PS

foldDigits ∷ ∀ f. Foldable f ⇒ f Int → Int
foldDigits = foldl (\acc d → acc * 10 + d) zero

digit ∷ ∀ m. Monad m ⇒ P.ParserT String m Int
digit = do
  char ← PS.oneOf ['0','1','2','3','4','5','6','7','8','9']
  case char of
    '0' → pure 0
    '1' → pure 1
    '2' → pure 2
    '3' → pure 3
    '4' → pure 4
    '5' → pure 5
    '6' → pure 6
    '7' → pure 7
    '8' → pure 8
    '9' → pure 9
    _ → P.fail "Incorrect digit, impossible situation"

repeat ∷ ∀ a. Monoid a ⇒ a → Int → a
repeat = repeat' mempty
  where
  repeat' ∷ a → a → Int → a
  repeat' accum part count
    | count < one = accum
  repeat' accum part count =
    repeat' (accum <> part) part (count - one)
