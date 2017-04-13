module Data.Formatter.Parser.Number
  ( parseInteger
  , parseMaybeInteger
  , parseNumber
  ) where

import Prelude

import Data.Int (toNumber, floor)
import Data.Array (some, many, length)
import Data.Formatter.Internal (digit, foldDigits)
import Data.Function (on)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Data.Maybe (Maybe(..), maybe)
import Math as Math


parseInteger ∷ P.Parser String Int
parseInteger = some digit <#> foldDigits

parseMaybeInteger ∷ P.Parser String (Maybe Int)
parseMaybeInteger = many digit <#> (\l -> if length l == 0 then Nothing else Just $ foldDigits l)

parseFractional ∷ P.Parser String Number
parseFractional = parseInteger <#> case _ of
  0 ->  0.0
  n -> (toNumber n) / (pow 10 $ numOfDigits n)

parseNumber ∷ P.Parser String Number
parseNumber = (+)
  <$> (parseInteger <#> toNumber)
  <*> (PC.option 0.0 $ PC.try $ PS.oneOf ['.', ','] *> parseFractional)

pow :: Int -> Int -> Number
pow = Math.pow `on` toNumber

numOfDigits ∷ Int → Int
numOfDigits 0 = 0
numOfDigits n = 1 + (floor $ log10 $ toNumber n)

log10 ∷ Number → Number
log10 n = Math.log10e * Math.log n
