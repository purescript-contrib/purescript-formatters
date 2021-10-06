module Data.Formatter.Parser.Number
  ( parseInteger
  , parseMaybeInteger
  , parseNumber
  , parseDigit
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Array (some)
import Data.Formatter.Internal (foldDigits)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Data.Formatter.Parser.Utils (oneOfAs)
import Text.Parsing.Parser.String as PS
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Foldable (foldMap)

parseInteger ∷ ∀ m. Monad m ⇒ P.ParserT String m Int
parseInteger = some parseDigit <#> foldDigits

parseMaybeInteger ∷ ∀ m. Monad m ⇒ P.ParserT String m (Maybe Int)
parseMaybeInteger = PC.optionMaybe parseInteger

parseFractional ∷ ∀ m. Monad m ⇒ P.ParserT String m Number
parseFractional = do
  digitStr <- (some parseDigit) <#> (foldMap show >>> ("0." <> _))
  case fromString digitStr of
      Just n -> pure n
      Nothing -> P.fail ("Not a number: " <> digitStr)

parseNumber ∷ ∀ m. Monad m ⇒ P.ParserT String m Number
parseNumber = (+)
  <$> (parseInteger <#> toNumber)
  <*> (PC.option 0.0 $ PC.try $ PS.oneOf ['.', ','] *> parseFractional)


parseDigit ∷ ∀ m. Monad m ⇒ P.ParserT String m Int
parseDigit = PC.try $ PS.char `oneOfAs`
  [ Tuple '0' 0
  , Tuple '1' 1
  , Tuple '2' 2
  , Tuple '3' 3
  , Tuple '4' 4
  , Tuple '5' 5
  , Tuple '6' 6
  , Tuple '7' 7
  , Tuple '8' 8
  , Tuple '9' 9]
