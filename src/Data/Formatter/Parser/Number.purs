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
import Data.Maybe (Maybe)
import Data.Foldable (foldMap)
import Global (readFloat)

parseInteger ∷ ∀ s m. Monad m ⇒ PS.StringLike s ⇒ P.ParserT s m Int
parseInteger = some parseDigit <#> foldDigits

parseMaybeInteger ∷ ∀ s m. Monad m ⇒ PS.StringLike s ⇒ P.ParserT s m (Maybe Int)
parseMaybeInteger = PC.optionMaybe parseInteger

parseFractional ∷ ∀ s m. Monad m ⇒ PS.StringLike s ⇒ P.ParserT s m Number
parseFractional = (some parseDigit) <#> (foldMap show >>> ("0." <> _) >>> readFloat)

parseNumber ∷ ∀ s m. Monad m ⇒ PS.StringLike s ⇒ P.ParserT s m Number
parseNumber = (+)
  <$> (parseInteger <#> toNumber)
  <*> (PC.option 0.0 $ PC.try $ PS.oneOf ['.', ','] *> parseFractional)


parseDigit ∷ ∀ s m. Monad m ⇒ PS.StringLike s ⇒ P.ParserT s m Int
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
