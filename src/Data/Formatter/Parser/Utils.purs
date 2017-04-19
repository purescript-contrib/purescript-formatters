module Data.Formatter.Parser.Utils where

import Prelude

import Data.Tuple (Tuple(..))
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable)
import Data.Either (Either)

oneOfAs :: ∀ c s m f a b. Functor f => Foldable f => Monad m => (a -> P.ParserT s m b) -> f (Tuple a c) -> P.ParserT s m c
oneOfAs p xs = PC.choice $ (\(Tuple s r) -> p s $> r) <$> xs

runP :: ∀ a. P.Parser String a → String → Either String a
runP p s = lmap P.parseErrorMessage $ P.runParser s p
