module Data.Formatter.Parser.Utils
  ( oneOfAs
  , runP
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParserT, Parser, runParser, ParseError, parseErrorMessage, parseErrorPosition)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable)
import Data.Either (Either)

oneOfAs :: forall c s m f a b. Functor f => Foldable f => Monad m => (a -> ParserT s m b) -> f (Tuple a c) -> ParserT s m c
oneOfAs p xs = PC.choice $ (\(Tuple s r) -> p s $> r) <$> xs

runP :: forall a. Parser String a -> String -> Either String a
runP p s = lmap printError $ runParser s (p <* PS.eof)

printError :: ParseError -> String
printError err = parseErrorMessage err <> " " <> (printPosition $ parseErrorPosition err)

printPosition :: Position -> String
printPosition (Position { line, column }) = "(line " <> show line <> ", col " <> show column <> ")"
