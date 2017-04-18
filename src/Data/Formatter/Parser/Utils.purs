module Data.Formatter.Parser.Utils where

import Prelude

import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Combinators as PC

oneOfAs p xs = PC.choice $ (\(Tuple s r) -> p s $> r) <$> xs
