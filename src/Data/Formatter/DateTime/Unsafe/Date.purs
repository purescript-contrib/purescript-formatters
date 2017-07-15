module Data.Formatter.DateTime.Unsafe.Date where

import Prelude

import Data.Formatter.DateTime.Commands (FormatterCommand)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)

newtype Formatter = Formatter (List FormatterCommand)
derive instance eqFormatter ∷ Eq Formatter
derive instance genericFormatter ∷ Generic Formatter _
instance showFormatter ∷ Show Formatter where
  show = genericShow
