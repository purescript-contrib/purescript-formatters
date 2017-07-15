module Data.Formatter.DateTime.Time where

import Prelude

import Data.DateTime (DateTime(..), Time, time)
import Data.Either (Either)
import Data.Formatter.DateTime as FD
import Data.Formatter.DateTime (fromTimeFormatter)
import Data.Formatter.DateTime.Unsafe.Time (Formatter(..)) as F
import Data.Formatter.DateTime.Types (kind FormatList, FProxy)
import Data.Formatter.DateTime.Unsafe.Formatter (class PrintFormatI, class ValidTime, printFormatI)
import Data.List (List(..))

format ∷ F.Formatter → Time → String
format fmt x = FD.format (fromTimeFormatter fmt) $ DateTime bottom x

formatTime ∷ String → Time → Either String String
formatTime fmt x = FD.formatDateTime fmt $ DateTime bottom x

unformat ∷ F.Formatter → String → Either String Time
unformat fmt x = FD.unformat (fromTimeFormatter fmt) x <#> time

unformatTime ∷ String → String → Either String Time
unformatTime fmt x = FD.unformatDateTime fmt x <#> time


class PrintFormat (fmt ∷ FormatList) where
  printFormat ∷ FProxy fmt → F.Formatter

instance pf ∷ (ValidTime a, PrintFormatI a) ⇒ PrintFormat a where
  printFormat prx = F.Formatter $ printFormatI prx Nil
