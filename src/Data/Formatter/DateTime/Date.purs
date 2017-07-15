module Data.Formatter.DateTime.Date where

import Prelude

import Data.DateTime (DateTime(..), Date, date)
import Data.Either (Either)
import Data.Formatter.DateTime as FD
import Data.Formatter.DateTime (fromDateFormatter)
import Data.Formatter.DateTime.Unsafe.Date (Formatter(..)) as F
import Data.Formatter.DateTime.Types (kind FormatList, FProxy)
import Data.Formatter.DateTime.Unsafe.Formatter (class PrintFormatI, class ValidDate, printFormatI)
import Data.List (List(..))

format ∷ F.Formatter → Date → String
format fmt x = FD.format (fromDateFormatter fmt) $ DateTime x bottom

formatTime ∷ String → Date → Either String String
formatTime fmt x = FD.formatDateTime fmt $ DateTime x bottom

unformat ∷ F.Formatter → String → Either String Date
unformat fmt x = FD.unformat (fromDateFormatter fmt) x <#> date

unformatTime ∷ String → String → Either String Date
unformatTime fmt x = FD.unformatDateTime fmt x <#> date


class PrintFormat (fmt ∷ FormatList) where
  printFormat ∷ FProxy fmt → F.Formatter

instance pf ∷ (ValidDate a, PrintFormatI a) ⇒ PrintFormat a where
  printFormat prx = F.Formatter $ printFormatI prx Nil
