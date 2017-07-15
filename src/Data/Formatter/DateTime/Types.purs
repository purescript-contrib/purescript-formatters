module Data.Formatter.DateTime.Types
  ( YearFull
  , YearTwoDigits
  , YearAbsolute
  , MonthFull
  , MonthShort
  , MonthTwoDigits
  , DayOfMonthTwoDigits
  , DayOfMonth
  , UnixTimestamp
  , DayOfWeek
  , Hours24
  , Hours12
  , Meridiem
  , Minutes
  , MinutesTwoDigits
  , Seconds
  , SecondsTwoDigits
  , Milliseconds
  , MillisecondsShort
  , MillisecondsTwoDigits
  , Placeholder
  , FCons
  , FNil
  , type (:)
  , FProxy(..)
  , kind FormatAtom
  , kind FormatList
  ) where


foreign import kind FormatAtom
foreign import kind FormatList

foreign import data FCons ∷ FormatAtom → FormatList → FormatList
foreign import data FNil ∷ FormatList

infixr 6 type FCons as :

foreign import data YearFull ∷ FormatAtom
foreign import data YearTwoDigits ∷ FormatAtom
foreign import data YearAbsolute ∷ FormatAtom
foreign import data MonthFull ∷ FormatAtom
foreign import data MonthShort ∷ FormatAtom
foreign import data MonthTwoDigits ∷ FormatAtom
foreign import data DayOfMonthTwoDigits ∷ FormatAtom
foreign import data DayOfMonth ∷ FormatAtom
foreign import data UnixTimestamp ∷ FormatAtom
foreign import data DayOfWeek ∷ FormatAtom
foreign import data Hours24 ∷ FormatAtom
foreign import data Hours12 ∷ FormatAtom
foreign import data Meridiem ∷ FormatAtom
foreign import data Minutes ∷ FormatAtom
foreign import data MinutesTwoDigits ∷ FormatAtom
foreign import data Seconds ∷ FormatAtom
foreign import data SecondsTwoDigits ∷ FormatAtom
foreign import data Milliseconds ∷ FormatAtom
foreign import data MillisecondsShort ∷ FormatAtom
foreign import data MillisecondsTwoDigits ∷ FormatAtom
foreign import data Placeholder ∷ Symbol → FormatAtom

data FProxy (a ∷ FormatList) = FProxy
