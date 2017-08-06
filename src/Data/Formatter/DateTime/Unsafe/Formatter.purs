module Data.Formatter.DateTime.Unsafe.Formatter where

import Prelude

import Data.Formatter.DateTime.Commands as FC
import Data.Formatter.DateTime.Types
  ( type (:)
  , kind FormatList
  , kind FormatAtom
  , DayOfMonth
  , DayOfMonthTwoDigits
  , DayOfWeek
  , FNil
  , FProxy(..)
  , Hours12
  , Hours24
  , Meridiem
  , Milliseconds
  , MillisecondsShort
  , MillisecondsTwoDigits
  , Minutes
  , MinutesTwoDigits
  , MonthFull
  , MonthShort
  , MonthTwoDigits
  , Placeholder
  , Seconds
  , SecondsTwoDigits
  , UnixTimestamp
  , YearAbsolute
  , YearFull
  , YearTwoDigits
  )
import Data.List (List(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)



foreign import kind HoursType
foreign import data NoHours ∷ HoursType -- initial state
foreign import data HoursType24 ∷ HoursType -- 24-hour time is present
foreign import data HoursType12 ∷ HoursType -- 12-hour time with meridiem is present
foreign import data Hours12Only ∷ HoursType -- 12-hour time without meridiem is present
foreign import data MeridiemOnly ∷ HoursType -- meridiem without 12-hour time is present

class ValidHoursType (ht ∷ HoursType)
instance vht1 ∷ ValidHoursType HoursType12
instance vht2 ∷ ValidHoursType HoursType24
instance vht3 ∷ ValidHoursType NoHours

class ValidateHours (fmt ∷ FormatList) (i ∷ HoursType) (o ∷ HoursType) | fmt i → o

instance vh1  ∷ ValidateHours rest HoursType24 o  ⇒ ValidateHours (Hours24  : rest) NoHours o
instance vh2  ∷ ValidateHours rest Hours12Only o  ⇒ ValidateHours (Hours12  : rest) NoHours o
instance vh3  ∷ ValidateHours rest MeridiemOnly o ⇒ ValidateHours (Meridiem : rest) NoHours o
instance vh4  ∷ ValidateHours rest HoursType12 o  ⇒ ValidateHours (Meridiem : rest) Hours12Only o
instance vh5  ∷ ValidateHours rest HoursType12 o  ⇒ ValidateHours (Hours12  : rest) MeridiemOnly o
instance vh0  ∷ ValidateHours FNil t t
instance vh6  ∷ ValidateHours rest i o ⇒ ValidateHours (YearFull               : rest) i o
instance vh7  ∷ ValidateHours rest i o ⇒ ValidateHours (YearTwoDigits          : rest) i o
instance vh8  ∷ ValidateHours rest i o ⇒ ValidateHours (YearAbsolute           : rest) i o
instance vh9  ∷ ValidateHours rest i o ⇒ ValidateHours (MonthFull              : rest) i o
instance vh10 ∷ ValidateHours rest i o ⇒ ValidateHours (MonthShort             : rest) i o
instance vh11 ∷ ValidateHours rest i o ⇒ ValidateHours (MonthTwoDigits         : rest) i o
instance vh12 ∷ ValidateHours rest i o ⇒ ValidateHours (DayOfMonthTwoDigits    : rest) i o
instance vh13 ∷ ValidateHours rest i o ⇒ ValidateHours (DayOfMonth             : rest) i o
instance vh14 ∷ ValidateHours rest i o ⇒ ValidateHours (UnixTimestamp          : rest) i o
instance vh15 ∷ ValidateHours rest i o ⇒ ValidateHours (DayOfWeek              : rest) i o
instance vh16 ∷ ValidateHours rest i o ⇒ ValidateHours (Minutes                : rest) i o
instance vh17 ∷ ValidateHours rest i o ⇒ ValidateHours (MinutesTwoDigits       : rest) i o
instance vh18 ∷ ValidateHours rest i o ⇒ ValidateHours (Seconds                : rest) i o
instance vh19 ∷ ValidateHours rest i o ⇒ ValidateHours (SecondsTwoDigits       : rest) i o
instance vh20 ∷ ValidateHours rest i o ⇒ ValidateHours (Milliseconds           : rest) i o
instance vh21 ∷ ValidateHours rest i o ⇒ ValidateHours (MillisecondsShort      : rest) i o
instance vh22 ∷ ValidateHours rest i o ⇒ ValidateHours (MillisecondsTwoDigits  : rest) i o
instance vh23 ∷ ValidateHours rest i o ⇒ ValidateHours (Placeholder s          : rest) i o





foreign import kind MinutesType
foreign import data NoMinutes ∷ MinutesType -- initial state
foreign import data MinutesUsed ∷ MinutesType -- one minute is present
foreign import data MinutesInvalid ∷ MinutesType -- more then one minute is present

class ValidMinutesType (ht ∷ MinutesType)
instance vmint1 ∷ ValidMinutesType MinutesUsed
instance vmint3 ∷ ValidMinutesType NoMinutes

class ValidateMinutes (fmt ∷ FormatList) (i ∷ MinutesType) (o ∷ MinutesType) | fmt i → o

instance vmin1  ∷ ValidateMinutes rest MinutesUsed o    ⇒ ValidateMinutes (MinutesTwoDigits : rest) NoMinutes o
instance vmin2  ∷ ValidateMinutes rest MinutesUsed o    ⇒ ValidateMinutes (Minutes          : rest) NoMinutes o
instance vmin3  ∷ ValidateMinutes rest MinutesInvalid o ⇒ ValidateMinutes (MinutesTwoDigits : rest) MinutesUsed o
instance vmin4  ∷ ValidateMinutes rest MinutesInvalid o ⇒ ValidateMinutes (Minutes          : rest) MinutesUsed o
instance vmin0  ∷ ValidateMinutes FNil t t
instance vmin6  ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (YearFull              : rest) i o
instance vmin7  ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (YearTwoDigits         : rest) i o
instance vmin8  ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (YearAbsolute          : rest) i o
instance vmin9  ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (MonthFull             : rest) i o
instance vmin10 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (MonthShort            : rest) i o
instance vmin11 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (MonthTwoDigits        : rest) i o
instance vmin12 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (DayOfMonthTwoDigits   : rest) i o
instance vmin13 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (DayOfMonth            : rest) i o
instance vmin14 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (UnixTimestamp         : rest) i o
instance vmin15 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (DayOfWeek             : rest) i o
instance vmin16 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (Hours24               : rest) i o
instance vmin17 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (Hours12               : rest) i o
instance vmin18 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (Meridiem              : rest) i o
instance vmin19 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (Seconds               : rest) i o
instance vmin20 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (SecondsTwoDigits      : rest) i o
instance vmin21 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (Milliseconds          : rest) i o
instance vmin22 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (MillisecondsShort     : rest) i o
instance vmin23 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (MillisecondsTwoDigits : rest) i o
instance vmin24 ∷ ValidateMinutes rest i o ⇒ ValidateMinutes (Placeholder a         : rest) i o





foreign import kind SecondsType
foreign import data NoSeconds ∷ SecondsType -- initial state
foreign import data SecondsUsed ∷ SecondsType -- one second is present
foreign import data SecondsInvalid ∷ SecondsType -- more then one second is present

class ValidSecondsType (ht ∷ SecondsType)
instance vst1 ∷ ValidSecondsType SecondsUsed
instance vst3 ∷ ValidSecondsType NoSeconds

class ValidateSeconds (fmt ∷ FormatList) (i ∷ SecondsType) (o ∷ SecondsType) | fmt i → o

instance vs1  ∷ ValidateSeconds rest SecondsUsed o    ⇒ ValidateSeconds (SecondsTwoDigits : rest) NoSeconds o
instance vs2  ∷ ValidateSeconds rest SecondsUsed o    ⇒ ValidateSeconds (Seconds          : rest) NoSeconds o
instance vs3  ∷ ValidateSeconds rest SecondsInvalid o ⇒ ValidateSeconds (SecondsTwoDigits : rest) SecondsUsed o
instance vs4  ∷ ValidateSeconds rest SecondsInvalid o ⇒ ValidateSeconds (Seconds          : rest) SecondsUsed o
instance vs0  ∷ ValidateSeconds FNil t t
instance vs6  ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (YearFull              : rest) i o
instance vs7  ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (YearTwoDigits         : rest) i o
instance vs8  ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (YearAbsolute          : rest) i o
instance vs9  ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (MonthFull             : rest) i o
instance vs10 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (MonthShort            : rest) i o
instance vs11 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (MonthTwoDigits        : rest) i o
instance vs12 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (DayOfMonthTwoDigits   : rest) i o
instance vs13 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (DayOfMonth            : rest) i o
instance vs14 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (UnixTimestamp         : rest) i o
instance vs15 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (DayOfWeek             : rest) i o
instance vs16 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (Hours24               : rest) i o
instance vs17 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (Hours12               : rest) i o
instance vs18 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (Meridiem              : rest) i o
instance vs19 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (Minutes               : rest) i o
instance vs20 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (MinutesTwoDigits      : rest) i o
instance vs21 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (Milliseconds          : rest) i o
instance vs22 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (MillisecondsShort     : rest) i o
instance vs23 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (MillisecondsTwoDigits : rest) i o
instance vs24 ∷ ValidateSeconds rest i o ⇒ ValidateSeconds (Placeholder a         : rest) i o







foreign import kind MillisecondsType
foreign import data NoMilliseconds ∷ MillisecondsType -- initial state
foreign import data MillisecondsUsed ∷ MillisecondsType -- one millisecond is present
foreign import data MillisecondsInvalid ∷ MillisecondsType -- more then one millisecond is present

class ValidMillisecondsType (ht ∷ MillisecondsType)
instance vmst1 ∷ ValidMillisecondsType MillisecondsUsed
instance vmst3 ∷ ValidMillisecondsType NoMilliseconds

class ValidateMilliseconds (fmt ∷ FormatList) (i ∷ MillisecondsType) (o ∷ MillisecondsType) | fmt i → o

instance vms1  ∷ ValidateMilliseconds rest MillisecondsUsed o    ⇒ ValidateMilliseconds (MillisecondsTwoDigits : rest) NoMilliseconds o
instance vms2  ∷ ValidateMilliseconds rest MillisecondsUsed o    ⇒ ValidateMilliseconds (MillisecondsShort     : rest) NoMilliseconds o
instance vms3  ∷ ValidateMilliseconds rest MillisecondsUsed o    ⇒ ValidateMilliseconds (Milliseconds          : rest) NoMilliseconds o
instance vms4  ∷ ValidateMilliseconds rest MillisecondsInvalid o ⇒ ValidateMilliseconds (MillisecondsTwoDigits : rest) MillisecondsUsed o
instance vms6  ∷ ValidateMilliseconds rest MillisecondsInvalid o ⇒ ValidateMilliseconds (MillisecondsShort     : rest) MillisecondsUsed o
instance vms7  ∷ ValidateMilliseconds rest MillisecondsInvalid o ⇒ ValidateMilliseconds (Milliseconds          : rest) MillisecondsUsed o
instance vms0  ∷ ValidateMilliseconds FNil t t
instance vms8  ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (YearFull            : rest) i o
instance vms9  ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (YearTwoDigits       : rest) i o
instance vms10 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (YearAbsolute        : rest) i o
instance vms11 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (MonthFull           : rest) i o
instance vms12 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (MonthShort          : rest) i o
instance vms13 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (MonthTwoDigits      : rest) i o
instance vms14 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (DayOfMonthTwoDigits : rest) i o
instance vms15 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (DayOfMonth          : rest) i o
instance vms16 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (UnixTimestamp       : rest) i o
instance vms17 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (DayOfWeek           : rest) i o
instance vms18 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (Hours24             : rest) i o
instance vms19 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (Hours12             : rest) i o
instance vms20 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (Meridiem            : rest) i o
instance vms21 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (Minutes             : rest) i o
instance vms22 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (MinutesTwoDigits    : rest) i o
instance vms24 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (Seconds             : rest) i o
instance vms25 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (SecondsTwoDigits    : rest) i o
instance vms26 ∷ ValidateMilliseconds rest i o ⇒ ValidateMilliseconds (Placeholder a       : rest) i o





foreign import kind YearType
foreign import data NoYear ∷ YearType -- initial state
foreign import data YearUsed ∷ YearType -- one Year is present
foreign import data YearInvalid ∷ YearType -- more then one Year is present

class ValidYearType (ht ∷ YearType)
instance vyt1 ∷ ValidYearType YearUsed
instance vyt2 ∷ ValidYearType NoYear

class ValidateYear (fmt ∷ FormatList) (i ∷ YearType) (o ∷ YearType) | fmt i → o

instance vy0  ∷ ValidateYear rest YearUsed o    ⇒ ValidateYear (YearFull      : rest) NoYear o
instance vy1  ∷ ValidateYear rest YearUsed o    ⇒ ValidateYear (YearTwoDigits : rest) NoYear o
instance vy2  ∷ ValidateYear rest YearUsed o    ⇒ ValidateYear (YearAbsolute  : rest) NoYear o
instance vy3  ∷ ValidateYear rest YearInvalid o ⇒ ValidateYear (YearFull      : rest) YearUsed o
instance vy4  ∷ ValidateYear rest YearInvalid o ⇒ ValidateYear (YearTwoDigits : rest) YearUsed o
instance vy5  ∷ ValidateYear rest YearInvalid o ⇒ ValidateYear (YearAbsolute  : rest) YearUsed o
instance vy6  ∷ ValidateYear FNil t t
instance vy7  ∷ ValidateYear rest i o ⇒ ValidateYear (MillisecondsTwoDigits : rest) i o
instance vy8  ∷ ValidateYear rest i o ⇒ ValidateYear (MillisecondsShort     : rest) i o
instance vy9  ∷ ValidateYear rest i o ⇒ ValidateYear (Milliseconds          : rest) i o
instance vy10 ∷ ValidateYear rest i o ⇒ ValidateYear (MonthFull             : rest) i o
instance vy11 ∷ ValidateYear rest i o ⇒ ValidateYear (MonthShort            : rest) i o
instance vy12 ∷ ValidateYear rest i o ⇒ ValidateYear (MonthTwoDigits        : rest) i o
instance vy13 ∷ ValidateYear rest i o ⇒ ValidateYear (DayOfMonthTwoDigits   : rest) i o
instance vy14 ∷ ValidateYear rest i o ⇒ ValidateYear (DayOfMonth            : rest) i o
instance vy15 ∷ ValidateYear rest i o ⇒ ValidateYear (UnixTimestamp         : rest) i o
instance vy16 ∷ ValidateYear rest i o ⇒ ValidateYear (DayOfWeek             : rest) i o
instance vy17 ∷ ValidateYear rest i o ⇒ ValidateYear (Hours24               : rest) i o
instance vy18 ∷ ValidateYear rest i o ⇒ ValidateYear (Hours12               : rest) i o
instance vy19 ∷ ValidateYear rest i o ⇒ ValidateYear (Meridiem              : rest) i o
instance vy20 ∷ ValidateYear rest i o ⇒ ValidateYear (Minutes               : rest) i o
instance vy21 ∷ ValidateYear rest i o ⇒ ValidateYear (MinutesTwoDigits      : rest) i o
instance vy22 ∷ ValidateYear rest i o ⇒ ValidateYear (Seconds               : rest) i o
instance vy24 ∷ ValidateYear rest i o ⇒ ValidateYear (SecondsTwoDigits      : rest) i o
instance vy26 ∷ ValidateYear rest i o ⇒ ValidateYear (Placeholder a         : rest) i o




foreign import kind MonthType
foreign import data NoMonth ∷ MonthType -- initial state
foreign import data MonthUsed ∷ MonthType -- one Month is present
foreign import data MonthInvalid ∷ MonthType -- more then one Month is present

class ValidMonthType (ht ∷ MonthType)
instance vmt0 ∷ ValidMonthType MonthUsed
instance vmt1 ∷ ValidMonthType NoMonth

class ValidateMonth (fmt ∷ FormatList) (i ∷ MonthType) (o ∷ MonthType) | fmt i → o

instance vm0  ∷ ValidateMonth rest MonthUsed o    ⇒ ValidateMonth (MonthFull      : rest) NoMonth o
instance vm1  ∷ ValidateMonth rest MonthUsed o    ⇒ ValidateMonth (MonthShort     : rest) NoMonth o
instance vm2  ∷ ValidateMonth rest MonthUsed o    ⇒ ValidateMonth (MonthTwoDigits : rest) NoMonth o
instance vm3  ∷ ValidateMonth rest MonthInvalid o ⇒ ValidateMonth (MonthFull      : rest) MonthUsed o
instance vm4  ∷ ValidateMonth rest MonthInvalid o ⇒ ValidateMonth (MonthShort     : rest) MonthUsed o
instance vm5  ∷ ValidateMonth rest MonthInvalid o ⇒ ValidateMonth (MonthTwoDigits : rest) MonthUsed o
instance vm6  ∷ ValidateMonth FNil t t
instance vm7  ∷ ValidateMonth rest i o ⇒ ValidateMonth (MillisecondsTwoDigits : rest) i o
instance vm8  ∷ ValidateMonth rest i o ⇒ ValidateMonth (MillisecondsShort     : rest) i o
instance vm9  ∷ ValidateMonth rest i o ⇒ ValidateMonth (Milliseconds          : rest) i o
instance vm10 ∷ ValidateMonth rest i o ⇒ ValidateMonth (YearFull              : rest) i o
instance vm11 ∷ ValidateMonth rest i o ⇒ ValidateMonth (YearTwoDigits         : rest) i o
instance vm12 ∷ ValidateMonth rest i o ⇒ ValidateMonth (YearAbsolute          : rest) i o
instance vm13 ∷ ValidateMonth rest i o ⇒ ValidateMonth (DayOfMonthTwoDigits   : rest) i o
instance vm14 ∷ ValidateMonth rest i o ⇒ ValidateMonth (DayOfMonth            : rest) i o
instance vm15 ∷ ValidateMonth rest i o ⇒ ValidateMonth (UnixTimestamp         : rest) i o
instance vm16 ∷ ValidateMonth rest i o ⇒ ValidateMonth (DayOfWeek             : rest) i o
instance vm17 ∷ ValidateMonth rest i o ⇒ ValidateMonth (Hours24               : rest) i o
instance vm18 ∷ ValidateMonth rest i o ⇒ ValidateMonth (Hours12               : rest) i o
instance vm19 ∷ ValidateMonth rest i o ⇒ ValidateMonth (Meridiem              : rest) i o
instance vm20 ∷ ValidateMonth rest i o ⇒ ValidateMonth (Minutes               : rest) i o
instance vm21 ∷ ValidateMonth rest i o ⇒ ValidateMonth (MinutesTwoDigits      : rest) i o
instance vm22 ∷ ValidateMonth rest i o ⇒ ValidateMonth (Seconds               : rest) i o
instance vm24 ∷ ValidateMonth rest i o ⇒ ValidateMonth (SecondsTwoDigits      : rest) i o
instance vm26 ∷ ValidateMonth rest i o ⇒ ValidateMonth (Placeholder a         : rest) i o

-- TODO add cases for DayOfWeek once we support actually support day of week
foreign import kind DayType
foreign import data NoDay ∷ DayType -- initial state
foreign import data DayUsed ∷ DayType -- one Day is present
foreign import data DayInvalid ∷ DayType -- more then one Day is present

class ValidDayType (ht ∷ DayType)
instance vdt0 ∷ ValidDayType DayUsed
instance vdt1 ∷ ValidDayType NoDay

class ValidateDay (fmt ∷ FormatList) (i ∷ DayType) (o ∷ DayType) | fmt i → o

instance vd0  ∷ ValidateDay rest DayUsed o    ⇒ ValidateDay (DayOfMonthTwoDigits : rest) NoDay o
instance vd1  ∷ ValidateDay rest DayUsed o    ⇒ ValidateDay (DayOfMonth : rest) NoDay o
instance vd2  ∷ ValidateDay rest DayInvalid o ⇒ ValidateDay (DayOfMonthTwoDigits : rest) DayUsed o
instance vd3  ∷ ValidateDay rest DayInvalid o ⇒ ValidateDay (DayOfMonth : rest) DayUsed o
instance vd4  ∷ ValidateDay FNil t t
instance vd5  ∷ ValidateDay rest i o              ⇒ ValidateDay (MillisecondsTwoDigits      : rest) i o
instance vd6  ∷ ValidateDay rest i o              ⇒ ValidateDay (MillisecondsShort          : rest) i o
instance vd7  ∷ ValidateDay rest i o              ⇒ ValidateDay (Milliseconds               : rest) i o
instance vd8  ∷ ValidateDay rest i o              ⇒ ValidateDay (YearFull                   : rest) i o
instance vd9  ∷ ValidateDay rest i o              ⇒ ValidateDay (YearTwoDigits              : rest) i o
instance vd10 ∷ ValidateDay rest i o              ⇒ ValidateDay (YearAbsolute               : rest) i o
instance vd11 ∷ ValidateDay rest i o              ⇒ ValidateDay (MonthFull                  : rest) i o
instance vd12 ∷ ValidateDay rest i o              ⇒ ValidateDay (MonthShort                 : rest) i o
instance vd13 ∷ ValidateDay rest i o              ⇒ ValidateDay (MonthTwoDigits             : rest) i o
instance vd14 ∷ ValidateDay rest i o              ⇒ ValidateDay (UnixTimestamp              : rest) i o
instance vd15 ∷ ValidateDay rest i o              ⇒ ValidateDay (DayOfWeek                  : rest) i o
instance vd16 ∷ ValidateDay rest i o              ⇒ ValidateDay (Hours24                    : rest) i o
instance vd17 ∷ ValidateDay rest i o              ⇒ ValidateDay (Hours12                    : rest) i o
instance vd18 ∷ ValidateDay rest i o              ⇒ ValidateDay (Meridiem                   : rest) i o
instance vd19 ∷ ValidateDay rest i o              ⇒ ValidateDay (Minutes                    : rest) i o
instance vd20 ∷ ValidateDay rest i o              ⇒ ValidateDay (MinutesTwoDigits           : rest) i o
instance vd21 ∷ ValidateDay rest i o              ⇒ ValidateDay (Seconds                    : rest) i o
instance vd22 ∷ ValidateDay rest i o              ⇒ ValidateDay (SecondsTwoDigits           : rest) i o
instance vd24 ∷ ValidateDay rest i o              ⇒ ValidateDay (Placeholder a              : rest) i o

foreign import kind EmptyType
foreign import data IsEmpty ∷ EmptyType
foreign import data NotEmpty ∷ EmptyType

class ValidEmptyType (ht ∷ EmptyType)
instance vnilt1 ∷ ValidEmptyType NotEmpty

class ValidateEmpty (fmt ∷ FormatList) (i ∷ EmptyType) (o ∷ EmptyType) | fmt i → o

instance vnil0  ∷ ValidateEmpty FNil t t
instance vnil1  ∷ ValidateEmpty (MillisecondsTwoDigits : rest) IsEmpty NotEmpty
instance vnil2  ∷ ValidateEmpty (MillisecondsShort     : rest) IsEmpty NotEmpty
instance vnil3  ∷ ValidateEmpty (Milliseconds          : rest) IsEmpty NotEmpty
instance vnil8  ∷ ValidateEmpty (YearFull              : rest) IsEmpty NotEmpty
instance vnil9  ∷ ValidateEmpty (YearTwoDigits         : rest) IsEmpty NotEmpty
instance vnil10 ∷ ValidateEmpty (YearAbsolute          : rest) IsEmpty NotEmpty
instance vnil11 ∷ ValidateEmpty (MonthFull             : rest) IsEmpty NotEmpty
instance vnil12 ∷ ValidateEmpty (MonthShort            : rest) IsEmpty NotEmpty
instance vnil13 ∷ ValidateEmpty (MonthTwoDigits        : rest) IsEmpty NotEmpty
instance vnil14 ∷ ValidateEmpty (DayOfMonthTwoDigits   : rest) IsEmpty NotEmpty
instance vnil15 ∷ ValidateEmpty (DayOfMonth            : rest) IsEmpty NotEmpty
instance vnil16 ∷ ValidateEmpty (UnixTimestamp         : rest) IsEmpty NotEmpty
instance vnil17 ∷ ValidateEmpty (DayOfWeek             : rest) IsEmpty NotEmpty
instance vnil18 ∷ ValidateEmpty (Hours24               : rest) IsEmpty NotEmpty
instance vnil19 ∷ ValidateEmpty (Hours12               : rest) IsEmpty NotEmpty
instance vnil20 ∷ ValidateEmpty (Meridiem              : rest) IsEmpty NotEmpty
instance vnil21 ∷ ValidateEmpty (Minutes               : rest) IsEmpty NotEmpty
instance vnil22 ∷ ValidateEmpty (MinutesTwoDigits      : rest) IsEmpty NotEmpty
instance vnil24 ∷ ValidateEmpty (Seconds               : rest) IsEmpty NotEmpty
instance vnil26 ∷ ValidateEmpty (SecondsTwoDigits      : rest) IsEmpty NotEmpty
instance vnil27 ∷ ValidateEmpty (a: rest) NotEmpty NotEmpty
instance vnil28 ∷ ValidateEmpty rest i o ⇒ ValidateEmpty (Placeholder a : rest) IsEmpty o





foreign import kind OnlyTimeType
foreign import data IsOnlyTime ∷ OnlyTimeType
foreign import data NotOnlyTime ∷ OnlyTimeType

class ValidOnlyTimeType (ht ∷ OnlyTimeType)
instance vtot1 ∷ ValidOnlyTimeType IsOnlyTime

class ValidateOnlyTime (fmt ∷ FormatList) (i ∷ OnlyTimeType) (o ∷ OnlyTimeType) | fmt i → o

instance vto0  ∷ ValidateOnlyTime FNil t t
instance vto8  ∷ ValidateOnlyTime (YearFull              : rest) i NotOnlyTime
instance vto9  ∷ ValidateOnlyTime (YearTwoDigits         : rest) i NotOnlyTime
instance vto10 ∷ ValidateOnlyTime (YearAbsolute          : rest) i NotOnlyTime
instance vto11 ∷ ValidateOnlyTime (MonthFull             : rest) i NotOnlyTime
instance vto12 ∷ ValidateOnlyTime (MonthShort            : rest) i NotOnlyTime
instance vto13 ∷ ValidateOnlyTime (MonthTwoDigits        : rest) i NotOnlyTime
instance vto14 ∷ ValidateOnlyTime (DayOfMonthTwoDigits   : rest) i NotOnlyTime
instance vto15 ∷ ValidateOnlyTime (DayOfMonth            : rest) i NotOnlyTime
instance vto16 ∷ ValidateOnlyTime (UnixTimestamp         : rest) i NotOnlyTime
instance vto17 ∷ ValidateOnlyTime (DayOfWeek             : rest) i NotOnlyTime
instance vto18 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (Hours24               : rest) i o
instance vto19 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (Hours12               : rest) i o
instance vto20 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (Meridiem              : rest) i o
instance vto21 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (Minutes               : rest) i o
instance vto22 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (MinutesTwoDigits      : rest) i o
instance vto24 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (Seconds               : rest) i o
instance vto26 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (SecondsTwoDigits      : rest) i o
instance vto1  ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (MillisecondsTwoDigits : rest) i o
instance vto2  ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (MillisecondsShort     : rest) i o
instance vto3  ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (Milliseconds          : rest) i o
instance vto28 ∷ ValidateOnlyTime rest i o ⇒ ValidateOnlyTime (Placeholder a         : rest) i o






foreign import kind OnlyDateType
foreign import data IsOnlyDate ∷ OnlyDateType
foreign import data NotOnlyDate ∷ OnlyDateType

class ValidOnlyDateType (ht ∷ OnlyDateType)
instance vdot1 ∷ ValidOnlyDateType IsOnlyDate

class ValidateOnlyDate (fmt ∷ FormatList) (i ∷ OnlyDateType) (o ∷ OnlyDateType) | fmt i → o

instance vdo0  ∷ ValidateOnlyDate FNil t t
instance vdo18 ∷ ValidateOnlyDate (Hours24               : rest) i NotOnlyDate
instance vdo19 ∷ ValidateOnlyDate (Hours12               : rest) i NotOnlyDate
instance vdo20 ∷ ValidateOnlyDate (Meridiem              : rest) i NotOnlyDate
instance vdo21 ∷ ValidateOnlyDate (Minutes               : rest) i NotOnlyDate
instance vdo22 ∷ ValidateOnlyDate (MinutesTwoDigits      : rest) i NotOnlyDate
instance vdo24 ∷ ValidateOnlyDate (Seconds               : rest) i NotOnlyDate
instance vdo26 ∷ ValidateOnlyDate (SecondsTwoDigits      : rest) i NotOnlyDate
instance vdo1  ∷ ValidateOnlyDate (MillisecondsTwoDigits : rest) i NotOnlyDate
instance vdo2  ∷ ValidateOnlyDate (MillisecondsShort     : rest) i NotOnlyDate
instance vdo3  ∷ ValidateOnlyDate (UnixTimestamp         : rest) i NotOnlyDate
instance vdo16 ∷ ValidateOnlyDate (Milliseconds          : rest) i NotOnlyDate
instance vdo8  ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (YearFull              : rest) i o
instance vdo9  ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (YearTwoDigits         : rest) i o
instance vdo10 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (YearAbsolute          : rest) i o
instance vdo11 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (MonthFull             : rest) i o
instance vdo12 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (MonthShort            : rest) i o
instance vdo13 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (MonthTwoDigits        : rest) i o
instance vdo14 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (DayOfMonthTwoDigits   : rest) i o
instance vdo15 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (DayOfMonth            : rest) i o
instance vdo17 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (DayOfWeek             : rest) i o
instance vdo28 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (Placeholder a         : rest) i o

foreign import kind UnixType
foreign import data CanUseUnix ∷ UnixType -- UnixTimestamp can be used
foreign import data CanNotUseUnix ∷ UnixType -- some date or time command was used
foreign import data UnixUsed ∷ UnixType -- UnixTimestamp was used
foreign import data InvalidUnixUse ∷ UnixType -- UnixTimestamp was used even tho it can't be used

class ValidUnixType (ht ∷ UnixType)
instance vut0 ∷ ValidUnixType CanUseUnix
instance vut1 ∷ ValidUnixType CanNotUseUnix
instance vut2 ∷ ValidUnixType UnixUsed

class ValidateUnix (fmt ∷ FormatList) (i ∷ UnixType) (o ∷ UnixType) | fmt i → o

instance vu0  ∷ ValidateUnix rest UnixUsed o ⇒ ValidateUnix (UnixTimestamp : rest) CanUseUnix o
instance vu1  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (UnixTimestamp : rest) CanNotUseUnix o

instance vu2  ∷ ValidateUnix FNil t t
instance vu3  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MillisecondsTwoDigits : rest) CanUseUnix o
instance vu4  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MillisecondsShort     : rest) CanUseUnix o
instance vu5  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Milliseconds          : rest) CanUseUnix o
instance vu6  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (YearFull              : rest) CanUseUnix o
instance vu7  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (YearTwoDigits         : rest) CanUseUnix o
instance vu8  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (YearAbsolute          : rest) CanUseUnix o
instance vu9  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MonthFull             : rest) CanUseUnix o
instance vu10 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MonthShort            : rest) CanUseUnix o
instance vu11 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MonthTwoDigits        : rest) CanUseUnix o
instance vu12 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (DayOfMonthTwoDigits   : rest) CanUseUnix o
instance vu13 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (DayOfMonth            : rest) CanUseUnix o
instance vu15 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (DayOfWeek             : rest) CanUseUnix o
instance vu16 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Hours24               : rest) CanUseUnix o
instance vu17 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Hours12               : rest) CanUseUnix o
instance vu18 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Meridiem              : rest) CanUseUnix o
instance vu19 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Minutes               : rest) CanUseUnix o
instance vu20 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MinutesTwoDigits      : rest) CanUseUnix o
instance vu21 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Seconds               : rest) CanUseUnix o
instance vu22 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (SecondsTwoDigits      : rest) CanUseUnix o

instance vu33  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MillisecondsTwoDigits : rest) CanNotUseUnix o
instance vu34  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MillisecondsShort     : rest) CanNotUseUnix o
instance vu35  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Milliseconds          : rest) CanNotUseUnix o
instance vu36  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (YearFull              : rest) CanNotUseUnix o
instance vu37  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (YearTwoDigits         : rest) CanNotUseUnix o
instance vu38  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (YearAbsolute          : rest) CanNotUseUnix o
instance vu39  ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MonthFull             : rest) CanNotUseUnix o
instance vu310 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MonthShort            : rest) CanNotUseUnix o
instance vu311 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MonthTwoDigits        : rest) CanNotUseUnix o
instance vu312 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (DayOfMonthTwoDigits   : rest) CanNotUseUnix o
instance vu313 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (DayOfMonth            : rest) CanNotUseUnix o
instance vu315 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (DayOfWeek             : rest) CanNotUseUnix o
instance vu316 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Hours24               : rest) CanNotUseUnix o
instance vu317 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Hours12               : rest) CanNotUseUnix o
instance vu318 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Meridiem              : rest) CanNotUseUnix o
instance vu319 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Minutes               : rest) CanNotUseUnix o
instance vu320 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (MinutesTwoDigits      : rest) CanNotUseUnix o
instance vu321 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (Seconds               : rest) CanNotUseUnix o
instance vu322 ∷ ValidateUnix rest CanNotUseUnix o ⇒ ValidateUnix (SecondsTwoDigits      : rest) CanNotUseUnix o

instance vu433  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MillisecondsTwoDigits : rest) UnixUsed o
instance vu434  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MillisecondsShort     : rest) UnixUsed o
instance vu435  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Milliseconds          : rest) UnixUsed o
instance vu436  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (YearFull              : rest) UnixUsed o
instance vu437  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (YearTwoDigits         : rest) UnixUsed o
instance vu438  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (YearAbsolute          : rest) UnixUsed o
instance vu439  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MonthFull             : rest) UnixUsed o
instance vu4310 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MonthShort            : rest) UnixUsed o
instance vu4311 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MonthTwoDigits        : rest) UnixUsed o
instance vu4312 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (DayOfMonthTwoDigits   : rest) UnixUsed o
instance vu4313 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (DayOfMonth            : rest) UnixUsed o
instance vu4315 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (DayOfWeek             : rest) UnixUsed o
instance vu4316 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Hours24               : rest) UnixUsed o
instance vu4317 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Hours12               : rest) UnixUsed o
instance vu4318 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Meridiem              : rest) UnixUsed o
instance vu4319 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Minutes               : rest) UnixUsed o
instance vu4320 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MinutesTwoDigits      : rest) UnixUsed o
instance vu4321 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Seconds               : rest) UnixUsed o
instance vu4322 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (SecondsTwoDigits      : rest) UnixUsed o

instance vu5433  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MillisecondsTwoDigits : rest) InvalidUnixUse o
instance vu5434  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MillisecondsShort     : rest) InvalidUnixUse o
instance vu5435  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Milliseconds          : rest) InvalidUnixUse o
instance vu5436  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (YearFull              : rest) InvalidUnixUse o
instance vu5437  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (YearTwoDigits         : rest) InvalidUnixUse o
instance vu5438  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (YearAbsolute          : rest) InvalidUnixUse o
instance vu5439  ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MonthFull             : rest) InvalidUnixUse o
instance vu54310 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MonthShort            : rest) InvalidUnixUse o
instance vu54311 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MonthTwoDigits        : rest) InvalidUnixUse o
instance vu54312 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (DayOfMonthTwoDigits   : rest) InvalidUnixUse o
instance vu54313 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (DayOfMonth            : rest) InvalidUnixUse o
instance vu54315 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (DayOfWeek             : rest) InvalidUnixUse o
instance vu54316 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Hours24               : rest) InvalidUnixUse o
instance vu54317 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Hours12               : rest) InvalidUnixUse o
instance vu54318 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Meridiem              : rest) InvalidUnixUse o
instance vu54319 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Minutes               : rest) InvalidUnixUse o
instance vu54320 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (MinutesTwoDigits      : rest) InvalidUnixUse o
instance vu54321 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (Seconds               : rest) InvalidUnixUse o
instance vu54322 ∷ ValidateUnix rest InvalidUnixUse o ⇒ ValidateUnix (SecondsTwoDigits      : rest) InvalidUnixUse o

instance vu23 ∷ ValidateUnix rest i o             ⇒ ValidateUnix (Placeholder a         : rest) i o



class ValidTime (a ∷ FormatList)
instance vt ∷
  ( ValidTimePart a
  , ValidateOnlyTime a IsOnlyTime onlyTimeOut, ValidOnlyTimeType onlyTimeOut
  , ValidateEmpty a IsEmpty emptyOut, ValidEmptyType emptyOut
  ) ⇒ ValidTime a

class ValidTimePart (a ∷ FormatList)
instance vti ∷
  ( ValidateHours a NoHours hoursOut, ValidHoursType hoursOut
  , ValidateMinutes a NoMinutes minutesOut, ValidMinutesType minutesOut
  , ValidateSeconds a NoSeconds secondsOut, ValidSecondsType secondsOut
  , ValidateMilliseconds a NoMilliseconds millisecondsOut, ValidMillisecondsType millisecondsOut
  ) ⇒ ValidTimePart a




class ValidDate (a ∷ FormatList)
instance vd ∷
  ( ValidDatePart a
  , ValidateOnlyDate a IsOnlyDate onlyDateout, ValidOnlyDateType onlyDateout
  , ValidateEmpty a IsEmpty emptyOut, ValidEmptyType emptyOut
  ) ⇒ ValidDate a

class ValidDatePart (a ∷ FormatList)
instance vdi ∷
  ( ValidateYear a NoYear yearOut, ValidYearType yearOut
  , ValidateMonth a NoMonth monthOut, ValidMonthType monthOut
  , ValidateDay a NoDay dayOut, ValidDayType dayOut
  ) ⇒ ValidDatePart a




class ValidDateTime (a ∷ FormatList)
instance vdt ∷
  ( ValidTimePart a
  , ValidDatePart a
  , ValidateUnix a CanUseUnix unixOut, ValidUnixType unixOut
  , ValidateEmpty a IsEmpty emptyOut, ValidEmptyType emptyOut
  ) ⇒ ValidDateTime a




class PrintFormatI (fmt ∷ FormatList) where
  printFormatI ∷ FProxy fmt → List FC.FormatterCommand → List FC.FormatterCommand

instance rdi0 ∷ PrintFormatI FNil where
  printFormatI _ acc = acc

instance rdi1 ∷ (IsSymbol s, PrintFormatI rest) ⇒ PrintFormatI (Placeholder s : rest) where
  printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< (Cons $ FC.Placeholder $ reflectSymbol (SProxy ∷ SProxy s))

instance rdi2  ∷ PrintFormatI rest ⇒ PrintFormatI (YearFull              : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.YearFull
instance rdi3  ∷ PrintFormatI rest ⇒ PrintFormatI (YearTwoDigits         : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.YearTwoDigits
instance rdi4  ∷ PrintFormatI rest ⇒ PrintFormatI (YearAbsolute          : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.YearAbsolute
instance rdi5  ∷ PrintFormatI rest ⇒ PrintFormatI (MonthFull             : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.MonthFull
instance rdi6  ∷ PrintFormatI rest ⇒ PrintFormatI (MonthShort            : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.MonthShort
instance rdi7  ∷ PrintFormatI rest ⇒ PrintFormatI (MonthTwoDigits        : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.MonthTwoDigits
instance rdi8  ∷ PrintFormatI rest ⇒ PrintFormatI (DayOfMonthTwoDigits   : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.DayOfMonthTwoDigits
instance rdi9  ∷ PrintFormatI rest ⇒ PrintFormatI (DayOfMonth            : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.DayOfMonth
instance rdi10 ∷ PrintFormatI rest ⇒ PrintFormatI (UnixTimestamp         : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.UnixTimestamp
instance rdi11 ∷ PrintFormatI rest ⇒ PrintFormatI (DayOfWeek             : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.DayOfWeek
instance rdi12 ∷ PrintFormatI rest ⇒ PrintFormatI (Hours24               : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.Hours24
instance rdi13 ∷ PrintFormatI rest ⇒ PrintFormatI (Hours12               : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.Hours12
instance rdi14 ∷ PrintFormatI rest ⇒ PrintFormatI (Meridiem              : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.Meridiem
instance rdi15 ∷ PrintFormatI rest ⇒ PrintFormatI (Minutes               : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.Minutes
instance rdi16 ∷ PrintFormatI rest ⇒ PrintFormatI (MinutesTwoDigits      : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.MinutesTwoDigits
instance rdi17 ∷ PrintFormatI rest ⇒ PrintFormatI (Seconds               : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.Seconds
instance rdi18 ∷ PrintFormatI rest ⇒ PrintFormatI (SecondsTwoDigits      : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.SecondsTwoDigits
instance rdi19 ∷ PrintFormatI rest ⇒ PrintFormatI (Milliseconds          : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.Milliseconds
instance rdi20 ∷ PrintFormatI rest ⇒ PrintFormatI (MillisecondsShort     : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.MillisecondsShort
instance rdi21 ∷ PrintFormatI rest ⇒ PrintFormatI (MillisecondsTwoDigits : rest) where printFormatI _ = printFormatI (FProxy ∷ FProxy rest) <<< Cons FC.MillisecondsTwoDigits
