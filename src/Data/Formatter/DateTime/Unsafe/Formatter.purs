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

instance vh0  ∷ ValidateHours FNil t t
instance vh1  ∷ ValidateHours rest HoursType24 o  ⇒ ValidateHours (Hours24                : rest) NoHours o
instance vh2  ∷ ValidateHours rest Hours12Only o  ⇒ ValidateHours (Hours12                : rest) NoHours o
instance vh3  ∷ ValidateHours rest MeridiemOnly o ⇒ ValidateHours (Meridiem               : rest) NoHours o
instance vh4  ∷ ValidateHours rest HoursType12 o  ⇒ ValidateHours (Meridiem               : rest) Hours12Only o
instance vh5  ∷ ValidateHours rest HoursType12 o  ⇒ ValidateHours (Hours12                : rest) MeridiemOnly o
instance vh6  ∷ ValidateHours rest i o            ⇒ ValidateHours (YearFull               : rest) i o
instance vh7  ∷ ValidateHours rest i o            ⇒ ValidateHours (YearTwoDigits          : rest) i o
instance vh8  ∷ ValidateHours rest i o            ⇒ ValidateHours (YearAbsolute           : rest) i o
instance vh9  ∷ ValidateHours rest i o            ⇒ ValidateHours (MonthFull              : rest) i o
instance vh10 ∷ ValidateHours rest i o            ⇒ ValidateHours (MonthShort             : rest) i o
instance vh11 ∷ ValidateHours rest i o            ⇒ ValidateHours (MonthTwoDigits         : rest) i o
instance vh12 ∷ ValidateHours rest i o            ⇒ ValidateHours (DayOfMonthTwoDigits    : rest) i o
instance vh13 ∷ ValidateHours rest i o            ⇒ ValidateHours (DayOfMonth             : rest) i o
instance vh14 ∷ ValidateHours rest i o            ⇒ ValidateHours (UnixTimestamp          : rest) i o
instance vh15 ∷ ValidateHours rest i o            ⇒ ValidateHours (DayOfWeek              : rest) i o
instance vh16 ∷ ValidateHours rest i o            ⇒ ValidateHours (Minutes                : rest) i o
instance vh17 ∷ ValidateHours rest i o            ⇒ ValidateHours (MinutesTwoDigits       : rest) i o
instance vh18 ∷ ValidateHours rest i o            ⇒ ValidateHours (Seconds                : rest) i o
instance vh19 ∷ ValidateHours rest i o            ⇒ ValidateHours (SecondsTwoDigits       : rest) i o
instance vh20 ∷ ValidateHours rest i o            ⇒ ValidateHours (Milliseconds           : rest) i o
instance vh21 ∷ ValidateHours rest i o            ⇒ ValidateHours (MillisecondsShort      : rest) i o
instance vh22 ∷ ValidateHours rest i o            ⇒ ValidateHours (MillisecondsTwoDigits  : rest) i o
instance vh23 ∷ ValidateHours rest i o            ⇒ ValidateHours (Placeholder s          : rest) i o





foreign import kind MinutesType
foreign import data NoMinutes ∷ MinutesType -- initial state
foreign import data MinutesUsed ∷ MinutesType -- one minute is present
foreign import data MinutesInvalid ∷ MinutesType -- more then one minute is present

class ValidMinutesType (ht ∷ MinutesType)
instance vmt1 ∷ ValidMinutesType MinutesUsed
instance vmt3 ∷ ValidMinutesType NoMinutes

class ValidateMinutes (fmt ∷ FormatList) (i ∷ MinutesType) (o ∷ MinutesType) | fmt i → o

instance vm0  ∷ ValidateMinutes FNil t t
instance vm1  ∷ ValidateMinutes rest MinutesUsed o    ⇒ ValidateMinutes (MinutesTwoDigits      : rest) NoMinutes o
instance vm2  ∷ ValidateMinutes rest MinutesUsed o    ⇒ ValidateMinutes (Minutes               : rest) NoMinutes o
instance vm3  ∷ ValidateMinutes rest MinutesInvalid o ⇒ ValidateMinutes (MinutesTwoDigits      : rest) MinutesUsed o
instance vm4  ∷ ValidateMinutes rest MinutesInvalid o ⇒ ValidateMinutes (Minutes               : rest) MinutesUsed o
instance vm6  ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (YearFull              : rest) i o
instance vm7  ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (YearTwoDigits         : rest) i o
instance vm8  ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (YearAbsolute          : rest) i o
instance vm9  ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (MonthFull             : rest) i o
instance vm10 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (MonthShort            : rest) i o
instance vm11 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (MonthTwoDigits        : rest) i o
instance vm12 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (DayOfMonthTwoDigits   : rest) i o
instance vm13 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (DayOfMonth            : rest) i o
instance vm14 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (UnixTimestamp         : rest) i o
instance vm15 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (DayOfWeek             : rest) i o
instance vm16 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (Hours24               : rest) i o
instance vm17 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (Hours12               : rest) i o
instance vm18 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (Meridiem              : rest) i o
instance vm19 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (Seconds               : rest) i o
instance vm20 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (SecondsTwoDigits      : rest) i o
instance vm21 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (Milliseconds          : rest) i o
instance vm22 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (MillisecondsShort     : rest) i o
instance vm23 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (MillisecondsTwoDigits : rest) i o
instance vm24 ∷ ValidateMinutes rest i o              ⇒ ValidateMinutes (Placeholder a         : rest) i o





foreign import kind SecondsType
foreign import data NoSeconds ∷ SecondsType -- initial state
foreign import data SecondsUsed ∷ SecondsType -- one second is present
foreign import data SecondsInvalid ∷ SecondsType -- more then one second is present

class ValidSecondsType (ht ∷ SecondsType)
instance vst1 ∷ ValidSecondsType SecondsUsed
instance vst3 ∷ ValidSecondsType NoSeconds

class ValidateSeconds (fmt ∷ FormatList) (i ∷ SecondsType) (o ∷ SecondsType) | fmt i → o

instance vs0  ∷ ValidateSeconds FNil t t
instance vs1  ∷ ValidateSeconds rest SecondsUsed o    ⇒ ValidateSeconds (SecondsTwoDigits      : rest) NoSeconds o
instance vs2  ∷ ValidateSeconds rest SecondsUsed o    ⇒ ValidateSeconds (Seconds               : rest) NoSeconds o
instance vs3  ∷ ValidateSeconds rest SecondsInvalid o ⇒ ValidateSeconds (SecondsTwoDigits      : rest) SecondsUsed o
instance vs4  ∷ ValidateSeconds rest SecondsInvalid o ⇒ ValidateSeconds (Seconds               : rest) SecondsUsed o
instance vs6  ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (YearFull              : rest) i o
instance vs7  ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (YearTwoDigits         : rest) i o
instance vs8  ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (YearAbsolute          : rest) i o
instance vs9  ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (MonthFull             : rest) i o
instance vs10 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (MonthShort            : rest) i o
instance vs11 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (MonthTwoDigits        : rest) i o
instance vs12 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (DayOfMonthTwoDigits   : rest) i o
instance vs13 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (DayOfMonth            : rest) i o
instance vs14 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (UnixTimestamp         : rest) i o
instance vs15 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (DayOfWeek             : rest) i o
instance vs16 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (Hours24               : rest) i o
instance vs17 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (Hours12               : rest) i o
instance vs18 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (Meridiem              : rest) i o
instance vs19 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (Minutes               : rest) i o
instance vs20 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (MinutesTwoDigits      : rest) i o
instance vs21 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (Milliseconds          : rest) i o
instance vs22 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (MillisecondsShort     : rest) i o
instance vs23 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (MillisecondsTwoDigits : rest) i o
instance vs24 ∷ ValidateSeconds rest i o              ⇒ ValidateSeconds (Placeholder a         : rest) i o







foreign import kind MillisecondsType
foreign import data NoMilliseconds ∷ MillisecondsType -- initial state
foreign import data MillisecondsUsed ∷ MillisecondsType -- one millisecond is present
foreign import data MillisecondsInvalid ∷ MillisecondsType -- more then one millisecond is present

class ValidMillisecondsType (ht ∷ MillisecondsType)
instance vmst1 ∷ ValidMillisecondsType MillisecondsUsed
instance vmst3 ∷ ValidMillisecondsType NoMilliseconds

class ValidateMilliseconds (fmt ∷ FormatList) (i ∷ MillisecondsType) (o ∷ MillisecondsType) | fmt i → o

instance vms0  ∷ ValidateMilliseconds FNil t t
instance vms1  ∷ ValidateMilliseconds rest MillisecondsUsed o    ⇒ ValidateMilliseconds (MillisecondsTwoDigits : rest) NoMilliseconds o
instance vms2  ∷ ValidateMilliseconds rest MillisecondsUsed o    ⇒ ValidateMilliseconds (MillisecondsShort     : rest) NoMilliseconds o
instance vms3  ∷ ValidateMilliseconds rest MillisecondsUsed o    ⇒ ValidateMilliseconds (Milliseconds          : rest) NoMilliseconds o
instance vms4  ∷ ValidateMilliseconds rest MillisecondsInvalid o ⇒ ValidateMilliseconds (MillisecondsTwoDigits : rest) MillisecondsUsed o
instance vms6  ∷ ValidateMilliseconds rest MillisecondsInvalid o ⇒ ValidateMilliseconds (MillisecondsShort     : rest) MillisecondsUsed o
instance vms7  ∷ ValidateMilliseconds rest MillisecondsInvalid o ⇒ ValidateMilliseconds (Milliseconds          : rest) MillisecondsUsed o
instance vms8  ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (YearFull                   : rest) i o
instance vms9  ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (YearTwoDigits              : rest) i o
instance vms10 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (YearAbsolute               : rest) i o
instance vms11 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (MonthFull                  : rest) i o
instance vms12 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (MonthShort                 : rest) i o
instance vms13 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (MonthTwoDigits             : rest) i o
instance vms14 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (DayOfMonthTwoDigits        : rest) i o
instance vms15 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (DayOfMonth                 : rest) i o
instance vms16 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (UnixTimestamp              : rest) i o
instance vms17 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (DayOfWeek                  : rest) i o
instance vms18 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (Hours24                    : rest) i o
instance vms19 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (Hours12                    : rest) i o
instance vms20 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (Meridiem                   : rest) i o
instance vms21 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (Minutes                    : rest) i o
instance vms22 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (MinutesTwoDigits           : rest) i o
instance vms24 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (Seconds                    : rest) i o
instance vms25 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (SecondsTwoDigits           : rest) i o
instance vms26 ∷ ValidateMilliseconds rest i o              ⇒ ValidateMilliseconds (Placeholder a              : rest) i o



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
instance vdo3  ∷ ValidateOnlyDate (Milliseconds          : rest) i NotOnlyDate
instance vdo8  ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (YearFull              : rest) i o
instance vdo9  ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (YearTwoDigits         : rest) i o
instance vdo10 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (YearAbsolute          : rest) i o
instance vdo11 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (MonthFull             : rest) i o
instance vdo12 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (MonthShort            : rest) i o
instance vdo13 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (MonthTwoDigits        : rest) i o
instance vdo14 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (DayOfMonthTwoDigits   : rest) i o
instance vdo15 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (DayOfMonth            : rest) i o
instance vdo16 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (UnixTimestamp         : rest) i o
instance vdo17 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (DayOfWeek             : rest) i o
instance vdo28 ∷ ValidateOnlyDate rest i o ⇒ ValidateOnlyDate (Placeholder a         : rest) i o




class ValidTime (a ∷ FormatList)
instance vt ∷
  ( ValidTimePart a
  , ValidateOnlyTime a IsOnlyTime onlyTimeout
  , ValidOnlyTimeType onlyTimeout
  , ValidateEmpty a IsEmpty emptyOut
  , ValidEmptyType emptyOut
  ) ⇒ ValidTime a

class ValidTimePart (a ∷ FormatList)
instance vti ∷
  ( ValidateHours a NoHours hoursOut
  , ValidHoursType hoursOut
  , ValidateMinutes a NoMinutes minutesOut
  , ValidMinutesType minutesOut
  , ValidateSeconds a NoSeconds secondsOut
  , ValidSecondsType secondsOut
  , ValidateMilliseconds a NoMilliseconds millisecondsOut
  , ValidMillisecondsType millisecondsOut
  ) ⇒ ValidTimePart a




class ValidDate (a ∷ FormatList)
instance vd ∷
  ( ValidDatePart a
  , ValidateOnlyDate a IsOnlyDate onlyDateout
  , ValidOnlyDateType onlyDateout
  , ValidateEmpty a IsEmpty emptyOut
  , ValidEmptyType emptyOut
  ) ⇒ ValidDate a

class ValidDatePart (a ∷ FormatList)
instance vdi ∷ ValidDatePart a




class ValidDateTime (a ∷ FormatList)
instance vdt ∷
  ( ValidTimePart a
  , ValidDatePart a
  , ValidateEmpty a IsEmpty emptyOut
  , ValidEmptyType emptyOut
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
