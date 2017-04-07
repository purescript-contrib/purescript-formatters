module Test.Main where

import Prelude
import Control.Monad.Aff.Console as AffC
import Data.Date as D
import Data.DateTime as DTi
import Data.Formatter.DateTime as FDT
import Data.Formatter.Number as FN
import Data.Time as T
import Debug.Trace as DT
import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, put, get, execStateT)
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Enum (toEnum)
import Data.Functor.Mu (roll)
import Data.Maybe (fromMaybe)

type Tests e a = StateT Boolean (Aff (exception :: EXCEPTION, console :: CONSOLE | e)) a


execTests :: forall a e c.
  StateT a (Aff ( process :: PROCESS | e)) c ->
  a ->
  Eff (process :: PROCESS | e) (Canceler ( process :: PROCESS | e ))
execTests fn state = runAff (\s -> exit 1) (\s -> exit 0) (execStateT fn state)


log :: forall e. String -> Tests e Unit
log message = liftAff $ AffC.log message


foreign import data PROCESS :: Effect
foreign import exit :: Int -> forall e. Eff (process :: PROCESS | e) Unit


fnOne ∷ FN.Formatter
fnOne =
  { comma: false
  , before: 3
  , after: 2
  , abbreviations: false
  , sign: false
  }


fnTwo ∷ FN.Formatter
fnTwo =
  { comma: true
  , before: one
  , after: 4
  , abbreviations: false
  , sign: true
  }


fnThree ∷ FN.Formatter
fnThree =
 { comma: false
 , before: 2
 , after: 2
 , abbreviations: true
 , sign: true
 }


fdtOne ∷ FDT.Formatter
fdtOne =
  roll $ FDT.Placeholder "format string is "
  $ roll $ FDT.YearFull
  $ roll $ FDT.Placeholder "-"
  $ roll $ FDT.MonthShort
  $ roll FDT.End


numeralTests :: forall e. Tests e Unit
numeralTests = do
  log $ "\nNUMERAL TESTS\n"

  log $ "\nPRINT FORMATTER"
  log $ FN.printFormatter fnOne
  log $ FN.printFormatter fnTwo
  log $ FN.printFormatter fnThree

  log $ "\nPARSE FORMAT STRING"
  DT.traceAnyA $ FN.parseFormatString "000,0.00"
  DT.traceAnyA $ FN.parseFormatString "000"
  DT.traceAnyA $ FN.parseFormatString "0a"
  DT.traceAnyA $ FN.parseFormatString "-0,0.000"
  DT.traceAnyA $ FN.parseFormatString "+000.0"

  log $ "\n FORMAT"
  log $ FN.format fnOne 100.2
  log $ FN.format fnTwo 100.1
  log $ FN.format fnThree 100.3
  log $ FN.format fnThree 10004000.0

  log $ "\n UNFORMAT"
  DT.traceAnyA $ FN.unformat fnOne "001.12"
  DT.traceAnyA $ FN.unformat fnOne "-123.12"
  DT.traceAnyA $ FN.unformat fnOne "12.12"
  DT.traceAnyA $ FN.unformat fnThree "+123"
  DT.traceAnyA $ FN.unformat fnTwo "-100,000.1234"

  log $ "\n FORMAT NUMBER"
  DT.traceAnyA $ FN.formatNumber "00.00" 12.0
  DT.traceAnyA $ FN.formatNumber "00000,0.000" 123345.1235
  DT.traceAnyA $ FN.formatNumber "0.0" 123345.1235
  DT.traceAnyA $ FN.formatNumber "0.0" (-123345.1235)

  log $ "\n UNFORMAT NUMBER"
  DT.traceAnyA $ FN.unformatNumber "0.00" "12.00"


-- April 12th 2017 at 11:34:34:234
-- 4/12/2017
makeDateTime ∷ Int -> Int -> Int -> DTi.DateTime
makeDateTime year month day =
  DTi.DateTime
    (D.canonicalDate (fromMaybe bottom $ toEnum year) (fromMaybe bottom $ toEnum month) (fromMaybe bottom $ toEnum day))
    (T.Time
       (fromMaybe bottom $ toEnum 11)
       (fromMaybe bottom $ toEnum 34)
       (fromMaybe bottom $ toEnum 34)
       (fromMaybe bottom $ toEnum 234))

testDateTime :: DTi.DateTime
testDateTime = makeDateTime 2017 4 12


assert :: forall e. String -> String -> Boolean -> Tests e Unit
assert _           success true = log $ "  ✓ - Passed - " <> success
assert fail _              false = do
  log $ "  ☠ - Failed because " <> fail
  put false


failTest :: forall e. String -> Tests e Unit
failTest message = do
  log message
  put false


assertFormatting :: forall e. String -> String -> DateTime -> Tests e Unit
assertFormatting target' format dateTime = do
  let result = FDT.formatDateTime format dateTime
  let target = Right target'
  assert
    ((show result) <> " does not equal " <> (show target))
    ((show result) <> " equals " <> (show target))
    (result == target)


timeTest :: forall e. Tests e Unit
timeTest = do
  log "- Data.Formatter.DateTime.formatDateTime"

  -- var a = moment(
  --   'April 12th 2017 at 11:34:34:234',
  --   'MMMM Do YYYY [at] HH:mm:ss:SSS'
  -- );
  -- a.format('MMMM Do YYYY [at] HH:mm:ss:SSS')
  -- testDateTime = April 12th 2017 at 11:34:34:234
  assertFormatting "04/12/2017"      "MM/DD/YYYY" testDateTime
  assertFormatting "April"           "MMMM" testDateTime
  assertFormatting "2017-12-04"      "YYYY-DD-MM" testDateTime
  assertFormatting "2017-Apr"        "YYYY-MMM" testDateTime
  assertFormatting "Apr 1"           "MMM D" (makeDateTime 2017 4 1)

  -- This should probably be am (lowercase), if the desired
  -- functionality of the library is to mirror momentjs
  assertFormatting "11:34:34:234 AM" "hh:mm:ss:SSS a"  testDateTime
  assertFormatting "17"            "YY"  testDateTime
  log "  --- Format 20017 with YY"
  assertFormatting "17"            "YY"  (makeDateTime 20017 4 12)
  log "  --- Format 0 with YY"
  assertFormatting "00"            "YY"  (makeDateTime 0 4 12)
  log "  --- Format -1 with YY"
  assertFormatting "01"            "YY"  (makeDateTime (-1) 4 12)

  log "- Data.Formatter.DateTime.unformatDateTime "

  let dt = FDT.unformatDateTime "YYYY-DD-MM SSS" "2017-12-04 234"
  either
    (const $ failTest "Could not parse 017-12-04 234")
    (assertFormatting "2017-12-04 234" "YYYY-DD-MM SSS")
    dt

formattingTests :: forall e. Tests e Unit
formattingTests = do
  log $ "\nPARSE FORMAT STRING"

  DT.traceAnyA $ FDT.parseFormatString "YYYY-MM-DD"
  DT.traceAnyA $ FDT.parseFormatString "YY-Q-dddd HH:mm Z"

  log $ "\nFORMAT"
  DT.traceAnyA $ FDT.parseFormatString "YYYY-MM-DD trololo Q" <#> flip FDT.format testDateTime

  log $ "\nUNFORMAT"
  case FDT.parseFormatString "DD-MM-YYYY HH-:-mm" of
    Left _ → DT.traceAnyA "?"
    Right f → DT.traceAnyA $ FDT.unformat f "12-10-1345 04-:-32"

  log $ "\nUNFORMATDATETIME"
  DT.traceAnyA $ FDT.unformatDateTime "YYYY-DD-MM SSS" "3456-09-10 333"

main :: forall e.
  Eff ( process :: PROCESS, exception :: EXCEPTION, console :: CONSOLE | e)
    (Canceler ( process :: PROCESS, exception :: EXCEPTION, console :: CONSOLE | e))
main = execTests tests true
  where
  tests = do
    log "Testing time functions..."
    timeTest
    passed <- get
    when (passed /= true) (throwError (error "Tests did not pass."))
    --numeralTests
    --formattingTests
