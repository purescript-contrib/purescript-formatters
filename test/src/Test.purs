module Test.Test where

import Prelude
import Control.Monad.Aff.Console as AffC
import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, get, put, execStateT)



-- TODO switch to https://github.com/owickstrom/purescript-spec

foreign import data PROCESS :: Effect
foreign import exit :: Int -> forall e. Eff (process :: PROCESS | e) Unit


type Tests e a = StateT Boolean (Aff (exception :: EXCEPTION, console :: CONSOLE | e)) a


exec :: forall e c.
  StateT Boolean (Aff ( process :: PROCESS | e)) c ->
  Eff (process :: PROCESS | e) (Canceler ( process :: PROCESS | e ))
exec t = flip execTests true $ do
  void t
  passed <- get
  when (passed /= true) (throwError (error "Tests did not pass."))


execTests :: forall a e c.
  StateT a (Aff ( process :: PROCESS | e)) c ->
  a ->
  Eff (process :: PROCESS | e) (Canceler ( process :: PROCESS | e ))
execTests fn state = runAff (\s -> exit 1) (\s -> exit 0) (execStateT fn state)


log :: forall e. String -> Tests e Unit
log message = liftAff $ AffC.log message


assertEq :: forall a e. Show a => Eq a => a -> a -> Tests e Unit
assertEq result target = assert
  (show result <> " ≠ " <> show target)
  (show result <> " ≡ target")
  (result == target)

assert :: forall e. String -> String -> Boolean -> Tests e Unit
assert _    success true = log       $ "  ✓ - Passed - " <> success
assert fail _       false = failTest $ "  ☠ - Failed because " <> fail

failTest :: forall e. String -> Tests e Unit
failTest message = do
  log message
  put false
