module Test.Utils where

import Prelude

import Effect.Aff (Aff)

import Control.Monad.Reader.Class (class MonadReader, ask, local)
import Data.Foldable (class Foldable, for_)
import Data.Enum (toEnum)
import Data.Maybe (fromMaybe)
import Data.Monoid (power, guard)
import Data.DateTime (DateTime(..))
import Data.Date (canonicalDate)
import Data.Time (Time(..))
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Test.Assert (assertEqual)

-----------------------------------------------------------------

-- Provide similar API to purescript-spec to reduce code changes

describe :: forall m. MonadReader Int m => MonadAff m => String -> m Unit -> m Unit
describe msg runTest = do
  indentation <- ask
  let spacing = guard (indentation > 0) " "
  liftEffect $ log $ (power ">>" indentation) <> spacing <> msg
  local (_ + 1) runTest

it :: forall m. MonadReader Int m => MonadAff m => String -> m Unit -> m Unit
it = describe

shouldEqual :: forall m a. MonadAff m => Eq a => Show a => a -> a -> m Unit
shouldEqual actual expected =
  liftEffect $ assertEqual { actual, expected }

-----------------------------------------------------------------

forAll :: forall m a f. MonadReader Int m => MonadAff m => Foldable f => (a -> String) -> String -> f a -> (a -> Aff Unit) -> m Unit
forAll itTitle title arb f = describe title do
  for_ arb \a -> it (itTitle a) (liftAff $ f a)

makeDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> DateTime
makeDateTime year month day hour minute second millisecond =
  DateTime
    ( canonicalDate
        (fromMaybe bottom $ toEnum year)
        (fromMaybe bottom $ toEnum month)
        (fromMaybe bottom $ toEnum day)
    )
    ( Time
        (fromMaybe bottom $ toEnum hour)
        (fromMaybe bottom $ toEnum minute)
        (fromMaybe bottom $ toEnum second)
        (fromMaybe bottom $ toEnum millisecond)
    )
