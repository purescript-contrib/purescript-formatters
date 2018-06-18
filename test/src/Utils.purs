module Test.Utils where

import Prelude


import Test.Spec (describe, it, Spec)
import Effect.Aff (Aff)

import Data.Foldable (class Foldable, for_)
import Data.Enum (toEnum)
import Data.Maybe (fromMaybe)
import Data.DateTime (DateTime(..))
import Data.Date (canonicalDate)
import Data.Time (Time(..))


forAll ∷ ∀ a f. Foldable f ⇒ (a → String) → String → f a → (a → Aff Unit) → Spec Unit
forAll itTitle title arb f = describe title do
  for_ arb \a → it (itTitle a) (f a)

makeDateTime ∷ Int → Int → Int → Int → Int → Int → Int → DateTime
makeDateTime year month day hour minute second millisecond =
  DateTime
    (canonicalDate
      (fromMaybe bottom $ toEnum year)
      (fromMaybe bottom $ toEnum month)
      (fromMaybe bottom $ toEnum day))
    (Time
       (fromMaybe bottom $ toEnum hour )
       (fromMaybe bottom $ toEnum minute )
       (fromMaybe bottom $ toEnum second )
       (fromMaybe bottom $ toEnum millisecond))
