module Test.DateTime.Standards (standardsTest) where

import Prelude

import Data.Either (Either(..))
import Data.Formatter.DateTime as FDT
import Data.Formatter.DateTime.Standards as S
import Test.Spec (describe, Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (makeDateTime)

standardsTest ∷ Spec Unit
standardsTest = describe "Data.Formatter.DateTime.Standards" do

  let date = makeDateTime 2019 3 14 21 15 23 123
  let dateZeroMs = makeDateTime 2019 3 14 21 15 23 0

  describe "rfc3339" do
    let str = "2019-03-14T21:15:23Z"
    it "format" do
      FDT.format S.rfc3339 date `shouldEqual` str
    it "unformat" do
      FDT.unformat S.rfc3339 str `shouldEqual` Right dateZeroMs

  describe "rfc3339WithMilliseconds" do
    let str = "2019-03-14T21:15:23.123Z"
    it "format" do
      FDT.format S.rfc3339WithMilliseconds date `shouldEqual` str
    it "unformat" do
      FDT.unformat S.rfc3339WithMilliseconds str `shouldEqual` Right date
