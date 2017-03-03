module Tests.Operations.IsEmptySpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations.Regular (charsToSymbols, isEmpty)

spec :: Spec
spec =
  describe "FA" $ do

    it "0.txt is not empty" $
      assertFa "0.txt" (not . isEmpty)

    it "3.txt is empty" $
      assertFa "3.txt" isEmpty

    it "oneStateFa.txt is not empty" $
      assertFa "oneStateFa.txt" (not . isEmpty)
