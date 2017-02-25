module Tests.Operations.IsUniversalSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations (charsToSymbols, isUniversal)

spec :: Spec
spec =
  describe "FA" $ do

    it "oneStateFa.txt is universal" $
      assertFa "oneStateFa.txt" isUniversal

    it "0.txt is not universal" $
      assertFa "0.txt" (not . isUniversal)

