module Tests.Operations.IsEmptySpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations (charsToSymbols, isEmpty)

spec :: Spec
spec =
  describe "Fwa" $ do

    it "defined in 0.txt is not empty" $
      assertFa "0.txt" (not . isEmpty)

    it "defined in 3.txt is empty" $
      assertFa "3.txt" isEmpty

